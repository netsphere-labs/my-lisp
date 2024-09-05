
// Reader

// Lisp は構文を追加できるので、bison/flex で固定的に parse するわけにいかない
// 全部手書きする

// "1+" "1-" というシンボルがある。いったん token を切り出して、
// 全体にマッチするか確認して, 数値を生成しないといけない


#include "s_expr.h"
#include <cctype>
#include <iomanip>
#include <memory>
#include <sstream>
#include <functional> // std::function
#include <fstream>
#include <unicode/ustream.h>
#include <unicode/unistr.h>
using namespace icu;

namespace my {

// Standard Macro Characters が決まっている。いかなる文字でも追加できる!
enum token_type {
    END_OF_FILE = 0,
    right_paren,

    value,
    reader_error,  // "..", "...", "# " and "#)"
};

token_type read_from_string(std::istream& stream, value_t* value_r);


////////////////////////
// 再帰で、各 parse 関数を呼び出していく

// トークン(文字列) が数値にマッチするか
static bool match_number(const UnicodeString& str, double* num_r)
{
    // `std::stod()` は `strtod()` の wrap. `strtod()` のほうを使う
    std::string u8;
    const char* s = str.toUTF8String(u8).c_str();
    const char* endptr = s;
    *num_r = strtod(s, (char**) &endptr);
    if ( errno == ERANGE ) {
        // CL: "1e1000000000000" => FLOATING-POINT-OVERFLOW error
        throw std::invalid_argument("overflow or underflow");
    }

    if (endptr != s && *endptr == '\0')
        return true;
    else
        return false;
}


// 文字列
// CL: 制御文字の埋め込みはサポートされない。ホントに?
static value_t parse_string(std::istream& stream, char dmy)
{
    std::string v = "";

    int ch;
    while (ch = stream.get(), ch != EOF) {
        if (ch == '"')
            return std::make_shared<my::string>( UnicodeString::fromUTF8(v) );
        else if (ch == '\\' ) {  // CL: 後続が何であれ, バックスラッシュが単に取れるだけ
            char ch2 = stream.get();
            if (ch2 == EOF) throw std::invalid_argument("unexpected EOF");
            v += ch2;
        }
        else
            v += ch; // 改行文字も含めれる
    }

    throw std::invalid_argument("unexpected EOF");
}


// (1 . (2))  => (1 2)
// (1 2 3 . 4) は OK だが, (1 2 . 4 5) はエラー
static value_t parse_list(std::istream& stream, char dmy)
{
    std::shared_ptr<cons> lst = std::make_shared<cons>();

    int dot_cons = 0;
    for (int i = 0; ; ++i) {
        value_t val;
        token_type tok = read_from_string(stream, &val);
        std::shared_ptr<symbol> sym;
        switch (tok) {
        case token_type::END_OF_FILE:
            throw std::invalid_argument("unexpected EOF");
        case token_type::right_paren:
            if (lst->empty())
                return nilValue;
            else {
                if (dot_cons == 1)
                    throw std::invalid_argument("Nothing after `.` in list");
                return lst;
            }
            break;
        case token_type::value:
            sym = OBJECT_CAST<symbol>(val);
            if (sym && sym->name() == ".") {
                if (dot_cons == 0) {
                    if (i == 0)
                        throw std::invalid_argument("Nothing before `.` in list");
                    lst->append(val); // "."
                    dot_cons = 1;
                }
                else {
                    throw std::invalid_argument("dot context error");
                }
            }
            else {
                switch (dot_cons) {
                case 0:
                    lst->append(val); break;
                case 1:
                    {
                        ListPtr vallst = OBJECT_CAST<list>(val);
                        if (vallst != nullptr ) { // (1 . nil) => (1)
                            lst->pop_back(); lst->append_range(vallst);
                        }
                        else
                            lst->append(val);
                        dot_cons = 2;
                    }
                    break;
                case 2:
                    throw std::invalid_argument("More than one object follows `.` in list");
                }
            }
            break;
        default:
            abort(); // internal error
        }
    }

    throw std::runtime_error("syntax error: unclosed list");
}

static value_t parse_quote(std::istream& stream, char dmy)
{
    std::shared_ptr<cons> lst = std::make_shared<cons>();
    lst->append(std::make_shared<my::symbol>("QUOTE"));

    value_t val;
    token_type tok = read_from_string(stream, &val);
    switch (tok) {
    case token_type::END_OF_FILE:
        throw std::invalid_argument("unexpected EOF");
    case token_type::value:
        lst->append(val);
        break;
    default:
        throw std::invalid_argument("syntax error");
    }

    return lst;
}


// TODO: 数チェック
static value_t parse_quasiquote(std::istream& stream, char dmy)
{
    std::shared_ptr<cons> lst = std::make_shared<cons>();
    lst->append(std::make_shared<my::symbol>("QUASIQUOTE"));

    value_t val;
    token_type tok = read_from_string(stream, &val);
    switch (tok) {
    case token_type::END_OF_FILE:
        throw std::invalid_argument("unexpected EOF");
    case token_type::value:
        lst->append(val);
        break;
    default:
        throw std::invalid_argument("syntax error");
    }

    return lst;
}

static value_t parse_unquote(std::istream& stream, char dmy)
{
    std::shared_ptr<cons> lst = std::make_shared<cons>();

    int ch = stream.peek();
    if (ch == '@') { // ,@
        ch = stream.get();
        lst->append(std::make_shared<my::symbol>("UNQUOTE-SPLICING"));
    }
    else
        lst->append(std::make_shared<my::symbol>("UNQUOTE"));

    value_t val;
    token_type tok = read_from_string(stream, &val);
    switch (tok) {
    case token_type::END_OF_FILE:
        throw std::invalid_argument("unexpected EOF");
    case token_type::value:
        lst->append(val);
        break;
    default:
        throw std::invalid_argument("syntax error");
    }

    return lst;
}

// 2文字目で定義されているもの:
// https://www.lispworks.com/documentation/HyperSpec/Body/02_dh.htm
static value_t parse_sharp_macro(std::istream& stream, char dmy)
{
    return nilValue;  // TODO: impl.
}

// シンボル, 数値
static value_t parse_token(std::istream& stream)
{
    // terminating macro char 以外は全部使える. 漢字などや '#' も
    static const std::string term = "\"'(),;`";

    std::string sym = "";

    int ch;
    while (ch = stream.get(), ch != EOF) {
        if (ch == '\\') {
            // 小文字のシンボルをつくれる!
            char ch2 = stream.get();
            if (ch2 == EOF) throw std::invalid_argument("unexpected EOF");
            sym += ch2;
        }
        else if ( (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') )
            sym += toupper(ch);
        else if (isspace(ch) || term.find(ch) != std::string::npos) {
            stream.unget();
            break;
        }
        else
            sym += ch;
    }

    // 定数か
    if (sym == "NIL")
        return nilValue;
    else if (sym == "T")
        return true;

    UnicodeString u_sym = UnicodeString::fromUTF8(sym);

    if (isdigit(sym[0]) || sym[0] == '+' || sym[0] == '-' || sym[0] == '.') {
        // 数値かどうか再確認
        double num = 0;
        if ( match_number(u_sym, &num) )
            return num;
    }

    // 残り: シンボル
    return std::make_shared<my::symbol>(u_sym);
}


// マクロ文字に出会うと, reader macro function を呼び出す.
// https://www.lispworks.com/documentation/HyperSpec/Body/02_d.htm
struct ReaderMacro {
    const char macro_char;
    std::function<my::value_t(std::istream&, char)> func;
};

static const ReaderMacro macroTable[] = {
    {'(', parse_list},
    //')'
    {'\'', parse_quote},  // '(a b c)  => (quote (a b c))
    //{';', parse_comment},
    {'"', parse_string},
    {'`',  parse_quasiquote},    // `(a b c)  => (quasiquote (a b c))
    {',',  parse_unquote},
    //{",@", "unquote-splicing"},
    {'#', parse_sharp_macro}, // 二文字目を見る。#! など定義されていない場合は no dispatch function error
};


// 値を一つだけ読む
token_type read_from_string(std::istream& stream, value_t* value_r)
{
    int ch = stream.get();

    while (true ) {  // skip whitespace
        if (isspace(ch))
            ch = stream.get();
        else if (ch == ';') {
            while (ch = stream.get(), ch != '\r' && ch != '\n') ;
        }
        else
            break;
    }

    if (ch == EOF)
        return token_type::END_OF_FILE;

    // error checks
    if (ch == '.') {
        if ( stream.peek() == '.' )
            throw std::invalid_argument("too many dots");
    }
    else if (ch == '#') {
        int ch2 = stream.peek();
        if ( ch2 == EOF ) throw std::invalid_argument("unexpected EOF");
        if ( ch2 == ')' || isspace(ch2) )
            throw std::invalid_argument("illegal sharp macro character");
    }

    // 平場での、特別扱いの文字
    for (const auto& macro: macroTable) {
        if (ch == macro.macro_char) {
            *value_r = macro.func(stream, ch);
            return token_type::value;
        }
    }

    if (ch == ')')
        return token_type::right_paren;

    stream.unget();
    *value_r = parse_token(stream);
    return token_type::value;
}


value_t READ(std::istream& stream)
{
    value_t val;
    token_type tok = read_from_string(stream, &val);
    switch (tok) {
    case token_type::value:
        return val;
    default:
        throw std::invalid_argument("something wrong");
    }
}


bool value_isa(const value_t& val, const icu::UnicodeString& klass)
{
    // TODO: ちゃんと作る
    if (klass == "LIST") {
        return OBJECT_CAST<list>(val) != nullptr;
    }
    else if (klass == "SYMBOL") {
        return OBJECT_CAST<symbol>(val) != nullptr;
    }
    else {
        abort(); // internal error
    }
}


} // namespace my
