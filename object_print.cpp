
#include "s_expr.h"
#include <iomanip> // std::quoted
#include <unicode/unistr.h>
#include <unicode/ustream.h>
using namespace icu;

namespace my {

// TODO: 定数リストに格納
const std::shared_ptr<null> nilValue = std::make_shared<null>();
const std::shared_ptr<object> trueValue = std::make_shared<object>();


static void indent(std::ostream& out, int level) {
    for (int i = 0; i < level; ++i)
        out << "   ";
}

////////////////////////////////////////////////////////
// class object

void object::write_indented(std::ostream& out, int level) const {
    indent(out, level);
    UnicodeString s = print();
    out << s;
}

UnicodeString object::print() const {
    return "T";
}

////////////////////////////////////////////////////////
// class symbol

// std::quoted は, 出力時は,「"」とエスケープ文字だけをエスケープする。
// CL: 改行はそのまま表示する
static UnicodeString quote_str(const UnicodeString& str,
                               const UnicodeString& escapes) {
    UnicodeString ret = "";
    for (int i = 0; i < str.length(); i = str.moveIndex32(i, +1)) {
        UChar32 ch = str.char32At(i);
        if (escapes.indexOf(ch) != -1)
            ret += UnicodeString("\\") + ch;
        else
            ret += ch;
    }

    return ret;
}

// - macro characters, whitespace, 英小文字が含まれている場合 "|" で囲む
//   例  ";" => "|;|"
//   See http://clhs.lisp.se/Body/02_ade.htm
// - 「\」は quote する。「"」は quote しない.
UnicodeString symbol::print() const {
    static const std::string macro_chars = "\"'(),;`#";

    bool need_escape = false;
    for (int i = 0; i < string_.length(); i = string_.moveIndex32(i, +1)) {
        UChar32 ch = string_.char32At(i);
        if ( isspace(ch) || ch >= 'a' && ch <= 'z' ||
             macro_chars.find(ch) != std::string::npos) {
            need_escape = true; break;
        }
    }

    if (need_escape)
        return UnicodeString("|") + quote_str(string_, "\\") + "|";
    else
        return quote_str(string_, "\\");
}

////////////////////////////////////////////////////////
// class string

UnicodeString string::print() const {
    return UnicodeString("\"") +
           quote_str(string_, UnicodeString::fromUTF8("\\\"")) + "\"";
}


////////////////////////////////////////////////////////
// class cons

UnicodeString cons::print() const
{
    if (list_.empty())
        throw std::out_of_range("empty"); // out_of_range は logic_error

    PrintVisitor v = PrintVisitor();
    UnicodeString ret = "(";
    auto i = list_.begin();
    ret += std::visit(v, *i);
    while (++i != list_.end())
        ret += " " + std::visit(v, *i);
    ret += ")";

    return ret;
}


////////////////////////////////////////////////////////
// class vector

// 空のときは #()
UnicodeString vector::print() const
{
    PrintVisitor v = PrintVisitor();
    UnicodeString ret = UnicodeString("#(") ;
    auto i = vector_.begin();
    if (i != vector_.end()) {
        ret += std::visit(v, *i);
        while (++i != vector_.end())
            ret += " " + std::visit(v, *i);
    }
    ret += ")";

    return ret;
}


struct WriteVisitor {
    WriteVisitor(std::ostream& out, int indent): out_(out), indent_(indent) { }

    //void operator()(std::monostate) { nilValue->write_indented(out_, indent_); }
    void operator()(bool) { trueValue->write_indented(out_, indent_); }
    void operator()(int64_t v) { fixnum(v).write_indented(out_, indent_); }
    void operator()(double v) { double_float(v).write_indented(out_, indent_); }
    void operator()(const std::shared_ptr<object>& v) {
        v->write_indented(out_, indent_); }

private:
    std::ostream& out_ ;
    int indent_;
};


void cons::write_indented(std::ostream& out, int level) const
{
    if (list_.empty())
        throw std::out_of_range("empty");

    WriteVisitor v = WriteVisitor(out, level + 1);
    PrintVisitor p = PrintVisitor();

    indent(out, level); out << "(";
    for (auto i = list_.begin(); i != list_.end(); ++i) {
        if (i == list_.begin())
            out << std::visit(p, *i);
        else
            std::visit(v, *i);
        out << '\n';
    }

    indent(out, level); out << ")";
}


void PRINT(const value_t& value, std::ostream& out)
{
    WriteVisitor v = WriteVisitor(out, 0);
    std::visit(v, value);
}

} // namespace my
