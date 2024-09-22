
// 評価する EVAL
// Lisp は, 入力として S式を得て, 評価し, S式を出力する

#include "s_expr.h"
#include "environment.h"
#include <iostream>
using namespace icu;

namespace my {

value_t EVAL1(value_t ast, EnvPtr env);

value_t eval_atom(const value_t& atom, EnvPtr env)
{
    std::shared_ptr<symbol> sym = OBJECT_CAST<symbol>(atom);
    if (sym != nullptr) {
        return env->find_value(sym->name());
    }

    // そのまま返す
    return atom;
}


// @return `nil` 以外は全部 true
bool value_isTrue(const value_t& value) {
    if ( std::holds_alternative<ObjectPtr>(value) ) {
        std::shared_ptr<null> p = std::dynamic_pointer_cast<null>(std::get<ObjectPtr>(value));
        return p != nullptr ? false : true;
    }
    return true;
}

// 相互末尾再帰にも対応する
// トランポリンについては、例えば
// https://stackoverflow.com/questions/25228871/how-to-understand-trampoline-in-javascript
struct Trampoline {
    enum Type {
        DONE,
        MORE
    } type;

    // 値 = AST なので、共用. `MORE` の場合 AST の意味。
    value_t const value;

    // MORE: 次の関数の環境. ここが TCO になる
    EnvPtr const innerEnv;

    Trampoline(const value_t& val): type(DONE), value(val) { }

    Trampoline(Type t, const value_t& ast, EnvPtr inner):
        type(t), value(ast), innerEnv(inner) { }
};

static Trampoline eval1_tco(value_t ast, EnvPtr env);

static Trampoline eval_implicit_progn(ListPtr list, EnvPtr env)
{
    int i;
    for (i = 0; i < list->length() - 1; ++i)
        EVAL1(list->at(i), env);

    return eval1_tco(list->at(i), env); // TCO
}


// ビルトイン関数の実行
// @param evaled_args 評価された後の実引数のリスト
value_t function::apply(ListPtr evaled_args)
{
    EnvPtr inner = bind_arguments(evaled_args);
    if (is_builtin())
        return m_handler(inner);

    // eval1_tco() 内では, 以下の部分は外出しにする
    Trampoline t = eval_implicit_progn(m_body, inner);
    if (t.type == Trampoline::MORE)
        return EVAL1(t.value, t.innerEnv);
    else
        return t.value;
}


/////////////////////////////////////////////////////////////////////////
// Special Forms

/** 変数に代入
 setq {pair}* => result
;; Use of SETQ to update values by sequential assignment.
 (setq a (1+ b) b (1+ a) c (+ a b)) =>  7
 a =>  3
 b =>  4
 c =>  7
*/
static Trampoline do_setq(std::shared_ptr<cons> form, EnvPtr env)
{
    if ( form->length() == 1)
        return Trampoline(nilValue);
    if ( (form->length() % 2) == 0 )
        throw std::runtime_error("odd number of args to SETQ");

    value_t ret;
    for (int i = 1; i < form->length(); i += 2) {
        std::shared_ptr<symbol> id = VALUE_CAST_CHECKED(symbol, form->at(i));
        // 定数へ代入しようとしてエラーがありうる
        ret = EVAL1(form->at(i + 1), env);
        env->set_value(id->name(), ret, false);
    }

    return ret;
}


/**
CL: (defun) はマクロ. 手抜きで, special op にする
defun function-name lambda-list [[declaration* | documentation]] form*
*/
static Trampoline do_defun(std::shared_ptr<cons> form, EnvPtr env)
{
    std::shared_ptr<symbol> name = VALUE_CAST_CHECKED(symbol, form->at(1));
    ListPtr params = VALUE_CAST_CHECKED(class list, form->at(2));

    // 暗黙の `block` が入る. lambda 式のほうには入らない
    std::shared_ptr<cons> block = std::make_shared<cons>();
    block->append(std::make_shared<symbol>("BLOCK"))
        .append(std::make_shared<symbol>(name->name()))
        .append_range(form->sub(3));   // an implicit progn.
    std::shared_ptr<cons> body = std::make_shared<cons>();
    body->append(block);

    FuncPtr func_ptr = std::make_shared<function>(
                                        name->name(), params, body, nullptr);
    globalEnv->set_function(name->name(), func_ptr);

    return Trampoline(nilValue);
}

/**
CL: DO, DO* はマクロ。手抜きで, special op にする
do ({var | (var [init-form [step-form]])}*) (end-test-form result-form*) declaration* {tag | statement}*
*/
static Trampoline do_do(std::shared_ptr<cons> form, EnvPtr env)
{
    /*
                for (int i = 1; i < argCount; i++) {
                    EVAL(list->item(i), env);
                }
                ast = list->item(argCount);
                continue; // TCO
    */
    return Trampoline(nilValue);
}


/** then-form, else-form は 1文だけ.
if test-form then-form [else-form] => result*
(if test-form then-form else-form)
 ==  (cond (test-form then-form) (t else-form))
*/
static Trampoline do_if(std::shared_ptr<cons> form, EnvPtr env)
{
    if ( !(form->length() >= 3 && form->length() <= 4) )
        throw std::runtime_error("args error");

    bool isTrue = value_isTrue(EVAL1(form->at(1), env));
    if (!isTrue && form->length() == 3)
        return Trampoline(nilValue);

    return eval1_tco(form->at(isTrue ? 2 : 3), env); // TCO
}

// 順に評価
static Trampoline do_progn(std::shared_ptr<cons> form, EnvPtr env)
{
    if (form->length() == 1)
        return Trampoline(nilValue);

    return eval_implicit_progn(form->sub(1), env);
}

static Trampoline do_quote(std::shared_ptr<cons> form, EnvPtr env)
{
    if (form->length() != 2)
        throw std::runtime_error("wrong number of args to QUOTE");

    return form->at(1);
}


/** Special Operator LET, LET*
新しいスコープを導入する
  (let ((if 10)) (+ if 10))  => これは通る! 逆にこれがマクロを難しくする
次は通らない:
* (defconstant hoge 10)
HOGE
* (let ((hoge 5)) (+ hoge 30))    #=> COMMON-LISP-USER::HOGE names a defined constant, and cannot be used in LET.
なので, 慣習として, 定数名は "+" で囲む
*/
static Trampoline do_let_star(std::shared_ptr<cons> form, EnvPtr env)
{
    std::shared_ptr<symbol> op = OBJECT_CAST<symbol>(form->at(0));
    bool is_star = op->name() == "LET*";

    // (let () (+ 2 3))   0個も可!
    ListPtr bindings = VALUE_CAST_CHECKED(class list, form->at(1));
    EnvPtr inner = std::make_shared<Environment>(env);
    for ( const auto& var : *bindings ) {
        std::shared_ptr<symbol> sym;
        value_t val;
        std::shared_ptr<cons> pair = OBJECT_CAST<cons>(var);
        if (pair != nullptr) {
            // 値あり
            if (pair->length() != 2)
                throw std::runtime_error("The LET binding spec is malformed.");
            sym = VALUE_CAST_CHECKED(symbol, pair->at(0));
            val = EVAL1(pair->at(1), is_star ? inner : env);
        }
        else { // 変数名のみ
            sym = VALUE_CAST_CHECKED(symbol, var);
            val = nilValue;
        }
        // 定数の場合はここでエラー
        inner->set_value(sym->name(), val, false);
    }

    Trampoline ret = eval_implicit_progn(form->sub(2), inner); // TCO
    std::cout << "LET result = " ; PRINT(ret.value, std::cout); // DEBUG
    return ret;
}

// val がリストで，かつ op シンボルか
// @return 違った場合 nullptr
std::shared_ptr<cons> starts_with(const value_t& val,
                                  const icu::UnicodeString& op)
{
    std::shared_ptr<cons> form = OBJECT_CAST<cons>(val);
    if (form == nullptr)
        return nullptr;

    std::shared_ptr<symbol> sym = OBJECT_CAST<symbol>(form->at(0));
    if ( sym == nullptr || sym->name() != op )
        return nullptr;

    return form;
}


/* lambda form と (function ...) と共用
lambda form だけ特別扱いされる
     ((lambda lambda-list . body) . arguments)
   is semantically equivalent to the function form
     (funcall #'(lambda lambda-list . body) . arguments)
*/
static FuncPtr get_function(const value_t& func_name, EnvPtr env)
{
    std::cout << __func__ << ": "; PRINT(func_name, std::cout); std::cout << "\n"; // DEBUG

    std::shared_ptr<symbol> sym = OBJECT_CAST<symbol>(func_name);
    if (sym) {
        // 1. (funcall #'+ 1 2 3) =>  6
        // 環境から関数を探して返す
        FuncPtr func = env->find_function(sym->name());
        if (!func)
            throw std::runtime_error("function not found");
        return func;
    }
    else {
        // lambda expression
        std::shared_ptr<cons> lambda_expr = starts_with(func_name, "LAMBDA");
        if (lambda_expr == nullptr)
            throw std::runtime_error("not symbol nor lambda expression");
        if (lambda_expr->length() < 2)
            throw std::runtime_error("args needed");

        // クロージャを作って返す
        FuncPtr func = std::make_shared<function>("<lambda>",
                                        OBJECT_CAST<list>(lambda_expr->at(1)),
                                        lambda_expr->sub(2),
                                        env);
        return func;
    }
}

// function name => function
//    name: function name
//     or lambda expression
static Trampoline do_function(std::shared_ptr<cons> form, EnvPtr env)
{
    if (form->length() != 2)
        throw std::runtime_error("wrong number of args to FUNCTION");

    return value_t(get_function(form->at(1), env));
}


static value_t do_quasiquote_sub(ListPtr tmpl, EnvPtr env)
{
    // `,x の形   tmpl = (unquote x)
    std::shared_ptr<cons> unq = starts_with(tmpl->at(0), "UNQUOTE"); // ","
    if (unq != nullptr) {
        // `,1       => 1
        // `,x       => 変数 X を評価
        // `,(+ 2 3) => 5  リストを評価
        return EVAL1(tmpl->at(1), env);
    }
    else {
        // `,@s はエラー: `,@S is not a well-formed backquote expression
        unq = starts_with(tmpl->at(0), "UNQUOTE-SPLICING"); // ",@"
        if (unq != nullptr)
            throw std::runtime_error("not a well-formed backquote expression");
    }

    // `(1 2)     => (1 2)
    // `(1 ,x 3)  => x を評価して埋め込む
    // `(1 ,@s 5) => s は LIST でなければならない。展開して埋め込む
    std::shared_ptr<cons> ret = std::make_shared<class cons>();

    for (const auto& v : *tmpl) {
        std::shared_ptr<cons> sub = OBJECT_CAST<cons>(v);
        if (sub != nullptr) {
            std::shared_ptr<symbol> op = OBJECT_CAST<symbol>(sub->at(0));
            if (op != nullptr && op->name() == "UNQUOTE")  // ","
                ret->append(EVAL1(sub->at(1), env));
            else if (op != nullptr && op->name() == "UNQUOTE-SPLICING") {// ",@"
                ListPtr lst = VALUE_CAST_CHECKED(class list, EVAL1(sub->at(1), env));
                if ( !lst->empty()) // NIL のときは要素削除
                    ret->append_range(lst);
            }
            else {
                value_t r = do_quasiquote_sub(sub, env);
                ret->append(r);
            }
        }
        else
            ret->append(v); // 評価しない
    }

    // `()  => NIL
    if (ret->empty())
        return nilValue;
    else
        return ret;
}

// マクロの外側でも使える
Trampoline do_quasiquote(std::shared_ptr<cons> form, EnvPtr env)
{
    ListPtr tmpl = OBJECT_CAST<class list>(form->at(1));
    if (!tmpl || tmpl->empty() )
        return form->at(1); // シンボルもそのまま返せばよい

    value_t ret = do_quasiquote_sub(OBJECT_CAST<list>(tmpl), env);
    return ret;
}

struct StopIteration : public std::exception
{
    StopIteration(const UnicodeString& name, const value_t val):
        tag(name), ret(val) { }

    UnicodeString tag;
    value_t ret;
};

// a structured, lexical, non-local exit facility.
Trampoline do_block(std::shared_ptr<cons> form, EnvPtr env)
{
    // 新しいスコープを導入するわけではない。
    //EnvPtr inner = std::make_shared<Environment>(env);

    std::shared_ptr<symbol> id = VALUE_CAST_CHECKED(symbol, form->at(1));
    env->push_block_tag(id->name());

    try {
        Trampoline t = eval_implicit_progn(form->sub(2), env);
        env->pop_block_tag(id->name());
        return t;
    } catch (StopIteration& ex) {
        env->pop_block_tag(id->name()); // only exited once.
        if (ex.tag != id->name() )
            throw;
        return ex.ret;
    }
}

Trampoline do_return_from(std::shared_ptr<cons> form, EnvPtr env)
{
    if (form->length() < 2 || form->length() > 3)
        throw std::runtime_error("args number error");

    std::shared_ptr<symbol> id = VALUE_CAST_CHECKED(symbol, form->at(1));
    throw StopIteration(id->name(),
                        form->length() == 2 ? nilValue : form->at(2) );
}


// Special operator の一覧は仕様で決まっている。後から追加できない。
//  -- <a href="https://www.lispworks.com/documentation/HyperSpec/Body/03_ababa.htm">3.1.2.1.2.1 Special Forms</a>
struct SpecialForm {
    icu::UnicodeString name;
    std::function<Trampoline(std::shared_ptr<cons>, EnvPtr )> func;
};

static const SpecialForm specialForms[] = {
    // evaluation and compilation section

    //    {"eval-when", },
    //    {"load-time-value", },
    {"QUOTE", do_quote},
    //    {"symbol-macrolet", },
    //    {"locally", },
    //    {"the", },

    // The data and control flow section

//    {"flet", do_flet},   // define local functions
//    {"labels", },
//    {"macrolet", do_macrolet},  // define local macros
    {"FUNCTION", do_function},
    {"LET", do_let_star},   // `let` performs the bindings in parallel
    {"LET*", do_let_star},  // `let*` does them sequentially
    //    {"progv", },
    {"SETQ", do_setq},
    {"BLOCK", do_block },
    {"RETURN-FROM", do_return_from },
    //{"tagbody", },
    //{"catch", },
    //{"throw", },
//    {"unwind-protect", do_unwind_protect },
    //{"go", },
    {"IF", do_if},
    //{"multiple-value-call", },
    //{"multiple-value-prog1", },
    {"PROGN", do_progn},

    // とりあえず special operator として追加する:
    {"QUASIQUOTE", do_quasiquote},
    {"DEFUN", do_defun},
    {"DO", do_do},
    {"DO*", do_do},
};


static ListPtr eval_args(ListPtr args, EnvPtr env)
{
    std::cout << __func__ << ": "; PRINT(args, std::cout); std::cout << "\n"; // DEBUG

    if (args->length() == 0)
        return nilValue;

    std::shared_ptr<cons> ret = std::make_shared<cons>();
    for (auto it = args->begin(); it != args->end(); ++it)
        ret->append(EVAL1(*it, env));

    return ret;
}


// implicit progn が複数の要素だったとき, `PROGN` で括る
static value_t make_progn(ListPtr list)
{
    if (list->length() <= 1)
        return list->at(0);

    std::shared_ptr<cons> progn = std::make_shared<cons>();
    progn->append(std::make_shared<symbol>("PROGN"));
    progn->append_range(list);
    return progn;
}


#ifndef DISABLE_MACRO
extern value_t macroExpand(EnvPtr args);
#endif

static Trampoline eval1_tco(value_t ast, EnvPtr env)
{
    ListPtr list = OBJECT_CAST<class list>(ast);
    if (!list || list->empty() )
        return eval_atom(ast, env);

#ifndef DISABLE_MACRO
    // CL: special form と同名の関数は禁止.
    EnvPtr exp_env = std::make_shared<Environment>();
    exp_env->set_value("FORM", ast, false);
    ast = macroExpand(exp_env);  // 実引数を評価せずに渡す
    exp_env.reset();
#endif

    list = OBJECT_CAST<class list>(ast);
    if ( !list || list->empty() )
        return eval_atom(ast, env);

    // From here on down we are evaluating a non-empty list.
    //std::shared_ptr<cons> args = std::dynamic_pointer_cast<cons>(list);
    ListPtr evaled;
    FuncPtr func;

    // First handle the special forms.
    std::shared_ptr<symbol> sym = OBJECT_CAST<symbol>(list->car());
    if (sym != nullptr) {
        icu::UnicodeString special = sym->name();
        for ( const auto& op : specialForms ) {
            if (op.name == special)
                return op.func(std::dynamic_pointer_cast<cons>(list), env);
        }
    }

/* どのメソッドを呼び出すか、実引数を評価した後に決める
     1. compute the list of applicable methods
     2. if no method is applicable then signal an error
     3. sort the applicable methods in order of specificity
     4. invoke the most specific method.
*/

    // Now we're left with the case of a regular list to be evaluated.
    // だいぶ手抜きでいく
    evaled = eval_args(list->sub(1), env);
    //std::cout << "args = " ; PRINT(list->sub(1), std::cout); // DEBUG
    std::cout << "evaled args = " ; PRINT(evaled, std::cout); // DEBUG

    // lambda form だけ特別扱いされる.
    func = get_function(list->at(0), env);
    // ここではもう, 元の env は不要
    if ( func->is_builtin() ) {
        env = nullptr;
        return func->apply(evaled);
    }
    else {
        // ここが TCO になる
        return Trampoline(Trampoline::MORE,
                          make_progn(func->getBody()),
                          func->bind_arguments(evaled) ); // innerEnv
    }
}


value_t EVAL1(value_t ast, EnvPtr env)
{
    while (true) {
        //std::cout << "EVAL1() loop: "; PRINT(ast, std::cout); std::cout << "\n"; // DEBUG

        Trampoline t = eval1_tco(ast, env);
        if (t.type == Trampoline::DONE)
            return t.value;
        else { // type == Trampoline::MORE
            ast = t.value; // TCO
            env = t.innerEnv;
        }
    }
}

} // namespace my
