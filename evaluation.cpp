
// 評価する EVAL
// Lisp は, 入力として S式を得て, 評価し, S式を出力する

#include "s_expr.h"
#include "environment.h"
#include <iostream>

namespace my {

value_t EVAL(value_t ast, EnvPtr env);

static value_t eval_atom(const value_t& atom, EnvPtr env)
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


/////////////////////////////////////////////////////////////////////////
// Special Forms

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
    EnvPtr const innerEnv;

    Trampoline(const value_t& val): type(DONE), value(val) { }
    Trampoline(Type t, const value_t& ast): type(t), value(ast) { }
    Trampoline(Type t, const value_t& ast, EnvPtr inner):
        type(t), value(ast), innerEnv(inner) { }
};


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
        ret = EVAL(form->at(i + 1), env);
        env->set_value(id->name(), ret, false);
    }

    return ret;
}


/*
(defun) はマクロで、次のように展開される
* (macroexpand '(defun dbl (n) (* 2 n)))
(PROGN
 (EVAL-WHEN (:COMPILE-TOPLEVEL) (SB-C:%COMPILER-DEFUN 'DBL T NIL NIL))
 (SB-IMPL::%DEFUN 'DBL
                  (SB-INT:NAMED-LAMBDA DBL
                      (N)
                    (BLOCK DBL (* 2 N)))))
T
*/
/*
static ret_t do_flet()
{
            if (special == "fn*") {
                checkArgsIs("fn*", 2, argCount);

                const malSequence* bindings =
                    VALUE_CAST(malSequence, list->item(1));
                StringVec params;
                for (int i = 0; i < bindings->count(); i++) {
                    const malSymbol* sym =
                        VALUE_CAST(malSymbol, bindings->item(i));
                    params.push_back(sym->value());
                }

                return mal::lambda(params, list->item(2), env);
            }
}
*/


/** then-form, else-form は 1文だけ.
if test-form then-form [else-form] => result*
(if test-form then-form else-form)
 ==  (cond (test-form then-form) (t else-form))
*/
static Trampoline do_if(std::shared_ptr<cons> form, EnvPtr env)
{
    if ( !(form->length() >= 3 && form->length() <= 4) )
        throw std::runtime_error("args error");

    bool isTrue = value_isTrue(EVAL(form->at(1), env));
    if (!isTrue && form->length() == 3)
        return Trampoline(nilValue);

    return Trampoline(Trampoline::MORE, form->at(isTrue ? 2 : 3)); // TCO
}

// 順に評価
static Trampoline do_progn(std::shared_ptr<cons> form, EnvPtr env)
{
    if (form->length() == 1)
        return Trampoline(nilValue);

    int i;
    for (i = 1; i < form->length() - 1; ++i)
        EVAL(form->at(i), env);

    return Trampoline(Trampoline::MORE, form->at(i)); // TCO
}

static Trampoline do_quote(std::shared_ptr<cons> form, EnvPtr env)
{
    if (form->length() != 2)
        throw std::runtime_error("wrong number of args to QUOTE");

    return form->at(1);
}

static ListPtr make_progn(ListPtr list)
{
    std::shared_ptr<cons> progn = std::make_shared<cons>();
    progn->append(std::make_shared<symbol>("PROGN"));
    progn->append_range(list);
    return progn;
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
            val = EVAL(pair->at(1), is_star ? inner : env);
        }
        else { // 変数名のみ
            sym = VALUE_CAST_CHECKED(symbol, var);
            val = nilValue;
        }
        // 定数の場合はここでエラー
        inner->set_value(sym->name(), val, false);
    }

    // an implicit progn.
    return Trampoline(Trampoline::MORE, make_progn(form->sub(2)), inner); // TCO
}

extern void PRINT(const value_t& value, std::ostream& out);

// lambda form と (function ...) と共用
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
        std::shared_ptr<cons> lambda_expr = OBJECT_CAST<cons>(func_name);
        if (lambda_expr == nullptr)
            throw std::runtime_error("not symbol nor cons");
        if (lambda_expr->length() < 2)
            throw std::runtime_error("args needed");

        sym = VALUE_CAST_CHECKED(symbol, lambda_expr->at(0));
        if ( sym == nullptr || sym->name() != "LAMBDA" )
            throw std::runtime_error("not lambda");

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
    //{"block", },
    //{"return-from", },
    //{"tagbody", },
    //{"catch", },
    //{"throw", },
//    {"unwind-protect", do_unwind_protect },
    //{"go", },
    {"IF", do_if},
    //{"multiple-value-call", },
    //{"multiple-value-prog1", },
    {"PROGN", do_progn},
};


/**
 * repeatedly expands form until it is no longer a macro form.
 * @return 展開した AST を返す
 */
value_t macroExpand(const value_t& ast, EnvPtr env) {
    // TODO: impl. とりあえずそのまま返す
    return ast;
}


/** 関数の実行
1. lambda form だけ特別扱いされる
     ((lambda lambda-list . body) . arguments)
   is semantically equivalent to the function form
     (funcall #'(lambda lambda-list . body) . arguments)

2. どのメソッドを呼び出すか、実引数を評価した後に決める
     1. compute the list of applicable methods
     2. if no method is applicable then signal an error
     3. sort the applicable methods in order of specificity
     4. invoke the most specific method.
*/
static ListPtr eval_args(ListPtr args, EnvPtr env)
{
    std::cout << __func__ << ": "; PRINT(args, std::cout); std::cout << "\n"; // DEBUG

    if (args->length() == 0)
        return nilValue;

    std::shared_ptr<cons> ret = std::make_shared<cons>();
    for (auto it = args->begin(); it != args->end(); ++it)
        ret->append(EVAL(*it, env));

    return ret;
}


value_t EVAL(value_t ast, EnvPtr env)
{
    while (true) {
        std::cout << "EVAL() loop: "; PRINT(ast, std::cout); std::cout << "\n"; // DEBUG

        ListPtr list = OBJECT_CAST<class list>(ast);
        if (!list || list->empty() )
            return eval_atom(ast, env);

        // CL: special form と同名の関数は禁止.
        ast = macroExpand(ast, env);  // 実引数を評価せずに渡す
                                      // TODO: 実引数の個数の事前検査?
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
                if (op.name == special) {
                    Trampoline sp_ret = op.func(std::dynamic_pointer_cast<cons>(list), env);
                    if (sp_ret.type == Trampoline::DONE)
                        return sp_ret.value;
                    else { // type == Trampoline::MORE
                        ast = sp_ret.value; // TCO
                        if (sp_ret.innerEnv != nullptr)
                            env = sp_ret.innerEnv;
                        goto NEXT;
                    }
                }
            }
        }

        // Now we're left with the case of a regular list to be evaluated.
        // だいぶ手抜きでいく
        evaled = eval_args(list->sub(1), env);
        // lambda form だけ特別扱いされる
        func = get_function(list->at(0), env); // TODO: メソッド選定
        // ここではもう, 元の env は不要
        if ( func->is_builtin() ) {
            env = nullptr;
            return func->apply(evaled);
        }
        else {
            env = func->bind_arguments(evaled);
            ast = make_progn(func->getBody());  // TCO
        }

    NEXT: ;
    }
}

} // namespace my
