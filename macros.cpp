
#include "environment.h"

namespace my {

extern value_t EVAL(value_t ast, EnvPtr env);
extern value_t eval_atom(const value_t& atom, EnvPtr env);


// Macro DEFMACRO
// defmacro name lambda-list [[declaration* | documentation]] form*
//
// global environment に macro function を登録する
// defmacro name lambda-list [[declaration* | documentation]] form*
void define_macro(const icu::UnicodeString& name,
                  const icu::UnicodeString& params,
                  ListPtr body)  // (defmacro hoge () 1) も通る
{
    value_t paramv = read_from_string(params);
    ListPtr param_list = VALUE_CAST_CHECKED(list, paramv);
    FuncPtr func_ptr = std::make_shared<function>(name, param_list, body, nullptr, true);
    globalEnv->set_function(name, func_ptr);
}


// 展開結果が list とは限らない
value_t function::expand_macro(ListPtr args)
{
    if (m_params->length() != args->length())
        throw std::runtime_error("args count mismatch");

    EnvPtr env = std::make_shared<Environment>();
    for (int i = 0; i < m_params->length(); ++i) {
        std::shared_ptr<symbol> var = VALUE_CAST_CHECKED(symbol, m_params->at(i));
        env->set_value(var->name(), args->at(i), false);
    }

    // ここからマクロ本体を展開
    return EVAL(m_body, env); // EVAL() 経由で do_quasiquote()
}


// ビルトイン function MACROEXPAND, MACROEXPAND-1
// 例:
//   (defmacro alpha (x y) `(beta ,x ,y))    =>  ALPHA
//   (macroexpand-1 '(alpha a b))            =>  (BETA A B), T  この段階では a, bは評価されない
//
//   (macroexpand-1 1)   => 1, NIL
//   (macroexpand-1 '(1 2))   => (1 2), NIL
//   (macroexpand-1 '(list 1 2))  => (LIST 1 2), NIL
my::value_t macroExpand1(my::EnvPtr args)
{
    // list とは限らない. EVAL() と同様に場合分けしていく
    value_t ast = args->find_value("FORM");

    ListPtr tmpl = OBJECT_CAST<class list>(ast);
    if (!tmpl || tmpl->empty() )
        return eval_atom(ast, args); // ここは評価する

    std::shared_ptr<symbol> op = OBJECT_CAST<symbol>(tmpl->car());
    if (op != nullptr) {
        FuncPtr fn = args->find_function(op->name() );
        if (fn != nullptr && fn->is_macro() )
            return fn->expand_macro(tmpl->sub(1));
    }

    return tmpl; // 評価せず戻す
}

/**
 * repeatedly expands form until it is no longer a macro form.
 * @return 展開した AST を返す
 */
value_t macroExpand(EnvPtr args)
{
    // TODO: impl.
    return macroExpand1(args);
}


} // namespace my
