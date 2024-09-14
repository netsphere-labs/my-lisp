
// マクロのテスト
#include "../environment.h"
#include <iostream>

namespace my {

extern value_t EVAL(value_t ast, EnvPtr env);

// Function MACROEXPAND, MACROEXPAND-1
extern value_t macroExpand1(EnvPtr args);

extern void define_macro(const icu::UnicodeString& name,
                         const icu::UnicodeString& params,
                         ListPtr body);

// ビルトイン関数
value_t do_add(EnvPtr args)
{
    value_t x = args->find_value("X");  double xv = std::get<double>(x);
    value_t y = args->find_value("Y");  double yv = std::get<double>(y);

    return xv + yv;
}

value_t do_multiply(EnvPtr args)
{
    value_t x = args->find_value("X"); double xv = std::get<double>(x);
    value_t y = args->find_value("Y"); double yv = std::get<double>(y);

    return xv * yv;
}

} // namespace my


// quasiquote の呼び出し
// `(+ ,a (* ,b 3))   => (+ 10 (* 30 3))
void test_quasiquote()
{
    my::EnvPtr env = std::make_shared<my::Environment>();

    my::value_t form;
    my::value_t res;
    form = my::read_from_string("(setq a 10)");
    res = my::EVAL(form, env);

    form = my::read_from_string("(setq b 30)");
    res = my::EVAL(form, env);

    form = my::VALUE_CAST_CHECKED(class my::list, my::read_from_string("`(+ ,a (* ,b 3))"));
    my::PRINT(form, std::cout);
    res = my::EVAL(form, env);
    my::PRINT(res, std::cout);
}

void test_expand()
{
    std::cout << __func__ << " ======================================= \n";

    my::EnvPtr env = std::make_shared<my::Environment>();

    // 展開してみる
    my::value_t arg1 = my::read_from_string("(mac1 3 4)");
    my::FuncPtr p = env->find_function("MACROEXPAND-1");
    std::shared_ptr<my::cons> args = std::make_shared<my::cons>();
    args->append(arg1);
    my::value_t result = p->apply(args);
    my::PRINT(result, std::cout);  // => (+ 3 (* 4 3))
}

int main()
{
    define_function("MACROEXPAND-1", "(form)", my::macroExpand1);
    define_function("+", "(x y)", my::do_add);
    define_function("*", "(x y)", my::do_multiply);

    test_quasiquote();

    my::ListPtr list = my::VALUE_CAST_CHECKED(class my::list, my::read_from_string("`(+ ,a (* ,b 3))"));
    my::define_macro("MAC1", "(a b)", list);

    test_expand();

    // 評価する
    my::EnvPtr env = std::make_shared<my::Environment>();
    my::value_t arg1 = my::read_from_string("(mac1 3 4)");
    my::value_t result = my::EVAL(arg1, env);
    my::PRINT(result, std::cout);

    return 0;
}
