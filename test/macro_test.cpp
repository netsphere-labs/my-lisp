
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


int main()
{
    define_function("MACROEXPAND-1", "(form)", my::macroExpand1);
    define_function("+", "(x y)", my::do_add);
    define_function("*", "(x y)", my::do_multiply);

    my::ListPtr list = my::VALUE_CAST_CHECKED(class my::list, my::read_from_string("`(+ ,a (* ,b 3))"));
    my::define_macro("MAC1", "(a b)", list);

    my::EnvPtr env = std::make_shared<my::Environment>();

    // まず、展開してみる
    my::value_t arg1 = my::read_from_string("(mac1 3 4)");
    my::FuncPtr p = env->find_function("MACROEXPAND-1");
    std::shared_ptr<my::cons> args = std::make_shared<my::cons>();
    args->append(arg1);
    my::value_t result = p->apply(args);
    my::PRINT(result, std::cout);  // => (+ 3 (* 4 3))

    // 評価する
    //result = my::EVAL(arg1, env);
    //my::PRINT(result, std::cout);

    return 0;
}
