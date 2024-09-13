
#include "../environment.h"
#include <sstream>
#include <iostream>

namespace my {
    extern value_t EVAL(value_t ast, EnvPtr env);
    extern bool value_isTrue(const value_t& value) ;

// ビルトイン関数
// Function NOT
value_t do_not(my::EnvPtr args) {
    my::value_t x = args->find_value("X");
    return value_isTrue(x) ? my::nilValue : my::trueValue;
}

// ビルトイン関数
// 標準出力に出力
// Function WRITE, PRIN1, PRINT, PPRINT, PRINC
value_t do_print(my::EnvPtr args) {
    my::value_t x = args->find_value("X");
    double v = std::get<double>(x);
    printf("%f\n", v);

    return my::nilValue;
}

} // namespace my


my::value_t func1(my::EnvPtr args) {
    my::value_t x = args->find_value("X");
    int64_t v = std::get<int64_t>(x);
    printf("func1: %ld\n", v);

    return x;
}

my::value_t func2(my::EnvPtr args) {
    my::value_t x = args->find_value("X");

    std::cout << __func__ << ": "; PRINT(x, std::cout); std::cout << "\n"; // DEBUG

    double v = std::get<double>(x);
    printf("func2: %f\n", v);

    return x;
}


void test_lambda()
{
    // TODO: ● lambda expr のテスト
}

int main()
{
    define_function("FUNC1", "(x)", func1);
    define_function("FUNC2", "(x)", func2);
    define_function("NOT", "(x)", my::do_not);
    define_function("PRINT", "(x)", my::do_print);

    icu::UnicodeString ast =
        "(progn "
        "   (setq x 5) "
        "   (if (not x) (func1 x) (let ((x 30)) (func2 x))) "
        "   (print x))";
    my::value_t astv = my::read_from_string(ast);
    my::EVAL(astv, my::globalEnv);

    return 0;
}
