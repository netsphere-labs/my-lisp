
#include "../environment.h"
#include <sstream>

namespace my {
    extern value_t EVAL(value_t ast, EnvPtr env);
    extern bool value_isTrue(const value_t& value) ;
}

my::value_t func1(my::EnvPtr args) {
    my::value_t x = args->find_value("X");
    int64_t v = std::get<int64_t>(x);
    printf("func1: %ld\n", v);

    return x;
}

my::value_t func2(my::EnvPtr args) {
    my::value_t x = args->find_value("X");
    int64_t v = std::get<int64_t>(x);
    printf("func2: %ld\n", v);

    return x;
}

// ビルトイン関数
// Function NOT
my::value_t do_not(my::EnvPtr args) {
    my::value_t x = args->find_value("X");
    return value_isTrue(x) ? my::nilValue : my::trueValue;
}

// ビルトイン関数
// 標準出力に出力
// Function WRITE, PRIN1, PRINT, PPRINT, PRINC
my::value_t do_print(my::EnvPtr args) {
    my::value_t x = args->find_value("X");
    int64_t v = std::get<int64_t>(x);
    printf("%ld\n", v);

    return my::nilValue;
}


int main()
{
    define_function("FUNC1", "(x)", func1);
    define_function("FUNC2", "(x)", func2);
    define_function("NOT", "(x)", do_not);
    define_function("PRINT", "(x)", do_print);

    icu::UnicodeString ast =
        "(progn "
        "   (setq x 5) "
        "   (if (not x) (func1 x) (let ((x 30)) (func2 x))) "
        "   (print x))";

    std::string u;
    std::stringstream ast_ss(ast.toUTF8String(u));
    my::value_t astv = my::READ(ast_ss);

    my::EVAL(astv, my::globalEnv);

    return 0;
}
