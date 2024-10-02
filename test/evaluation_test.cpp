
#include "../environment.h"
#include <sstream>
#include <iostream>
#include <fstream>
#include "../eval.h"


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
    printf("func2: %f\n", v);  //=> 30

    return x;
}

// * (defun times-elm (n xs)
//     (mapcar (lambda (x) (* x n)) xs))   n, xs をレキシカルに外側へ探す
// * (times-elm 3 '(1 2 3))
// (3 6 9)
void test_lambda()
{
    my::EnvPtr env = std::make_shared<my::Environment>();

    std::ifstream ifs("lambda.lisp", std::ios_base::in | std::ios_base::binary);
    my::value_t astv = my::READ(ifs);
    my::value_t res = my::EVAL1(astv, env);
    PRINT(res, std::cout);  //=> (3 33 300)
}

int main()
{
    my::setup_functions();

    define_function("FUNC1", "(x)", func1);
    define_function("FUNC2", "(x)", func2);

    icu::UnicodeString ast =
        "(progn "
        "   (setq x 5) "
        "   (if (not x) (func1 x) (let ((x 30)) (func2 x))) "
        "   (print x))";
    my::value_t astv = my::read_from_string(ast);
    my::EVAL1(astv, my::globalEnv);  //=> 5 外側がイキ.

    test_lambda();

    return 0;
}
