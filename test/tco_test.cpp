
#include "../environment.h"
#include <sstream>
#include <iostream>
#include <fstream>

namespace my {
extern value_t EVAL1(value_t ast, EnvPtr env);
extern bool value_isTrue(const value_t& value) ;

extern void setup_functions();

my::value_t do_1minus(my::EnvPtr args) {
    my::value_t x = args->find_value("X");
    double v = std::get<double>(x);

    return v - 1;
}

// @return T or NIL
my::value_t do_gt(my::EnvPtr args) {
    double x = std::get<double>(args->find_value("X"));
    double y = std::get<double>(args->find_value("Y"));
    return x > y ? trueValue : nilValue;
}

} // namespace my


int main()
{
    my::EnvPtr env = std::make_shared<my::Environment>();

    my::setup_functions();
    my::define_function("1-", "(x)", my::do_1minus);
    my::define_function(">", "(x y)", my::do_gt);
    my::globalEnv->set_value("T", my::trueValue, true);
    my::globalEnv->set_value("NIL", my::nilValue, true);

    // 偶数 = true
    icu::UnicodeString ast = "(defun iseven (n) (if (> n 0) (isodd (1- n)) t))";
    my::value_t astv = my::read_from_string(ast);
    my::EVAL1(astv, env);

    // 奇数 = true
    ast = "(defun isodd (n) (if (> n 0) (iseven (1- n)) nil))";
    astv = my::read_from_string(ast);
    my::EVAL1(astv, env);

    ast = "(print (iseven 10))";
    astv = my::read_from_string(ast);
    my::EVAL1(astv, env); //=> T

    ast = "(print (isodd 10))";
    astv = my::read_from_string(ast);
    my::EVAL1(astv, env);  //=> NIL

    ast = "(print (iseven 9))";
    astv = my::read_from_string(ast);
    my::EVAL1(astv, env);  //=> NIL

    ast = "(print (isodd 9))";
    astv = my::read_from_string(ast);
    my::EVAL1(astv, env); //=> T

    astv = my::read_from_string("(print (iseven 1000000))");
    my::EVAL1(astv, env);  //=> T

    return 0;
}
