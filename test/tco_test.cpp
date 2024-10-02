
#include "../environment.h"
#include <sstream>
#include <iostream>
#include <fstream>
#include "../eval.h"


int main()
{
    my::EnvPtr env = std::make_shared<my::Environment>();

    my::setup_functions();

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
