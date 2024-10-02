
#include "../environment.h"
#include <sstream>
#include <iostream>
#include <fstream>
#include "../eval.h"


int main() {
    my::setup_functions();

    icu::UnicodeString ast =
        "(print "
        "   (let ((x 1)) "
        "      (block stop (setq x 2) (print x) (return-from stop) (setq x 3)) "
        "      x) "
        " ) ";

    my::value_t astv = my::read_from_string(ast);
    my::EVAL1(astv, my::globalEnv);  //=> 2

    return 0;
}
