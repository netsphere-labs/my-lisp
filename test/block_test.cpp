
#include "../environment.h"
#include <sstream>
#include <iostream>
#include <fstream>

namespace my {
extern value_t EVAL1(value_t ast, EnvPtr env);
extern bool value_isTrue(const value_t& value) ;

extern void setup_functions();

} // namespace my

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
