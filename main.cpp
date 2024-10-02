
#include "edit_line.h"
#include "environment.h"
#include "s_expr.h"
#include "eval.h"
#include <iostream>
#include <unicode/unistr.h>
using namespace icu;

int main()
{
    my::EditLine editLine("~/my-lisp.history");

    my::EnvPtr toplevel = std::make_shared<my::Environment>();
    my::setup_functions();

    UnicodeString line;
    bool f;
    while ( editLine.get("* ", &line) ) {
        my::value_t astv = my::read_from_string(line);
        my::value_t r = my::EVAL1(astv, toplevel);
        PRINT(r, std::cout); std::cout << "\n";
    }

    return 0;
}
