
#include "../edit_line.h"
#include <unicode/unistr.h>
using namespace icu;

int main()
{
    my::EditLine editLine("~/my-lisp.history");

    UnicodeString line;
    bool f;
    do {
        f = editLine.get("* ", &line);
    } while (f && line != "quit") ;

    return 0;
}
