
#include "../s_expr.h"
#include <utility>
//#include <fstream>
#include <stdexcept>
#include <iostream>
#include <fstream>

// Multiline comments
void test_comment()
{
    // TODO: impl.
}

int main(int argc, char** argv)
{
    std::ifstream ifs("test.lisp", std::ios_base::in | std::ios_base::binary);

    try {
        my::value_t lst = my::READ(ifs);
        my::PRINT(lst, std::cout);
    } catch (const std::exception& ex) {
        std::cerr << ex.what() << '\n';
    }

    return 0;
}
