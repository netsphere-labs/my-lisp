
#include "../environment.h"
#include <stdio.h>
#include <iostream>

namespace my {
    extern void PRINT(const value_t& value, std::ostream& out);
}

my::value_t hoge(my::EnvPtr args)
{
    my::value_t x = args->find_value("X");
    my::value_t y = args->find_value("Y");

    printf("callback!\n");

    return x;
}

int main()
{
    my::Environment env;

    define_function("HOGE", "(x y)", hoge);

    // 呼び出し
    my::FuncPtr p = env.find_function("HOGE");
    std::shared_ptr<my::cons> args = std::make_shared<my::cons>();
    args->append(my::value_t(10));
    args->append(std::make_shared<my::string>("fuga"));
    my::value_t result = p->apply(args);

    PRINT(result, std::cout);

    return 0;
}
