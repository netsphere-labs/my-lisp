
#include "environment.h"
#include <unicode/unistr.h>
#include <iostream>
using namespace icu;

namespace my {

extern bool value_isTrue(const value_t& value) ;

// Function MACROEXPAND, MACROEXPAND-1
extern value_t macroExpand1(EnvPtr args);

/* https://hyotang666.github.io/archives/structure-vs-class.html

  DEFSTRUCT で自動生成される名前とビルトイン関数の名前が、チグハグ
    predicate named name-p,
       -- (typep subclass 'my-class) があれば足りる
    constructor function named make-constructor-name
       -- MAKE-INSTANCE があれば不要では?
    copier function named copy-constructor-name
       -- COPY-STRUCTURE があれば不要
    STRUCT名-SLOT名というアクセサ. 総称関数ではない。slot名だけのほうがいい?
       (slot-value c 'slot) これでアクセサがなくても値を取れる
 */

//////////////////////////////////////////////////////////////////////////
// <object>

// ビルトイン関数
// Function NOT
value_t do_not(my::EnvPtr args) {
    my::value_t x = args->find_value("X");
    return value_isTrue(x) ? my::nilValue : my::trueValue;
}

// ビルトイン関数
// 標準出力に出力
// Function WRITE, PRIN1, PRINT, PPRINT, PRINC
value_t do_print(my::EnvPtr args) {
    my::value_t x = args->find_value("X");
    PRINT(x, std::cout);
    std::cout << "\n";

    return my::nilValue;
}

// (setq a 1) =>  1
// (list a 2) =>  (1 2)
value_t do_list(EnvPtr args) {
    // 呼び出し時に評価済み
    return args->find_value("OBJECTS");
}


//////////////////////////////////////////////////////////////////////////
// <number>

// ビルトイン関数
value_t do_add(EnvPtr args)
{
    value_t x = args->find_value("X");  double xv = std::get<double>(x);
    value_t y = args->find_value("Y");  double yv = std::get<double>(y);

    return xv + yv;
}

value_t do_multiply(EnvPtr args)
{
    value_t x = args->find_value("X"); double xv = std::get<double>(x);
    value_t y = args->find_value("Y"); double yv = std::get<double>(y);

    return xv * yv;
}


//////////////////////////////////////////////////////////////////////////
// <function>

value_t do_mapcar(EnvPtr args)
{
    FuncPtr func = VALUE_CAST_CHECKED(function, args->find_value("FUNC"));
    ListPtr list = VALUE_CAST_CHECKED(class list, args->find_value("LIST"));

    std::shared_ptr<cons> ret = std::make_shared<cons>();
    for (const auto& v : *list) {
        // ここは eval しなおさない
        std::shared_ptr<cons> args = std::make_shared<cons>();
        args->append(v);
        ret->append( func->apply(args) );
    }

    if (ret->empty())
        return nilValue;
    else
        return ret;
}


struct BuiltinFunc {
    const UnicodeString& name;
    const UnicodeString& params;
    std::function<my::value_t(my::EnvPtr)> func;
};

static const BuiltinFunc funcs[] = {
    {"MACROEXPAND-1", "(form)", my::macroExpand1 },
    // <object>
    {"NOT", "(x)", my::do_not },
    {"PRINT", "(x)", my::do_print },
    {"LIST", "(&rest objects)", do_list },
    // <number>
    {"+", "(x y)", my::do_add },
    {"*", "(x y)", my::do_multiply },
    // <function>
    {"MAPCAR", "(func list)", do_mapcar },
};

void setup_functions()
{
    for (const auto& f : funcs)
        define_function(f.name, f.params, f.func);
}


} // namespace my
