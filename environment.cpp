
#include "environment.h"
#include "my_debug.h"

#include <algorithm>
#include <unicode/unistr.h>
#include <sstream>
#include <iostream>
using namespace icu;

namespace my {

///////////////////////////////////////////////////////////////////////////
// class function

icu::UnicodeString function::print() const {
    return "#<FUNCTION " + name() + ">";
}

// ビルトイン関数の実行
// @param env 実引数が評価された環境
value_t function::apply(ListPtr eval_args)
{
    EnvPtr inner = bind_arguments(eval_args);
    return m_handler(inner);
}

EnvPtr function::bind_arguments(ListPtr eval_args)
{
    if (m_params->length() != eval_args->length())
        throw std::runtime_error("Argument length mismatch");

    EnvPtr inner = std::make_shared<Environment>(m_outer_env); // クロージャの場合

    // 突き合わせしながら環境に登録
    for (int i = 0; i < m_params->length(); ++i) {
        std::shared_ptr<symbol> id = VALUE_CAST_CHECKED(symbol, m_params->at(i));
        inner->set_value(id->name(), eval_args->at(i), false);
    }

    return inner;
}


///////////////////////////////////////////////////////////////////////////
// class Environment

Environment::Environment(EnvPtr outer) : m_outer(outer)
{
    TRACE_ENV("Creating environment %p, outer=%p\n", this, m_outer.get() );
}


Environment::~Environment()
{
    TRACE_ENV("Destroying environment %p\n", this);
}


value_t Environment::find_value(const UnicodeString& symbol)
{
    ValueMap::const_iterator it;
    for (Environment* env = this; env; env = env->m_outer.get() ) {
        it = env->m_values.find(symbol);
        if (it != env->m_values.end())
            return it->second.val;
    }

    it = globalEnv->m_values.find(symbol);
    if (it != globalEnv->m_values.end())
        return it->second.val;

    std::string u;
    throw std::invalid_argument(symbol.toUTF8String(u) );
}

FuncPtr Environment::find_function(const UnicodeString& symbol)
{
    FuncMap::const_iterator it;
    for (Environment* env = this; env; env = env->m_outer.get() ) {
        it = env->m_functions.find(symbol);
        if (it != env->m_functions.end())
            return it->second;
    }

    it = globalEnv->m_functions.find(symbol);
    if (it != globalEnv->m_functions.end())
        return it->second;

    std::string u;
    throw std::invalid_argument(symbol.toUTF8String(u) );
}


void Environment::set_value(const UnicodeString& symbol, const value_t& value,
                            bool constant)
{
    std::cout << __func__ << ": "; PRINT(value, std::cout); std::cout << "\n"; // DEBUG

    m_values.insert(std::make_pair(symbol, BoundValue { .val = value, .constant = constant}));
}

void Environment::set_function(const UnicodeString& symbol, FuncPtr func)
{
    m_functions.insert(std::make_pair(symbol, func));
}


// TODO: インタプリタクラスへの移動
EnvPtr globalEnv = std::make_shared<Environment>();

void define_function(const icu::UnicodeString& name,
                            const icu::UnicodeString& params,
                            std::function<my::value_t(my::EnvPtr)> func)
{
    value_t paramv = read_from_string(params);
    ListPtr param_list = VALUE_CAST_CHECKED(list, paramv);
    my::FuncPtr func_ptr = std::make_shared<my::function>(name, param_list, func);
    my::globalEnv->set_function(name, func_ptr);
}


} // namespace my
