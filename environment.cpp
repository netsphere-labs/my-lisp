
#include "environment.h"
#include "my_debug.h"

#include <algorithm>
#include <sstream>
#include <iostream>
#include <unicode/unistr.h>
#include <unicode/ustream.h>
using namespace icu;

namespace my {

///////////////////////////////////////////////////////////////////////////
// class function

icu::UnicodeString function::print() const {
    return "#<FUNCTION " + name() + ">";
}

EnvPtr function::bind_arguments(ListPtr eval_args)
{
    // クロージャの場合, 外側の環境を使える
    EnvPtr inner = std::make_shared<Environment>(m_outer_env);

    // 突き合わせしながら環境に登録
    list::const_iterator p = m_params->begin();
    list::const_iterator q = eval_args->begin();
    for ( ; p != m_params->end(); ++p, ++q) {
        std::shared_ptr<symbol> id = VALUE_CAST_CHECKED(symbol, *p);
        if (id->name() == "&REST") {
            if ( (++p) == m_params->end())
                throw std::runtime_error("&rest param missing");
            id = VALUE_CAST_CHECKED(symbol, *p);
            inner->set_value(id->name(), eval_args->sub(q), false);
            return inner;
        }

        if (q == eval_args->end())
            throw std::runtime_error("Argument length short");
        inner->set_value(id->name(), *q, false);
    }
    if (q != eval_args->end())
        throw std::runtime_error("Argument length long");

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

// @return 見つからなかったときは nullptr
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
    return nullptr; // std::invalid_argument(symbol.toUTF8String(u) );
}


void Environment::set_value(const UnicodeString& symbol, const value_t& value,
                            bool constant)
{
    //std::cout << __func__ << " at " << __LINE__ << ": ";
    //std::cout << symbol << " = " ;
    //PRINT(value, std::cout); std::cout << "\n"; // DEBUG

    // insert() は更新しない!
    m_values[symbol] = BoundValue { .val = value, .constant = constant};
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
