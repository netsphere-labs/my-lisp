
#include "environment.h"
#include "my_debug.h"

#include <algorithm>
#include <unicode/unistr.h>
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

// TODO: インタプリタ class に移動
EnvPtr global_env = std::make_shared<Environment>();


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

    it = global_env->m_values.find(symbol);
    if (it != global_env->m_values.end())
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

    it = global_env->m_functions.find(symbol);
    if (it != global_env->m_functions.end())
        return it->second;

    std::string u;
    throw std::invalid_argument(symbol.toUTF8String(u) );
}


void Environment::set_value(const UnicodeString& symbol, const value_t& value, bool constant)
{
    m_values.insert(std::make_pair(symbol, BoundValue { .val = value, .constant = constant}));
}

void Environment::set_function(const UnicodeString& symbol, FuncPtr func)
{
    m_functions.insert(std::make_pair(symbol, func));
}

/*
Environment* Environment::getRoot() noexcept
{
    // Work our way down the the global environment.
    for (Environment* env = this; ; env = env->m_outer.get() ) {
        if (!env->m_outer)
            return env;
    }
}
*/

} // namespace my
