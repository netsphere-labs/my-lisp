
#ifndef MYLISP_EVAL_H
#define MYLISP_EVAL_H

#include "s_expr.h"
#include "environment.h"

namespace my {

extern value_t EVAL1(value_t ast, EnvPtr env);
extern bool value_isTrue(const value_t& value) ;

extern void setup_functions();

extern void define_macro(const icu::UnicodeString& name,
                         const icu::UnicodeString& params,
                         ListPtr body);

} // namespace my

#endif // !MYLISP_EVAL_H
