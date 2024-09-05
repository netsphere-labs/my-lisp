
#ifndef INCLUDE_ENVIRONMENT_H
#define INCLUDE_ENVIRONMENT_H

#include "s_expr.h"
#include <map>
#include <unicode/unistr.h>
#include <functional> // std::function

namespace my {

class Environment;
typedef std::shared_ptr<Environment> EnvPtr;


// λ式を (function) が評価して, function オブジェクトが生成
class function : public object
{
public:
    function() { }

    // 関数またはクロージャをつくって返す
    function(const icu::UnicodeString& name, ListPtr params, ListPtr body,
             EnvPtr outer) : m_name(name), m_params(params), m_body(body), m_outer_env(outer) { }

    function(const icu::UnicodeString& name, ListPtr params,
             const std::function<value_t(EnvPtr)>& handler) :
        m_name(name), m_params(params), m_handler(handler)  { }

    // @override
    virtual icu::UnicodeString print() const;

    icu::UnicodeString name() const { return m_name; }

    bool is_builtin() const { return m_handler != nullptr; }

    ListPtr getBody() const { return m_body; }

    // 実引数の bind だけをおこなう
    EnvPtr bind_arguments(ListPtr evaled_args);

    // 関数を実行. bind_arguments() を含む
    value_t apply(ListPtr evaled_args);

private:
    // クロージャは "<lambda>"
    icu::UnicodeString m_name;

    // 仮引数など
    ListPtr m_params;

    // ビルトイン
    std::function<value_t(EnvPtr)> m_handler;

    // implicit progn. nil がありえる
    ListPtr m_body;

    EnvPtr m_outer_env; // lexical environment. function では NULL.

    bool m_isMacro;
};

typedef std::shared_ptr<function> FuncPtr;


struct BoundValue {
    value_t val;
    bool constant;
};


// 参照可能な, レキシカル変数の集合
// 定数はオブジェクトではなく、変数の属性
class Environment //: public RefCounted
{
public:
    // @param outer レキシカルに外側.
    Environment(EnvPtr outer = nullptr);

    ~Environment();

    // value を設定する
    void set_value(const icu::UnicodeString& symbol,
                          const value_t& value, bool constant);

    // function を設定する
    void set_function(const icu::UnicodeString& symbol, FuncPtr value);

    // ローカルと, global environment から探す
    value_t find_value(const icu::UnicodeString& symbol);

    // ローカルと, global environment から探す
    FuncPtr find_function(const icu::UnicodeString& symbol);

private:
    typedef std::map<icu::UnicodeString, BoundValue> ValueMap;
    typedef std::map<icu::UnicodeString, FuncPtr> FuncMap;

    // Lisp-2
    // (defun a ...) した後でも, (symbol-value 'a) は unbound エラー.
    //  -> symbol-value と symbol-function は独立している
    ValueMap m_values;
    FuncMap m_functions;  // 関数またはマクロ

    EnvPtr m_outer;
};

} // namespace my

#endif // INCLUDE_ENVIRONMENT_H
