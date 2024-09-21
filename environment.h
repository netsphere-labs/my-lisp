
#ifndef INCLUDE_ENVIRONMENT_H
#define INCLUDE_ENVIRONMENT_H

#include "s_expr.h"
#include <map>
#include <unicode/unistr.h>
#include <functional> // std::function
#include <stack>

namespace my {

class Environment;
typedef std::shared_ptr<Environment> EnvPtr;


// λ式を (function) が評価して, function オブジェクトが生成
class function : public object
{
public:
    function() { }

    // 関数またはクロージャをつくって返す
    function(const icu::UnicodeString& name, ListPtr params,
             ListPtr body, // (defun ff () 1) も通る
             EnvPtr outer, bool isMacro = false) :
        m_name(name), m_params(params), m_body(body), m_outer_env(outer), m_isMacro(isMacro) { }

    function(const icu::UnicodeString& name, ListPtr params,
             const std::function<value_t(EnvPtr)>& handler) :
        m_name(name), m_params(params), m_handler(handler)  { }

    // @override
    virtual icu::UnicodeString print() const;

    icu::UnicodeString name() const { return m_name; }

    bool is_builtin() const noexcept { return m_handler != nullptr; }
    bool is_macro() const noexcept { return m_isMacro ; }

    ListPtr getBody() const noexcept { return m_body; }

    // 実引数の bind だけをおこなう
    EnvPtr bind_arguments(ListPtr evaled_args);

    // 関数を実行. bind_arguments() を含む
    virtual value_t apply(ListPtr evaled_args);

    value_t expand_macro(ListPtr args);

private:
    // クロージャは "<lambda>"
    icu::UnicodeString m_name;

    // 仮引数など
    ListPtr m_params;

    // ビルトイン
    std::function<value_t(EnvPtr)> m_handler;

    // implicit progn. nil がありえる.
    ListPtr m_body;

    EnvPtr m_outer_env; // lexical environment. function, macro では NULL.

    bool m_isMacro;
};

typedef std::shared_ptr<function> FuncPtr;

class GenericFunction : public function
{
public:
    // 実引数の型によって, 実際に呼び出すメソッドが変わる
    value_t apply(ListPtr evaled_args);
};


////////////////////////////////////////////////////////////////////////
// class Environment

/*
定数への再代入が禁止されるのであって、オブジェクトの変更は可能
-> 環境のほうで対応する
  (defconstant const '(1 2 3))
  (push 'x const)
  ; ==>
  ;   (SETQ CONST (CONS 'X CONST))
  error: CONST is a constant and thus can't be set.

* (defconstant const '(1 2 3))
CONST
* (nconc const 'x)
(1 2 3 . X)
* const
(1 2 3 . X)
*/


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
    // (setq if 10)   => これはエラー: PACKAGE-LOCK-VIOLATION. シンボルの束縛時にエラー.
    // (let ((if 10)) (+ if 10))  => これは通る! special op は function のほう.
    // 定数への(再)代入はエラー.
    void set_value(const icu::UnicodeString& symbol,
                          const value_t& value, bool constant);

    // function を設定する
    // (defun if (x) (+ x 10))    => これはエラー: Special form is an illegal function name: IF.
    // (defun list (x) (+ x 10))  #=> PACKAGE-LOCK-VIOLATION
    // (defun nil (x) (+ x 10))   #=> PACKAGE-LOCK-VIOLATION. シンボルの束縛時にエラー
    // 定数は set/find_value のほうなので、次は通る:
    // * (defconstant fuga 10)
    // FUGA
    // * (defun fuga (x) (+ x 10))
    void set_function(const icu::UnicodeString& symbol, FuncPtr value);

    void push_block_tag(const icu::UnicodeString& name) {
        _block_tag.push(name);
    }

    // ローカルと, global environment から探す
    value_t find_value(const icu::UnicodeString& symbol);

    // ローカルと, global environment から探す
    // @return 見つからなかったときは nullptr
    FuncPtr find_function(const icu::UnicodeString& symbol);

    void pop_block_tag(const icu::UnicodeString& tag) {
#ifndef NDEBUG
        icu::UnicodeString t = _block_tag.top();
        if (t != tag)
            throw std::runtime_error("internal error");
#endif
        _block_tag.pop();
    }

private:
    typedef std::map<icu::UnicodeString, BoundValue> ValueMap;
    typedef std::map<icu::UnicodeString, FuncPtr> FuncMap;

    // Lisp-2
    // (defun a ...) した後でも, (symbol-value 'a) は unbound エラー.
    //  -> symbol-value と symbol-function は独立している
    ValueMap m_values;
    FuncMap m_functions;  // 関数またはマクロ

    EnvPtr m_outer;

    std::stack<icu::UnicodeString> _block_tag;
};

extern EnvPtr globalEnv;

// global environment に関数を登録する
extern void define_function(const icu::UnicodeString& name,
                            const icu::UnicodeString& params,
                            std::function<my::value_t(my::EnvPtr)> func);


} // namespace my

#endif // INCLUDE_ENVIRONMENT_H
