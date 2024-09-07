
// クラスと S式

#ifndef INCLUDE_TYPES_H
#define INCLUDE_TYPES_H

#include "my_debug.h"
#include <map>
#include <unicode/unistr.h>
#include <variant>
#include <vector>
#include <map>
#include <list>
#include <stdexcept>
#include <memory>  // std::shared_ptr<>

namespace my {

class EmptyInputException : public std::runtime_error { };

/*
定数はクラスではない  -> 環境のほうで対応する
  (defconstant const '(1 2 3))
  (push 'x const)
  ; ==>
  ;   (SETQ CONST (CONS 'X CONST))
  error: CONST is a constant and thus can't be set.

定数への再代入が禁止されるのであって、オブジェクトの変更は可能
* (defconstant const '(1 2 3))
CONST
* (nconc const 'x)
(1 2 3 . X)
* const
(1 2 3 . X)
*/

/*
(describe) でクラスが分かる. (describe 'fixnum) とするとクラスの説明

* (describe 1)
1
  [fixnum]
* (describe 1.2)
1.2
  [single-float]
*/

/** 頂点オブジェクト. t はこの唯一のインスタンス
歴史的に, type と class が混在している
See https://sellout.github.io/2012/03/03/common-lisp-type-hierarchy

* (describe (find-class t))
#<SB-PCL:SYSTEM-CLASS COMMON-LISP:T>
  [standard-object]

Class precedence-list: T
Direct subclasses: ARRAY, SIMD-PACK-256, SIMD-PACK, NUMBER,
                   SB-KERNEL::RANDOM-CLASS, SB-KERNEL:FDEFN,
                   SB-KERNEL:CODE-COMPONENT, WEAK-POINTER,
                   SYSTEM-AREA-POINTER, SYMBOL, CHARACTER,
                   SB-PCL::SLOT-OBJECT, SEQUENCE, STREAM, FUNCTION
No direct slots.
*/
class object //: public RefCounted
{
protected:
    object() {
        TRACE_OBJECT("Creating object %p\n", this);
    }

    virtual ~object() {
        TRACE_OBJECT("Destroying object %p\n", this);
    }

    // methods //////////////////////////////////////////////////

    // EQ, EQL, EQUAL, EQUALP がある。
    // このうち EQ は override されず, 構文で限られた箇所でだけ使われる
    // 通常は, 型を区別したいなら EQL を使え。
    //     -> EQL も区別しすぎ。my::string を見よ.
    // CL: EQUAL は、Web上の多くの解説と異なり、首尾一貫していない;
    // See https://www.lispworks.com/documentation/HyperSpec/Body/f_equal.htm
    // 数値には function =, 文字には char=, 文字列には string= 関数を使え.
    //bool isEqualTo(const malValue* rhs) const;
public:
    virtual icu::UnicodeString print() const = 0;
    virtual void write_indented(std::ostream& out, int level) const ;
};

typedef std::shared_ptr<object> ObjectPtr;


//////////////////////////////////////////////////////////////////////
// 数値

/** 数値
fixnum (62bit整数), single-float (単精度), ratio (有理数) がある.

  NUMBER names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:NUMBER>:
  Class precedence-list: NUMBER, T
  Direct superclasses: T
  Direct subclasses: COMPLEX, REAL
  Sealed.
  No direct slots.
*/
class number : public object
{

};


/** 62bit 整数
  FIXNUM names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:FIXNUM>:
  Class precedence-list: FIXNUM, INTEGER, RATIONAL, REAL, NUMBER, T
  Direct superclasses: INTEGER
  No subclasses.
  Sealed.
  No direct slots.
*/
class fixnum : public number
{
public:
    explicit fixnum(int64_t value) : m_value(value) { }

    // @override
    virtual icu::UnicodeString print() const {
        char buf[22];
        sprintf(buf, "%ld", m_value);
        return buf;
        // `with()` だと ‘class icu_72::number::UnlocalizedNumberFormatter’ has no member named ‘format’
      //UErrorCode err1, err2;
      //return icu::number::NumberFormatter::withLocale(icu::Locale("ja", "JP")).formatInt(m_value, err1).toString(err2);
      //return std::to_string(m_value);
    }

private:
    const int64_t m_value;
};


// 1.0d0 のように exponent marker 'd' で倍精度になる。変態か
// 'e' は *read-default-float-format* に従う. デフォルトは単精度
// 手抜いて, いつでも倍精度にする
class double_float : public number
{
public:
    explicit double_float(double num) : number_(num) { }

    // @override
    virtual icu::UnicodeString print() const {
        // 細かくややこしい. "%.17g"
        // See https://qiita.com/dankogai/items/a333bed751e4b0a71975
        // 2.2204460492503131e-16
        char buf[26];
        sprintf(buf, "%.17g", number_);
        return buf;
    }

private:
    double number_;
};


//////////////////////////////////////////////////////////////////////
// S式
//  - `nil` は list として表現する
//  - `t` は (bool) true
//  - 整数と浮動小数点数も特別扱い

typedef std::variant< bool, int64_t, double, ObjectPtr > value_t;

extern bool value_isa(const value_t& , const icu::UnicodeString& klass);

extern const std::shared_ptr<class null> nilValue;
extern const std::shared_ptr<class symbol> trueValue;

extern value_t READ(std::istream& stream);

template <typename _T>
inline std::shared_ptr<_T> OBJECT_CAST(const value_t& val)
{
    // いきなり get() すると bad_variant_access エラーが起こりうる
    if ( std::holds_alternative<bool>(val) )
        return std::dynamic_pointer_cast<_T>(trueValue);
    else if ( std::holds_alternative<ObjectPtr>(val))
        return std::dynamic_pointer_cast<_T>(std::get<ObjectPtr>(val));

    return nullptr;
}

template <>
inline std::shared_ptr<number> OBJECT_CAST(const value_t& val)
{
    // いきなり get() すると bad_variant_access エラーが起こりうる
    if ( std::holds_alternative<int64_t>(val) )
        return std::make_shared<fixnum>(std::get<int64_t>(val));
    else if ( std::holds_alternative<double>(val) )
        return std::make_shared<double_float>(std::get<double>(val));

    return nullptr;
}


template<typename _T>
std::shared_ptr<_T> cast_checked(const value_t& value, const char* typeName) {
    std::shared_ptr<_T> ret = OBJECT_CAST<_T>(value);
    ASSERT(ret != nullptr, "must a %s", typeName);
    return ret;
}

// エラーチェック付き
#define VALUE_CAST_CHECKED(type, value)   cast_checked<type>(value, #type)


//////////////////////////////////////////////////////////////////////
// string

/** 文字列
CL: string = character を格納する vector.
    -> いったん character クラスはつくらずにいく。object から派生させる.

* (describe (class-of "hoge"))
#<BUILT-IN-CLASS SB-KERNEL:SIMPLE-CHARACTER-STRING>
  [standard-object]

Class precedence-list: SB-KERNEL:SIMPLE-CHARACTER-STRING,
                       SB-KERNEL::CHARACTER-STRING, SIMPLE-STRING,
                       STRING, VECTOR, SIMPLE-ARRAY, ARRAY, SEQUENCE, T
Direct superclasses: SB-KERNEL::CHARACTER-STRING, SIMPLE-STRING
No subclasses.
Sealed.
No direct slots.
*/
class string : public virtual object
{
public:
    explicit string(const icu::UnicodeString& str) : string_(str) { }

    // @override
    virtual icu::UnicodeString print() const;

/* 一致判定に EQL を使ってはいけない
* (eql "one" "one")
NIL
* (equal "two" "two")
T
* (string= "3" "3")
T
*/

private:
    icu::UnicodeString string_;
};


//////////////////////////////////////////////////////////////////////
// sequence

// コンテナの基底クラス。ハッシュは除く。
// simple-xx クラスは伸縮不可。string vs. simple-string など.
class sequence : public virtual object
{
public:
    // (length nil) => 0
    virtual int length() const noexcept = 0;

    virtual value_t at(int idx) const = 0;
};


// CL-USER:FOO は CL-USER パッケージの FOO シンボルの意味。
// :keyword も symbol
// Direct superclasses: T
//
// CL: symbol は immutable. グローバルにインスタンスを共有 (アドレスが同じ)
//     string と異なり, EQ, EQL で一致判定できる
class symbol : public virtual object
{
public:
    explicit symbol(const icu::UnicodeString& str) : string_(str) { }

    // @override
    virtual icu::UnicodeString print() const;

    icu::UnicodeString name() const { return string_; }

private:
    icu::UnicodeString const string_;
};


typedef std::shared_ptr<class list> ListPtr;

// cons 型, null 型
class list : public virtual sequence
{
public:
    virtual bool empty() const noexcept { return length() == 0; }

    // 挙動が微妙
    // (elt '(a b c) 0)  => A
    // (elt nil 0)       => The index 0 is too large for a list of length 0
    // (car nil)         => NIL
    virtual value_t car() const noexcept = 0;

    virtual ListPtr sub(int start) const = 0;

    // operator [] があるもの
    typedef std::vector<value_t> Container;
    typedef typename Container::const_iterator  const_iterator ;
    typedef typename Container::iterator        iterator ;

    virtual iterator begin() noexcept = 0;
    virtual iterator end() noexcept = 0;
    virtual const_iterator begin() const noexcept = 0;
    virtual const_iterator end() const noexcept = 0;
};


// 値があるのが cons 型, () が null 型
class cons : public virtual list
{
public:
    cons() { }

    template <class InputIterator>
    cons(InputIterator first, InputIterator last): list_(first, last) { }

    // @override
    virtual icu::UnicodeString print() const ;

    // @override
    virtual void write_indented(std::ostream& out, int level) const ;

    /* @override sequence
       範囲外は std::out_of_range 例外
       * (elt '( 1 2 . 3) 1)
       2
       * (elt '( 1 2 . 3) 2)   #=> error: is not of type LIST
    */
    virtual value_t at(int idx) const {
        if (idx < 0)
            throw std::out_of_range("idx must >= 0");

        const int len = length();
        if ( len >= 3 && (idx == len - 2 || idx == len - 1) ) {
            std::shared_ptr<symbol> sym =
                                OBJECT_CAST<symbol>(list_.at(len - 2));
            if (sym && sym->name() == ".")
                throw std::runtime_error("value is not of type LIST");
        }

        return list_.at(idx);
    }

    // @override sequence
    // 次は考慮しない: (length '(1 2 . 3))  #=> error: is not of type LIST
    int length() const noexcept { return list_.size(); }

    // @override
    value_t car() const noexcept {
        return list_.front();
    }

    // @override
    ListPtr sub(int start) const {
        if (start < 0 || start > length() )
            throw std::out_of_range("start must >= 0 && <= length()");

        if (start == length() )
            return std::static_pointer_cast<list>(nilValue);
        else
            return std::make_shared<cons>(list_.begin() + start, list_.end());
    }

    // NIL を足してもよい.
    list& append(const value_t& ptr) {
        list_.push_back(ptr);
        return *this;
    }

    void pop_back() { list_.pop_back(); }

    void append_range(const ListPtr val) {
        if ( val->empty() )
            ; // nil を足しても何もしない
        else {
            // std::static_pointer_cast だとエラー
            //std::shared_ptr<cons> p = std::dynamic_pointer_cast<cons>(val);
            // std::append_range() は C++23
            list_.insert(list_.end(), val->begin(), val->end());
        }
    }

    iterator begin() noexcept { return list_.begin(); }
    iterator end() noexcept { return list_.end() ; }
    const_iterator begin() const noexcept { return list_.begin(); }
    const_iterator end() const noexcept { return list_.end(); }

private:
    Container list_;
};


// インスタンスは `nil` のみ
class null : public virtual symbol, public virtual list
{
public:
    null() : symbol("NIL") { }

    // @override sequence
    int length() const noexcept { return 0; }

    // @override sequence
    // 範囲外 (常に) std::out_of_range 例外
    value_t at(int idx) const {
        throw std::out_of_range("The index is too large");
    }

    // @override
    // (car nil)  => NIL
    value_t car() const noexcept {
        return nilValue;
    }

    // @override
    ListPtr sub(int start) const {
        if (start < 0 || start > length() )
            throw std::out_of_range("start must >= 0 && <= length()");

        return std::static_pointer_cast<list>(nilValue);
    }

    iterator begin() noexcept { return iterator(); }
    iterator end() noexcept { return iterator(); }
    const_iterator begin() const noexcept { return const_iterator(); }
    const_iterator end() const noexcept { return const_iterator(); }
};


/** 伸縮可能からつくる。simple-vector は伸縮不可.
* (describe (class-of #1a(1 2)))
#<BUILT-IN-CLASS COMMON-LISP:SIMPLE-VECTOR>
  [standard-object]

Class precedence-list: SIMPLE-VECTOR, VECTOR, SIMPLE-ARRAY, ARRAY,
                       SEQUENCE, T
Direct superclasses: VECTOR, SIMPLE-ARRAY
No subclasses.
Sealed.
No direct slots.
*/
class vector : public sequence
{
public:
    vector() { }

    // @override
    virtual icu::UnicodeString print() const;

    // @override
    // 範囲外は std::out_of_range 例外
    virtual value_t at(int idx) const { return vector_.at(idx); }

private:
    std::vector<value_t> vector_;
};


struct PrintVisitor {
    PrintVisitor() { }

    //icu::UnicodeString operator()(std::monostate) { return nilValue->print(); }
    icu::UnicodeString operator()(bool) { return trueValue->print();  }
    icu::UnicodeString operator()(int64_t v) { return fixnum(v).print() ; }
    icu::UnicodeString operator()(double v) { return double_float(v).print() ; }
    icu::UnicodeString operator()(const std::shared_ptr<object>& v) {
        return v->print() ; }
};


} // of namespace my

#endif // INCLUDE_TYPES_H
