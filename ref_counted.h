
#ifndef REFCOUNTED_H
#define REFCOUNTED_H

#include "my_debug.h"

#include <cstddef>
#include <type_traits>  // std::is_base_of

namespace my {

// 参照カウンタ
// `std::shared_ptr` は, 格納するオブジェクトの外側に参照カウンタを保存する。
// 格納するオブジェクト型が限定できるなら、派生させたほうがいい
// See bits/shared_ptr.h
// ●● TODO: deleter を引数に取った方がいい?
class RefCounted
{
public:
    RefCounted() : m_refCount(1) { }

    virtual ~RefCounted() { }

    // @return 新しい参照カウントを返す
    int add_ref() const noexcept {
#ifndef _WIN32
        return __sync_add_and_fetch(&m_refCount, 1);
#else
        return InterlockedIncrement(&m_refCount); // 32bit
#endif
    }

    // @return 新しい参照カウントを返す
    int release() const {
#ifndef _WIN32
        int r = __sync_sub_and_fetch(&m_refCount, 1);
#else
        int r = InterlockedDecrement(&m_refCount);
#endif
        if (!r)
            delete this;
        return r;
    }

private:
    RefCounted(const RefCounted&); // no copy ctor
    RefCounted& operator = (const RefCounted&); // no assignments

    // 32bit
    mutable int m_refCount;
};


// `std::shared_ptr` のインタフェイスを真似する. 特に move
template<class _T>
class RefCountedPtr
{
    static_assert(std::is_base_of<RefCounted, _T>::value,
                  "_T must be derived of `RefCounted`");

public:
    // 後から更新がある.
    constexpr RefCountedPtr() noexcept : m_object(nullptr) { }

    // 生ポインタの所有権を受け取る
    explicit RefCountedPtr(_T* object) noexcept : m_object(object)
    {  }  // ここではカウンタを増やさない

    // copy
    RefCountedPtr(const RefCountedPtr& rhs) noexcept : m_object(rhs.m_object) {
        if (m_object)
            m_object->add_ref();
    }

    // move
    RefCountedPtr(RefCountedPtr&& rhs) noexcept : m_object(rhs.m_object) {
        rhs.m_object = nullptr;
    }

    // アップキャスト (基底クラスへの代入) を許可する
    //     CComPtr<IHen> hen;
    //     CComPtr<IUnknown> unknown = hen;
    template <typename _Derived>
    RefCountedPtr(const RefCountedPtr<_Derived>& other) noexcept
                                                : m_object(other.m_object) {
        if (m_object)
            m_object->add_ref();
    }

    ~RefCountedPtr() noexcept {
        safe_release();
    }

    // 入れ替え. 生ポインタ
    void reset(_T* object) noexcept {
        safe_release();
        m_object = object; // ここではカウンタを増やさない
    }

    void swap( RefCountedPtr& other ) noexcept {
        _T* temp = m_object;
        m_object = other.m_object;
        other.m_object = temp;
    }

    // copy
    RefCountedPtr& operator = (const RefCountedPtr& rhs) noexcept {
        if (m_object != rhs.m_object)
            RefCountedPtr(rhs).swap(*this); // 先に加算する
        return *this;
    }

    // 基底クラスへの代入
    template <typename _Derived>
    RefCountedPtr& operator =(const RefCountedPtr<_Derived>& other) noexcept {
        // friend がないとエラー:
        // error: ‘D* RefCountedPtr<D>::m_object’ is private within this context
        if (m_object != other.m_object)
            RefCountedPtr(other).swap(*this);
        return *this;
    }

    // move
    RefCountedPtr& operator = (RefCountedPtr&& rhs) noexcept {
        if (m_object != rhs.m_object) {
            safe_release();
            m_object = rhs.m_object; rhs.m_object = nullptr;
        }
        return *this;
    }


    bool operator == (const RefCountedPtr& rhs) const {
        return m_object == rhs.m_object;
    }

    bool operator != (const RefCountedPtr& rhs) const {
        return m_object != rhs.m_object;
    }

    // @note `explicit` を忘れずに!
    explicit operator bool () const noexcept {
        return m_object != nullptr;
    }
/* operator bool があればよい
    bool operator !() const noexcept {
        return !m_object;
    }
*/
    _T& operator *() const {
        return *ptr();
    }

    _T* operator ->() const noexcept {
        ASSERT(m_object, "must not nullptr");
        return m_object;
    }

    _T* ptr() const noexcept {
        return m_object;
    }

    _T* detach() noexcept {
        _T* temp = m_object; m_object = nullptr;
        return temp;
    }

protected:
    // 相互参照のときに二重解放しない
    void safe_release() noexcept {
#ifdef _WIN32
        _T* temp = static_cast<_T*>(InterlockedExchangePointer(&m_object, nullptr));
#else
        _T* temp = static_cast<_T*>(__sync_lock_test_and_set(&m_object, nullptr));
#endif
        if (temp)
            temp->release();
    }

    template<typename _Tp1> friend class RefCountedPtr;

private:
    // 差し替えがある
    _T* m_object ;
};

} // namespace my

#endif // REFCOUNTED_H
