
#include "../ref_counted.h"
#include <iostream>

class C : public my::RefCounted
{
public:
    C(int x): val(x) { }
    int val;
};

class D : public C
{
public:
    D(int x): C(x) { }
};

int main()
{
    my::RefCountedPtr<C> p(new C(10));
    my::RefCountedPtr<D> q(new D(20));

    p = q;
    int& v = (*p).val;
    std::cout << v << std::endl;

    return 0;
}
