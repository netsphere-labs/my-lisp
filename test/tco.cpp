
#include <iostream>
using namespace std;

// -S オプションでアセンブラ出力

bool isodd(int n) ;

// 偶数のとき true
bool iseven(int n) {
    return n > 0 ? isodd(n - 1) : true;
}

// 奇数のとき true
bool isodd(int n) {
    return n > 0 ? iseven(n - 1) : false;
}

int main()
{
    // オプションなし = segfault, -O3  => 結果 1 OK
    cout << "iseven(1'000'000) = " << iseven(1'000'000) << "\n";

    // ちょっとだけ間があく = 計算したうえで, 結果 1 OK
    cout << "isodd(2'000'000'001) = " << isodd(2'000'000'001) << "\n";
    return 0;
}
