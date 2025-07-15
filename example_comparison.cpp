#include <iostream>

#define DEBUG 1
#define MY_MACRO(x) std::cout << x

int main() {
    MY_MACRO("Hello");
    
#if DEBUG
    std::cout << "Debug mode" << std::endl;  // 这行会正常显示
#else
    std::cout << "Release mode" << std::endl; // 这行应该置灰
#endif

#if 0
    // 整个代码块应该置灰
    std::cout << "Disabled code" << std::endl;
    int unused = 42;
#endif

    return 0;
}
