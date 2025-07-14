#include <iostream>
#include <vector>

class TestClass {
public:
    void testMethod() {
        std::cout << "Testing auto bracket closing and formatting" << std::endl;
        
        // 测试括号自动闭合
        if (true) {
            std::vector<int> numbers = {1, 2, 3};
            for (auto num : numbers) {
                std::cout << num << " ";
            }
        }
    }
};

int main() {
    TestClass test;
    test.testMethod();
    return 0;
}
