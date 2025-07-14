#include <iostream>
#include <vector>
#include <memory>

// 测试clangd的语法检查和代码补全

class TestClass {
private:
    std::vector<int> data;
    std::unique_ptr<int> ptr;
    
public:
    TestClass() : ptr(std::make_unique<int>(42)) {}
    
    void addData(int value) {
        data.push_back(value);
    }
    
    // 故意添加一些问题来测试clangd
    void problematicFunction() {
        int unused_variable = 10;  // 未使用变量
        
        // 潜在的内存问题
        int* raw_ptr = new int(20);
        // 忘记delete raw_ptr; - 内存泄漏
        
        // 使用可能为空的指针
        int* maybe_null = nullptr;
        // *maybe_null = 5; // 解除注释会导致段错误
    }
    
    auto getData() const -> const std::vector<int>& {
        return data;
    }
};

int main() {
    TestClass test;
    test.addData(1);
    test.addData(2);
    test.addData(3);
    
    // 使用现代C++特性
    auto data = test.getData();
    for (const auto& item : data) {
        std::cout << item << " ";
    }
    std::cout << std::endl;
    
    return 0;
}
