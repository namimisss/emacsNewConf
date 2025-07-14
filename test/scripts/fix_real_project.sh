#!/bin/bash
# 手动测试C++项目的LSP功能

PROJECT_DIR="/home/fish/code/cpp/hello_cpp"

echo "=== 手动LSP功能验证 ==="
echo

echo "1. 检查main.cpp中的问题..."
cd "$PROJECT_DIR"

# 检查main.cpp中缺少的头文件
echo "main.cpp内容:"
cat main.cpp

echo
echo "2. 发现问题: main.cpp缺少<map>和<string>头文件"
echo "   第7行使用了std::map但没有包含相应头文件"

echo
echo "3. 修复main.cpp..."
cat > main.cpp << 'EOF'
#include <iostream>
#include <map>
#include <string>
#include "func.h"

int main()
{
	std::cout << "hello" << std::endl;
	std::map<std::string, std::string> sm;
	int a = 10;
	int b = 20;
	int c = add(a, b);
	std::cout << "c:" << c << std::endl;

	return 0;
}
EOF

echo "修复后的main.cpp:"
cat main.cpp

echo
echo "4. 重新生成编译数据库..."
cd build
cmake .. -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
cd ..

echo
echo "5. 测试编译..."
if cd build && make; then
    echo "✓ 编译成功"
    cd ..
else
    echo "✗ 编译失败"
    cd ..
fi

echo
echo "6. 创建一个测试脚本用于Emacs..."
cat > test_lsp_manually.el << 'EOF'
;; 手动测试LSP功能的脚本
(package-initialize)
(add-to-list 'load-path "/home/fish/.emacs.d/config")
(require 'jpackage)
(require 'jconfig)

;; 打开项目文件
(find-file "/home/fish/code/cpp/hello_cpp/main.cpp")
(c++-mode)

;; 启动LSP
(lsp)

;; 提示用户如何测试
(message "LSP已启动。测试步骤:")
(message "1. 将光标放在第11行的 'add' 函数名上")
(message "2. 按 M-. 跳转到定义")
(message "3. 按 M-, 返回")
(message "4. 按 M-? 查找引用")
EOF

echo "测试脚本已创建: $PROJECT_DIR/test_lsp_manually.el"

echo
echo "=== 手动测试步骤 ==="
echo "运行以下命令进行手动测试:"
echo "cd $PROJECT_DIR"
echo "emacs -l test_lsp_manually.el"
echo
echo "然后在Emacs中:"
echo "1. 等待LSP连接完成（状态栏显示LSP信息）"
echo "2. 将光标放在 'add(a, b)' 中的 'add' 上"
echo "3. 按 M-. (Alt+.) 跳转到函数定义"
echo "4. 应该跳转到 func.cpp 文件中的 add 函数"
echo "5. 按 M-, (Alt+,) 返回原位置"
echo "6. 按 M-? (Alt+?) 查看函数引用"
echo
echo "如果以上功能正常工作，说明LSP配置正确。"
