#!/bin/bash
# 简单的手动测试指南

PROJECT_DIR="/home/fish/code/cpp/hello_cpp"

echo "=== C++ LSP 功能手动验证指南 ==="
echo
echo "您的项目已经准备就绪！"
echo "项目位置: $PROJECT_DIR"
echo
echo "### 步骤1: 启动Emacs"
echo "cd $PROJECT_DIR"
echo "emacs main.cpp"
echo
echo "### 步骤2: 等待LSP连接"
echo "- 文件打开后，等待3-5秒"
echo "- 状态栏应该显示 'LSP[clangd:...]' 或类似信息"
echo "- 如果看到项目选择提示，选择 '.' (当前目录)"
echo
echo "### 步骤3: 测试函数定义跳转"
echo "1. 找到第11行: int c = add(a, b);"
echo "2. 将光标放在 'add' 函数名上"
echo "3. 按 M-. (Alt+点号)"
echo "4. 应该跳转到 func.cpp 文件中的 add 函数定义"
echo "5. 按 M-, (Alt+逗号) 返回原位置"
echo
echo "### 步骤4: 测试查找引用"
echo "1. 将光标放在 'add' 函数名上"
echo "2. 按 M-? (Alt+问号)"
echo "3. 应该显示所有使用 add 函数的地方"
echo
echo "### 步骤5: 测试其他LSP功能"
echo "- C-c l: 打开LSP命令菜单"
echo "- C-c l r r: 重命名符号"
echo "- C-c l g g: 查找定义"
echo "- C-c l g r: 查找引用"
echo
echo "### 预期结果"
echo "✓ 函数跳转工作 -> LSP定义查找正常"
echo "✓ 引用查找工作 -> LSP引用分析正常"
echo "✓ 重命名工作 -> LSP重构功能正常"
echo
echo "### 如果遇到问题"
echo "1. 重启Emacs"
echo "2. 检查状态栏是否显示LSP连接信息"
echo "3. 使用 C-c l w r 重启LSP服务器"
echo "4. 检查 *lsp-log* buffer中的错误信息"
echo
echo "### 当前项目状态"
cd "$PROJECT_DIR"
echo "- 编译数据库: $(test -f compile_commands.json && echo '✓ 存在' || echo '✗ 缺失')"
echo "- 项目可编译: $(cd build && make clean && make >/dev/null 2>&1 && echo '✓ 是' || echo '✗ 否')"
echo "- main.cpp: $(test -f main.cpp && echo "✓ $(wc -l < main.cpp) 行" || echo '✗ 缺失')"
echo "- func.h: $(test -f func.h && echo "✓ $(wc -l < func.h) 行" || echo '✗ 缺失')"
echo "- func.cpp: $(test -f func.cpp && echo "✓ $(wc -l < func.cpp) 行" || echo '✗ 缺失')"
echo
echo "您的环境已经配置完成，请按照上述步骤手动验证LSP功能！"
