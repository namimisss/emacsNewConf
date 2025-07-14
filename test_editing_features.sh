#!/bin/bash
# test_editing_features.sh - 测试编辑功能

echo "=== 编辑功能测试脚本 ==="
echo ""

echo "1. 检查配置状态..."
cd /home/fish/.emacs.d
emacs --batch --eval "(progn (add-to-list 'load-path (expand-file-name \"config\" user-emacs-directory)) (require 'main-config) (message \"✓ Electric pair mode: %s\" electric-pair-mode) (message \"✓ Show paren mode: %s\" show-paren-mode) (message \"✓ Smartparens global mode: %s\" (bound-and-true-p smartparens-global-mode)) (message \"✓ Delete selection mode: %s\" delete-selection-mode) (message \"✓ Line numbers mode: %s\" global-display-line-numbers-mode))" 2>/dev/null

echo ""
echo "2. 检查格式化工具..."
if command -v clang-format &> /dev/null; then
    echo "✓ clang-format已安装: $(clang-format --version | head -1)"
else
    echo "! clang-format未安装，C++格式化可能不工作"
fi

if command -v black &> /dev/null; then
    echo "✓ black已安装 (Python格式化)"
else
    echo "! black未安装，Python格式化可能不工作"
fi

echo ""
echo "=== 预期功能 ==="
echo ""
echo "🔧 自动括号配对:"
echo "   - 输入 ( 自动补全 )"
echo "   - 输入 { 自动补全 }"
echo "   - 输入 [ 自动补全 ]"
echo "   - 输入 \" 自动补全 \""
echo ""
echo "💾 保存时格式化:"
echo "   - C/C++文件保存时自动运行clang-format"
echo "   - Python文件保存时自动运行black或autopep8"
echo "   - Java文件保存时自动格式化"
echo ""
echo "📋 其他编辑功能:"
echo "   - 显示行号和列号"
echo "   - 高亮匹配的括号"
echo "   - 选中文本后输入会替换"
echo "   - 平滑滚动"
echo ""
echo "🎯 快速测试:"
echo "1. 打开C++文件: emacs -nw main.cpp"
echo "2. 输入 'if(' 应该自动补全为 'if()'"
echo "3. 保存文件应该自动格式化代码"
echo ""
echo "如果功能不正常，请检查相应的格式化工具是否安装。"
