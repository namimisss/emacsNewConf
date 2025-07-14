#!/bin/bash
# verify_lsp_refs.sh - 验证LSP引用信息显示

echo "=== LSP引用信息验证脚本 ==="
echo ""

echo "1. 检查当前配置状态..."
cd /home/fish/.emacs.d
emacs --batch --eval "(progn (add-to-list 'load-path (expand-file-name \"config\" user-emacs-directory)) (require 'main-config) (message \"✓ LSP lens启用: %s\" lsp-lens-enable) (message \"✓ LSP sideline启用: %s\" lsp-ui-sideline-enable))" 2>/dev/null

echo ""
echo "2. 检查clangd是否可用..."
if command -v clangd &> /dev/null; then
    echo "✓ clangd已安装: $(clangd --version | head -1)"
else
    echo "✗ clangd未安装"
fi

echo ""
echo "3. 检查C++项目结构..."
cd /home/fish/code/cpp/hello_cpp
if [ -f "main.cpp" ]; then
    echo "✓ main.cpp存在"
    if [ -f "func.h" ]; then
        echo "✓ func.h存在"
    else
        echo "! func.h缺失，可能影响引用显示"
    fi
else
    echo "✗ main.cpp缺失"
fi

echo ""
echo "=== 预期行为 ==="
echo "当使用 'emacs -nw main.cpp' 打开文件时，你应该看到："
echo "1. LSP连接信息: 'LSP :: Connected to [clangd:...]'"
echo "2. 函数上方显示引用计数 (可能需要几秒钟加载)"
echo "3. 代码行右侧显示类型信息"
echo ""
echo "快捷键:"
echo "- C-c l r : 查找引用"
echo "- C-c l d : 查找定义" 
echo "- C-c l i : 查找实现"
echo ""
echo "如果看不到引用信息，请等待几秒让clangd完全加载。"
