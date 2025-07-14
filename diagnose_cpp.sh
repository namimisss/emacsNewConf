#!/bin/bash
# Emacs C++ 项目诊断脚本

echo "=== Emacs C++ 项目诊断 ==="
echo

echo "1. 检查基础工具..."
echo -n "Emacs: "
emacs --version | head -1
echo -n "clangd: "
clangd --version | head -1
echo -n "cmake: "
cmake --version | head -1
echo

echo "2. 检查配置文件语法..."
echo -n "jconfig.el: "
if emacs --batch --eval "(with-temp-buffer (insert-file-contents \"config/jconfig.el\") (emacs-lisp-mode) (check-parens))" 2>/dev/null; then
    echo "✓ 语法正确"
else
    echo "✗ 语法错误"
fi

echo -n "jpackage.el: "
if emacs --batch --eval "(with-temp-buffer (insert-file-contents \"config/jpackage.el\") (emacs-lisp-mode) (check-parens))" 2>/dev/null; then
    echo "✓ 语法正确"
else
    echo "✗ 语法错误"
fi

echo -n "init.el: "
if emacs --batch --eval "(with-temp-buffer (insert-file-contents \"init.el\") (emacs-lisp-mode) (check-parens))" 2>/dev/null; then
    echo "✓ 语法正确"
else
    echo "✗ 语法错误"
fi

echo

echo "3. 测试配置加载..."
echo -n "包配置加载: "
if emacs --batch --eval "(progn (add-to-list 'load-path \"./config\") (package-initialize) (require 'jpackage))" 2>/dev/null; then
    echo "✓ 成功"
else
    echo "✗ 失败"
fi

echo -n "完整配置加载: "
if emacs --batch --eval "(progn (add-to-list 'load-path \"./config\") (package-initialize) (require 'jpackage) (require 'jconfig))" 2>/dev/null; then
    echo "✓ 成功"
else
    echo "✗ 失败"
fi

echo

echo "4. 检查C++环境..."
echo -n "clangd运行状态: "
if pgrep clangd >/dev/null; then
    echo "✓ 正在运行"
else
    echo "✗ 未运行"
fi

echo -n "编译数据库: "
if [ -f "test_cpp_project/compile_commands.json" ]; then
    echo "✓ 存在"
else
    echo "✗ 不存在"
fi

echo

echo "5. 推荐的启动步骤:"
echo "   1. cd /home/fish/.emacs.d"
echo "   2. emacs test_cpp_project/main.cpp"
echo "   3. 等待LSP连接并索引完成"
echo "   4. 使用 C-c l 查看LSP命令"

echo
echo "=== 诊断完成 ==="
