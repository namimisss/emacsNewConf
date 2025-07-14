#!/bin/bash
# 测试LSP连接脚本

echo "=== LSP 连接测试 ==="
echo

echo "1. 停止所有现有的clangd进程..."
pkill clangd 2>/dev/null
sleep 1

echo "2. 启动Emacs并测试LSP连接..."
emacs --batch \
  --eval "(progn 
    (package-initialize)
    (add-to-list 'load-path \"./config\")
    (require 'jpackage)
    (require 'jconfig)
    (setq lsp-log-io t)
    (setq lsp-print-performance t)
    (find-file \"test_cpp_project/main.cpp\")
    (c++-mode)
    (lsp)
    (sleep-for 5)
    (message \"LSP test completed\"))" 2>&1

echo
echo "3. 检查是否有clangd进程启动..."
ps aux | grep clangd | grep -v grep

echo
echo "=== 测试完成 ==="
