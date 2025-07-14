#!/bin/bash

echo "=== LSP修复后测试脚本 ==="
echo ""

cd /home/fish/code/cpp/hello_cpp

echo "1. 检查项目目录:"
pwd
echo ""

echo "2. 检查compile_commands.json:"
ls -la compile_commands.json
echo ""

echo "3. 检查compile_commands.json内容:"
cat compile_commands.json
echo ""

echo "4. 杀死现有的clangd进程:"
pkill -f clangd
sleep 1
echo ""

echo "5. 启动Emacs测试LSP (后台运行5秒):"
timeout 10 emacs main.cpp --eval "(progn (lsp) (sit-for 5) (message \"LSP Status: %s\" (if (lsp-workspaces) \"连接成功\" \"连接失败\")) (save-buffers-kill-emacs))" &
EMACS_PID=$!

echo "等待Emacs启动和LSP连接..."
sleep 3

echo ""
echo "6. 检查新的clangd进程:"
ps aux | grep clangd | grep -v grep
echo ""

wait $EMACS_PID

echo "7. 测试完成"
echo ""
echo "如果看到clangd进程启动且工作目录是 /home/fish/code/cpp/hello_cpp，则修复成功"
echo ""
echo "手动测试步骤:"
echo "cd /home/fish/code/cpp/hello_cpp"
echo "emacs main.cpp"
echo "然后检查是否自动启动了LSP，或者手动执行: M-x lsp"
