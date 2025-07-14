#!/bin/bash
# 完整的LSP clangd测试脚本

echo "=== 完整的 LSP clangd 测试 ==="
echo

echo "1. 清理环境..."
pkill clangd 2>/dev/null
rm -f /home/fish/.emacs.d/.lsp-session*
sleep 1

echo "2. 测试clangd直接连接..."
cd /home/fish/.emacs.d/test_cpp_project
echo "测试文件位置: $(pwd)/main.cpp"
echo "编译数据库: $(ls -la compile_commands.json 2>/dev/null || echo '不存在')"

echo
echo "3. 启动Emacs并测试LSP..."
cd /home/fish/.emacs.d
emacs --batch \
  --eval "(progn 
    (package-initialize)
    (add-to-list 'load-path \"./config\")
    (require 'jpackage)
    (require 'jconfig)
    (setq lsp-log-io nil)
    (setq lsp-print-performance nil)
    (find-file \"test_cpp_project/main.cpp\")
    (c++-mode)
    (message \"Current directory: %s\" default-directory)
    (message \"File path: %s\" (buffer-file-name))
    (lsp)
    (sleep-for 3)
    (when (lsp-workspaces)
      (message \"LSP workspace: %s\" (lsp--workspace-root (car (lsp-workspaces))))
      (message \"LSP server: %s\" (lsp--workspace-server-id (car (lsp-workspaces)))))
    (message \"LSP status: %s\" (if (lsp-workspaces) \"Connected\" \"Not connected\")))" 2>&1

echo
echo "4. 检查进程状态..."
echo "clangd进程:"
ps aux | grep clangd | grep -v grep

echo
echo "=== 测试完成 ==="
