#!/bin/bash

echo "=== 精确的LSP测试 ==="
echo ""

# 确保在正确的目录
cd /home/fish/code/cpp/hello_cpp

echo "当前目录: $(pwd)"
echo ""

# 杀死所有clangd进程
echo "1. 杀死所有clangd进程:"
sudo pkill -f clangd
sleep 2

echo "2. 确认没有clangd进程:"
ps aux | grep clangd | grep -v grep || echo "没有clangd进程"
echo ""

# 创建专门的测试emacs脚本
cat > /tmp/test_emacs_lsp.el << 'EOF'
;; 加载配置
(setq inhibit-startup-message t)
(add-to-list 'load-path "~/.emacs.d/elpa")
(require 'package)
(package-initialize)

;; 加载自定义配置
(load-file "~/.emacs.d/config/jpackage.el")
(load-file "~/.emacs.d/config/jconfig.el")

;; 启用调试
(setq lsp-log-io t)

;; 打开C++文件
(find-file "/home/fish/code/cpp/hello_cpp/main.cpp")

;; 显示模式
(message "Current major mode: %s" major-mode)

;; 手动触发LSP (因为有些时候hook可能不工作)
(when (fboundp 'lsp-deferred)
  (lsp-deferred))

;; 等待一会儿
(sit-for 3)

;; 检查LSP状态
(if (bound-and-true-p lsp-mode)
    (message "LSP mode is active!")
    (message "LSP mode is NOT active"))

;; 检查工作区
(when (fboundp 'lsp-workspaces)
  (let ((workspaces (lsp-workspaces)))
    (if workspaces
        (message "LSP workspaces: %s" workspaces)
        (message "No LSP workspaces found"))))

;; 保持运行一会儿然后退出
(run-with-timer 5 nil (lambda () (save-buffers-kill-emacs)))
EOF

echo "3. 使用专门的配置启动Emacs:"
emacs -Q -l /tmp/test_emacs_lsp.el &
EMACS_PID=$!

echo "等待Emacs启动LSP..."
sleep 8

echo ""
echo "4. 检查Emacs启动的clangd进程:"
ps aux | grep clangd | grep -v grep
echo ""

echo "5. 检查进程树:"
pstree -p | grep -A2 -B2 clangd || echo "没有找到clangd进程树"
echo ""

wait $EMACS_PID 2>/dev/null

echo "测试完成"
