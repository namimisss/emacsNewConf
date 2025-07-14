#!/bin/bash

echo "=== Emacs LSP 诊断脚本 ==="
echo "测试项目: /home/fish/code/cpp/hello_cpp"
echo ""

# 检查当前clangd进程
echo "1. 检查现有clangd进程："
ps aux | grep clangd | grep -v grep
echo ""

# 检查项目文件
echo "2. 检查项目文件："
echo "主文件: /home/fish/code/cpp/hello_cpp/main.cpp"
echo "compile_commands.json: $(ls -la /home/fish/code/cpp/hello_cpp/compile_commands.json 2>/dev/null || echo '不存在')"
echo ""

# 检查emacs配置
echo "3. 检查关键配置："
echo "lsp-mode是否已安装: $(emacs --batch --eval '(progn (add-to-list '\''load-path "~/.emacs.d/elpa") (require '\''package) (package-initialize) (if (package-installed-p '\''lsp-mode) (message "已安装") (message "未安装")))')"
echo ""

# 创建测试emacs脚本
cat > /tmp/test_lsp_startup.el << 'EOF'
;; 加载配置
(load-file "~/.emacs.d/config/jpackage.el")
(load-file "~/.emacs.d/config/jconfig.el")

;; 强制启用debug信息
(setq lsp-log-io t)
(setq lsp-trace t)
(setq lsp-print-performance t)

;; 打开C++文件并尝试启动LSP
(find-file "/home/fish/code/cpp/hello_cpp/main.cpp")

;; 强制启动LSP
(lsp)

;; 等待LSP启动
(sleep-for 3)

;; 检查LSP状态
(message "LSP workspace folders: %s" (lsp-workspace-folders))
(message "LSP sessions: %s" (hash-table-keys lsp--session-folders))

;; 保持Emacs运行以观察
(message "LSP启动测试完成，请检查*lsp-log*缓冲区")
EOF

echo "4. 创建的测试脚本: /tmp/test_lsp_startup.el"
echo ""

echo "5. 手动测试步骤："
echo "执行以下命令来测试LSP启动："
echo "cd /home/fish/code/cpp/hello_cpp"
echo "emacs -Q -l /tmp/test_lsp_startup.el"
echo ""

echo "6. 或者使用以下交互式测试："
echo "cd /home/fish/code/cpp/hello_cpp"
echo "emacs main.cpp"
echo "然后在emacs中执行: M-x lsp"
echo ""

echo "=== 诊断完成 ==="
