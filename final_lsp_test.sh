#!/bin/bash
# 最终验证LSP功能的自动化测试

PROJECT_DIR="/home/fish/code/cpp/hello_cpp"
EMACS_DIR="/home/fish/.emacs.d"

echo "=== 最终 LSP 功能验证 ==="
echo

echo "1. 清理环境..."
pkill clangd 2>/dev/null
rm -f "$EMACS_DIR/.lsp-session"*
sleep 1

echo "2. 验证项目状态..."
cd "$PROJECT_DIR"
echo "项目目录: $(pwd)"
echo "编译数据库: $(ls -la compile_commands.json)"
echo "源文件完整性:"
echo "  main.cpp: $(wc -l < main.cpp) 行"
echo "  func.h: $(wc -l < func.h) 行" 
echo "  func.cpp: $(wc -l < func.cpp) 行"

echo
echo "3. 测试编译状态..."
cd build && make clean && make
if [ $? -eq 0 ]; then
    echo "✓ 项目编译成功"
else
    echo "✗ 项目编译失败"
    exit 1
fi
cd ..

echo
echo "4. 运行自动化LSP测试..."
cd "$EMACS_DIR"
timeout 30s emacs --batch \
  --eval "(progn 
    (package-initialize)
    (add-to-list 'load-path \"./config\")
    (require 'jpackage)
    (require 'jconfig)
    (find-file \"$PROJECT_DIR/main.cpp\")
    (c++-mode)
    (lsp)
    (sleep-for 8)  ; 等待LSP完全初始化
    (if (lsp-workspaces)
        (progn
          (message \"=== LSP 测试结果 ===\")
          (message \"✓ LSP服务器: %s\" (lsp--workspace-server-id (car (lsp-workspaces))))
          (message \"✓ 工作区: %s\" (lsp--workspace-root (car (lsp-workspaces))))
          ;; 测试语义功能
          (goto-char (point-min))
          (if (search-forward \"add(\" nil t)
              (progn
                (backward-char 1)
                (message \"✓ 找到函数调用位置: line %d, col %d\" 
                        (line-number-at-pos) (current-column))
                ;; 尝试获取定义位置
                (condition-case err
                    (let ((locations (lsp-request \"textDocument/definition\"
                                                 (lsp--text-document-position-params))))
                      (if locations
                          (message \"✓ 函数定义查找成功: %s\" locations)
                        (message \"✗ 未找到函数定义\")))
                  (error (message \"✗ 定义查找出错: %s\" err))))
            (message \"✗ 未找到add函数调用\"))
          ;; 测试符号信息
          (condition-case err
              (let ((symbols (lsp-request \"textDocument/documentSymbol\" 
                                        (lsp--text-document-identifier))))
                (if symbols
                    (message \"✓ 文档符号获取成功: %d个符号\" (length symbols))
                  (message \"✗ 未获取到文档符号\")))
            (error (message \"✗ 符号获取出错: %s\" err))))
      (message \"✗ LSP连接失败\"))
    (message \"=== 测试完成 ===\"))" 2>&1

echo
echo "5. 检查LSP进程..."
ps aux | grep clangd | grep -v grep

echo
echo "=== 结论和建议 ==="
echo "如果看到'✓ 函数定义查找成功'消息，说明LSP功能正常。"
echo "如果仍有问题，请手动测试:"
echo "  1. cd $PROJECT_DIR"
echo "  2. emacs main.cpp"
echo "  3. 等待LSP连接（状态栏会显示）"
echo "  4. 将光标放在第11行的'add'上，按M-."
echo "  5. 应该跳转到func.cpp中的add函数定义"
echo
echo "常见问题解决:"
echo "  - 如果LSP不启动：重启Emacs"
echo "  - 如果跳转不工作：确保compile_commands.json存在且有效"
echo "  - 如果性能差：等待clangd完成索引（可能需要几分钟）"
