#!/bin/bash
# 在真实C++项目中测试LSP功能

PROJECT_DIR="/home/fish/code/cpp/hello_cpp"
EMACS_DIR="/home/fish/.emacs.d"

echo "=== 真实 C++ 项目 LSP 测试 ==="
echo "项目目录: $PROJECT_DIR"
echo

echo "1. 检查项目状态..."
cd "$PROJECT_DIR"
echo "当前目录: $(pwd)"
echo "CMakeLists.txt: $(ls -la CMakeLists.txt 2>/dev/null || echo '不存在')"
echo "编译数据库: $(ls -la compile_commands.json 2>/dev/null || echo '不存在')"
echo "源文件: $(ls -la *.cpp *.h 2>/dev/null)"

echo
echo "2. 清理旧的LSP环境..."
# 清理ccls缓存
rm -rf .ccls-cache
# 清理任何LSP会话文件
rm -f .lsp-session*
# 停止现有的clangd进程
pkill clangd 2>/dev/null
sleep 1

echo
echo "3. 测试clangd对项目的支持..."
if [ -f "compile_commands.json" ]; then
    echo "编译数据库存在，测试clangd..."
    timeout 10s clangd --check=main.cpp 2>/dev/null && echo "✓ clangd可以处理项目" || echo "✗ clangd处理项目失败"
else
    echo "⚠ 编译数据库不存在，需要重新生成"
fi

echo
echo "4. 启动Emacs并测试LSP功能..."
cd "$EMACS_DIR"
emacs --batch \
  --eval "(progn 
    (package-initialize)
    (add-to-list 'load-path \"./config\")
    (require 'jpackage)
    (require 'jconfig)
    (setq lsp-log-io t)
    (message \"Emacs配置加载完成\")
    (find-file \"$PROJECT_DIR/main.cpp\")
    (c++-mode)
    (message \"打开文件: %s\" (buffer-file-name))
    (message \"当前模式: %s\" major-mode)
    (lsp)
    (sleep-for 5)
    (if (lsp-workspaces)
        (progn
          (message \"✓ LSP连接成功\")
          (message \"工作区: %s\" (lsp--workspace-root (car (lsp-workspaces))))
          (message \"服务器: %s\" (lsp--workspace-server-id (car (lsp-workspaces))))
          ;; 测试跳转到定义功能
          (goto-char (point-min))
          (search-forward \"add(\" nil t)
          (backward-char 1)
          (message \"测试位置: %s\" (point))
          (condition-case err
              (progn
                (lsp-find-definition)
                (message \"✓ 函数定义跳转测试成功\"))
            (error (message \"✗ 函数定义跳转失败: %s\" err))))
        (message \"✗ LSP连接失败\")))" 2>&1

echo
echo "5. 检查LSP进程状态..."
echo "clangd进程:"
ps aux | grep clangd | grep -v grep || echo "无clangd进程运行"

echo
echo "6. 生成编译数据库（如果需要）..."
cd "$PROJECT_DIR"
if [ ! -f "compile_commands.json" ] || [ ! -s "compile_commands.json" ]; then
    echo "重新生成编译数据库..."
    mkdir -p build
    cd build
    cmake .. -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
    cd ..
    ln -sf build/compile_commands.json .
    echo "编译数据库已重新生成"
fi

echo
echo "=== 建议的手动测试步骤 ==="
echo "1. cd $PROJECT_DIR"
echo "2. emacs main.cpp"
echo "3. 将光标放在 add(a, b) 的 'add' 上"
echo "4. 按 M-. (Meta+点) 应该跳转到 func.cpp 中的定义"
echo "5. 按 M-, (Meta+逗号) 返回原位置"
echo
echo "=== 测试完成 ==="
