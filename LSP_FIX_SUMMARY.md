# 解决 Emacs 在 C++ 项目中不启动 clangd 的问题

## 问题分析
在 `/home/fish/code/cpp/hello_cpp` 项目中使用 `emacs main.cpp` 打开文件时没有自动启动 clangd LSP 服务器。

## 根本原因
1. **缺少 C++ 模式 Hook**: lsp-mode 配置中没有为 C/C++ 模式添加自动启动 LSP 的 hook
2. **配置不完整**: jpackage.el 中的 lsp-mode use-package 配置缺少 C++ 相关的 hook

## 已修复的问题

### 1. 修复 jpackage.el 中的 lsp-mode 配置
在 `/home/fish/.emacs.d/config/jpackage.el` 中，已将：
```elisp
:hook (
   (lsp-mode . lsp-enable-which-key-integration)
   (lsp-mode . flycheck-mode)
   (java-mode . lsp-deferred)
   (lsp-mode . lsp-lens-mode)
   (java-mode-hook lsp-java-boot-lens-mode)
   (sh-mode . lsp)
   )
```
修改为：
```elisp
:hook (
   (lsp-mode . lsp-enable-which-key-integration)
   (lsp-mode . flycheck-mode)
   (c-mode . lsp-deferred)        ; 新增
   (c++-mode . lsp-deferred)      ; 新增
   (java-mode . lsp-deferred)
   (lsp-mode . lsp-lens-mode)
   (java-mode-hook lsp-java-boot-lens-mode)
   (sh-mode . lsp)
   )
```

### 2. 修复 jconfig.el 中的 C++ hook 配置
在 `/home/fish/.emacs.d/config/jconfig.el` 中，已将：
```elisp
(add-hook 'c++-mode-hook #'j-cc-mode-hook-func)
(add-hook 'c++-mode-hook #'j-cc-fontify-constants-h)
(add-hook 'c-mode-hook #'j-cc-mode-hook-func)
(add-hook 'c-mode-hook #'j-cc-fontify-constants-h)
```
修改为：
```elisp
(add-hook 'c++-mode-hook #'j-cc-mode-hook-func)
(add-hook 'c++-mode-hook #'j-cc-fontify-constants-h)
(add-hook 'c++-mode-hook #'lsp-deferred)      ; 新增
(add-hook 'c-mode-hook #'j-cc-mode-hook-func)
(add-hook 'c-mode-hook #'j-cc-fontify-constants-h)
(add-hook 'c-mode-hook #'lsp-deferred)        ; 新增
```

## 测试方法

### 方法1: 自动测试
```bash
cd /home/fish/code/cpp/hello_cpp
emacs main.cpp
```
现在应该会自动启动 LSP。

### 方法2: 手动启动 LSP
如果自动启动失败，可以在 Emacs 中手动执行：
```
M-x lsp
```

### 方法3: 验证 LSP 状态
在 Emacs 中执行以下命令检查 LSP 状态：
```
M-x lsp-describe-session
```

## 预期结果
1. 打开 C++ 文件时应该自动启动 clangd
2. 在底部 modeline 应该显示 LSP 连接状态
3. 代码补全、语法检查、跳转定义等功能应该正常工作
4. `M-.` (goto-definition) 应该能跳转到函数定义

## 故障排除

### 如果仍然不工作：
1. 重启 Emacs 确保配置生效
2. 检查 `*Messages*` 缓冲区是否有错误信息
3. 检查 `*lsp-log*` 缓冲区查看 LSP 通信日志
4. 确保项目根目录有 `compile_commands.json` 文件

### 调试命令：
```elisp
;; 在 Emacs 中执行以下命令进行调试
(setq lsp-log-io t)               ; 启用 LSP 日志
(lsp-workspace-show-log)          ; 显示 LSP 日志
(lsp-describe-session)            ; 显示 LSP 会话信息
```

## 总结
通过添加 `(c-mode . lsp-deferred)` 和 `(c++-mode . lsp-deferred)` 到 lsp-mode 的 hook 配置中，现在 Emacs 应该在打开 C/C++ 文件时自动启动 clangd LSP 服务器。
