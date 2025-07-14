;; flycheck 替换 flymake 配置总结
;; 
;; 1. 基础flycheck配置 (在 jpackage.el):
;;    - global-flycheck-mode 全局启用
;;    - flycheck-checker-error-threshold 错误阈值
;;    - flycheck-check-syntax-automatically 自动检查配置
;;    - flycheck-idle-change-delay 延迟设置
;;
;; 2. LSP集成 (在 jpackage.el):
;;    - lsp-prefer-flymake nil (禁用flymake)
;;    - lsp-diagnostics-provider :flycheck (使用flycheck)
;;    - lsp-mode hook 中添加 flycheck-mode
;;
;; 3. lsp-ui集成 (在 jpackage.el):
;;    - lsp-ui-flycheck-enable t
;;    - lsp-ui-flycheck-list-position 'right
;;    - lsp-ui-flycheck-live-reporting t
;;
;; 4. Python/elpy集成 (在 jpackage.el):
;;    - elpy-mode hook 中添加 flycheck-mode
;;    - 从elpy-modules中删除flymake模块
;;
;; 5. 键绑定 (在 jconfig.el):
;;    - C-c f l: flycheck-list-errors
;;    - C-c f n: flycheck-next-error  
;;    - C-c f p: flycheck-previous-error
;;    - C-c f v: flycheck-verify-setup
;;
;; 6. 相关包:
;;    - flycheck: 主包
;;    - consult-flycheck: 与consult集成
;;    - flycheck-pos-tip: 错误提示
;;    - flycheck-popup-tip: 弹出提示
;;
;; 7. 语言支持:
;;    - C/C++: 通过ccls + lsp-mode + flycheck
;;    - Python: 通过elpy + flycheck
;;    - JavaScript: 通过lsp-mode + flycheck
;;    - Shell: 通过lsp-mode + flycheck
;;    - Java: 通过lsp-java + flycheck
