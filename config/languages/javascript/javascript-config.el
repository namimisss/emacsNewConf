;;; javascript-config.el --- JavaScript语言配置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: languages, javascript, node
;; Version: 1.0.0

;;; Commentary:

;; JavaScript 开发配置，包括：
;; - Tern 语言服务器支持
;; - 基础编辑功能
;; - 格式化支持

;;; Code:

;; =============================================================================
;; JavaScript 基础配置
;; =============================================================================

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :hook ((js2-mode . lsp-deferred))  ; 启用 LSP
  :config
  (setq js2-basic-offset 2
        js2-bounce-indent-p nil
        js2-highlight-level 2
        js2-mode-show-parse-errors nil  ; 关闭js2的错误显示，使用LSP
        js2-mode-show-strict-warnings nil)
  
  ;; 键绑定
  (define-key js2-mode-map (kbd "M-.") 'lsp-find-definition)
  (define-key js2-mode-map (kbd "M-,") 'lsp-find-references)
  (define-key js2-mode-map (kbd "M-?") 'lsp-find-references)
  (define-key js2-mode-map (kbd "C-c C-r") 'lsp-rename))

;; =============================================================================
;; JavaScript 重构工具
;; =============================================================================

(use-package js2-refactor
  :ensure t
  :hook (js2-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

;; =============================================================================
;; TypeScript 支持
;; =============================================================================

(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; =============================================================================
;; JSON 配置
;; =============================================================================

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config
  (setq json-reformat:indent-width 2))

;; =============================================================================
;; Web 模式 (HTML/CSS/JS混合)
;; =============================================================================

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.htm\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         ("\\.tsx\\'" . web-mode))  ; 移除 .jsx，让 js2-jsx-mode 处理
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t))

;; =============================================================================
;; 代码格式化和 Lint
;; =============================================================================

;; Prettier - JavaScript 代码格式化
(use-package prettier-js
  :ensure t
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (web-mode . prettier-js-mode))
  :config
  (setq prettier-js-args '("--single-quote" 
                          "--trailing-comma" "es5"
                          "--semi" "true"
                          "--tab-width" "2"
                          "--print-width" "100"))
  
  ;; 保存时自动格式化
  (defun my-js-format-on-save ()
    "JavaScript 文件保存时自动使用 Prettier 格式化"
    (when (and (derived-mode-p 'js2-mode 'typescript-mode)
               (executable-find "prettier"))
      (prettier-js)))
  
  (add-hook 'before-save-hook 'my-js-format-on-save))

;; ESLint 集成配置
;; 注意：flycheck 已在 tools-flycheck.el 中全局配置，这里只添加 JavaScript 特定设置

;; JavaScript 特定的 flycheck 配置
(with-eval-after-load 'flycheck
  ;; 禁用一些不需要的 JavaScript 检查器
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint json-jsonlist)))
  
  ;; 优先使用项目本地的 ESLint
  (defun my-use-eslint-from-node-modules ()
    "使用项目本地的 ESLint"
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/.bin/eslint"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  
  (add-hook 'flycheck-mode-hook #'my-use-eslint-from-node-modules))

;; ESLint 自动修复功能
(defun my-eslint-fix-buffer ()
  "使用 ESLint 自动修复当前缓冲区"
  (interactive)
  (if (executable-find "eslint")
      (progn
        (call-process "eslint" nil nil nil "--fix" (buffer-file-name))
        (revert-buffer t t t))
    (message "ESLint not found")))

;; 在 js2-mode 中绑定快捷键
(with-eval-after-load 'js2-mode
  (define-key js2-mode-map (kbd "C-c C-f") 'my-eslint-fix-buffer))

;; 自动修复和格式化组合
(defun my-js-format-and-lint ()
  "格式化并修复 JavaScript 代码"
  (interactive)
  (when (derived-mode-p 'js2-mode 'typescript-mode)
    (if (executable-find "eslint")
        (my-eslint-fix-buffer)
      (prettier-js))))

;; 绑定组合命令
(global-set-key (kbd "C-c f j") 'my-js-format-and-lint)

;; =============================================================================
;; Node.js 支持
;; =============================================================================

(use-package add-node-modules-path
  :ensure t
  :hook ((js2-mode . add-node-modules-path)
         (typescript-mode . add-node-modules-path)))

;; =============================================================================
;; NPM 集成
;; =============================================================================

(use-package npm-mode
  :ensure t
  :hook ((js2-mode . npm-mode)
         (typescript-mode . npm-mode)))


(provide 'javascript-config)

;;; javascript-config.el ends here
