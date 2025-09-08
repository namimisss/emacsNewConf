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

;; 注意：js2-mode 现在仅用于纯 JavaScript，JSX 文件由 rjsx-mode 处理
(use-package js2-mode
  :ensure t
  :mode "\\.mjs\\'"  ; 仅处理 ES6 模块文件
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
  :hook ((js2-mode . js2-refactor-mode)
         (rjsx-mode . js2-refactor-mode))  ; 也为 rjsx-mode 启用重构功能
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

;; =============================================================================
;; TypeScript 支持
;; =============================================================================

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"  ; 只处理 .ts 文件，.tsx 由 web-mode 处理
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
;; React 支持
;; =============================================================================

;; React 代码片段和工具
(use-package rjsx-mode
  :ensure t
  :mode (("\\.jsx\\'" . rjsx-mode)
         ("\\.js\\'" . rjsx-mode))  ; 使用 rjsx-mode 替代 js2-mode 以获得更好的 JSX 支持
  :hook (rjsx-mode . lsp-deferred)
  :config
  (setq js2-basic-offset 2
        js2-bounce-indent-p nil
        js2-highlight-level 2
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)
  
  ;; React 特定的键绑定
  (define-key rjsx-mode-map (kbd "M-.") 'lsp-find-definition)
  (define-key rjsx-mode-map (kbd "M-,") 'lsp-find-references)
  (define-key rjsx-mode-map (kbd "C-c C-r") 'lsp-rename))

;; React 代码片段
(use-package react-snippets
  :ensure t
  :after rjsx-mode)

;; =============================================================================
;; Vue 支持 - 使用 web-mode + Volar LSP (推荐方案)
;; =============================================================================

;; Vue.js 使用 web-mode - 同时支持 Vue 2 和 Vue 3
;; 这种方案通过 Volar LSP 提供完整的 Vue 开发支持
;; 
;; 优势：
;; 1. Vue 官方推荐的 Volar LSP
;; 2. 同时支持 Vue 2 和 Vue 3
;; 3. 更好的 TypeScript 集成
;; 4. 不依赖第三方 Vue mode 包
;;
;; 注意：Vue 文件将由 web-mode 处理，并在下面的 web-mode 配置中设置

;; =============================================================================
;; Web 模式 (HTML/CSS/JS混合)
;; =============================================================================

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.htm\\'" . web-mode)
         ("\\.vue\\'" . web-mode)   ; Vue 单文件组件 (Vue 2 & Vue 3)
         ("\\.tsx\\'" . web-mode))  ; TypeScript JSX
  :hook (web-mode . lsp-deferred)
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t
        web-mode-enable-auto-closing t
        web-mode-enable-auto-quoting t)
  
  ;; 文件类型特定配置
  (add-to-list 'web-mode-content-types-alist '("jsx" . "\\.tsx\\'"))
  (add-to-list 'web-mode-content-types-alist '("vue" . "\\.vue\\'"))
  
  ;; Vue 文件特定设置
  (defun my-web-mode-vue-setup ()
    "配置 web-mode 用于 Vue 开发"
    (when (string-match-p "\\.vue\\'" (or buffer-file-name ""))
      (setq-local web-mode-script-padding 0)
      (setq-local web-mode-style-padding 0)
      (setq-local web-mode-template-padding 0)))
  
  (add-hook 'web-mode-hook 'my-web-mode-vue-setup))

;; =============================================================================
;; 代码格式化和 Lint
;; =============================================================================

;; Prettier - JavaScript 代码格式化
(use-package prettier-js
  :ensure t
  :hook ((js2-mode . prettier-js-mode)      ; .mjs 文件支持
         (rjsx-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (web-mode . prettier-js-mode))     ; 包括 Vue、TSX、HTML
  :config
  (setq prettier-js-args '())
  
  ;; 保存时自动格式化
  (defun my-js-format-on-save ()
    "JavaScript/React/Vue 文件保存时自动使用 Prettier 格式化"
    (when (and (derived-mode-p 'js2-mode 'rjsx-mode 'typescript-mode 'web-mode)
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

;; 在 rjsx-mode 中绑定快捷键
(with-eval-after-load 'rjsx-mode
  (define-key rjsx-mode-map (kbd "C-c C-f") 'my-eslint-fix-buffer))

;; 自动修复和格式化组合
(defun my-js-format-and-lint ()
  "格式化并修复 JavaScript 代码"
  (interactive)
  (when (derived-mode-p 'js2-mode 'rjsx-mode 'typescript-mode 'web-mode)
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
  :hook ((js2-mode . add-node-modules-path)     ; .mjs 文件支持
         (rjsx-mode . add-node-modules-path)
         (typescript-mode . add-node-modules-path)
         (web-mode . add-node-modules-path)))  ; 包括 Vue、TSX、HTML

;; =============================================================================
;; NPM 集成
;; =============================================================================

(use-package npm-mode
  :ensure t
  :hook ((js2-mode . npm-mode)              ; .mjs 文件支持
         (rjsx-mode . npm-mode)
         (typescript-mode . npm-mode)
         (web-mode . npm-mode)))           ; 包括 Vue、TSX、HTML


(provide 'javascript-config)

;;; javascript-config.el ends here
