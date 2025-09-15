;;; javascript-treesit-config.el --- JavaScript Tree-sitter配置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: languages, javascript, typescript, tree-sitter
;; Version: 1.0.0

;;; Commentary:

;; JavaScript 开发配置 (Tree-sitter 版本)，包括：
;; - Tree-sitter 语法支持 (js-ts-mode, typescript-ts-mode, tsx-ts-mode)
;; - LSP 语言服务器支持
;; - 格式化支持 (Prettier + ESLint)
;; - 保持向后兼容的第三方包支持

;;; Code:

;; =============================================================================
;; JavaScript Tree-sitter 配置 (优先使用)
;; =============================================================================

;; JavaScript Tree-sitter 模式配置
(use-package js-ts-mode
  :ensure nil  ; 内置模式
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.mjs\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode))  ; JSX 也用 js-ts-mode
  :hook ((js-ts-mode . lsp-deferred))
  :config
  (setq js-indent-level 2)
  
  ;; 键绑定设置将在mode hook中处理
  )

;; =============================================================================
;; TypeScript Tree-sitter 支持
;; =============================================================================

;; TypeScript Tree-sitter 模式
(use-package typescript-ts-mode
  :ensure nil  ; 内置模式
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))  ; TSX 使用专门的模式
  :hook ((typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred))
  :config
  (setq typescript-ts-mode-indent-offset 2)
  
  ;; 键绑定设置将在mode hook中处理
  )

;; =============================================================================
;; JSON Tree-sitter 配置
;; =============================================================================

;; JSON Tree-sitter 模式
(use-package json-ts-mode
  :ensure nil  ; 内置模式
  :mode "\\.json\\'"
  :hook (json-ts-mode . lsp-deferred)
  :config
  (setq json-ts-mode-indent-offset 2))

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
;; Volar LSP 配置在单独的文件中
(require 'volar-config)

;; =============================================================================
;; Web 模式 (HTML/CSS/JS混合)
;; =============================================================================

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.htm\\'" . web-mode)
         ("\\.vue\\'" . web-mode))   ; Vue 单文件组件 (Vue 2 & Vue 3)
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
;; React 支持 - 现在使用 Tree-sitter
;; =============================================================================

;; React 代码片段
(use-package react-snippets
  :ensure t
  :after (js-ts-mode tsx-ts-mode))

;; =============================================================================
;; JavaScript 重构工具 (与 Tree-sitter 兼容)
;; =============================================================================

(use-package js2-refactor
  :ensure t
  :hook ((js-ts-mode . js2-refactor-mode)
         (typescript-ts-mode . js2-refactor-mode)
         (tsx-ts-mode . js2-refactor-mode))
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

;; =============================================================================
;; 代码格式化和 Lint (Tree-sitter 兼容)
;; =============================================================================

;; Prettier - JavaScript 代码格式化 (Tree-sitter 兼容)
(use-package prettier-js
  :ensure t
  :hook ((js-ts-mode . prettier-js-mode)        ; Tree-sitter JS
         (typescript-ts-mode . prettier-js-mode) ; Tree-sitter TS
         (tsx-ts-mode . prettier-js-mode)        ; Tree-sitter TSX
         (json-ts-mode . prettier-js-mode)       ; Tree-sitter JSON
         (web-mode . prettier-js-mode))          ; Vue/HTML
  :config
  (setq prettier-js-args '())
  
  ;; 保存时自动格式化
  (defun my-js-format-on-save ()
    "JavaScript/React/Vue 文件保存时自动使用 Prettier 格式化 (Tree-sitter 兼容)"
    (when (and (derived-mode-p 'js-ts-mode 'typescript-ts-mode 'tsx-ts-mode 
                               'json-ts-mode 'web-mode)
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

;; 自动修复和格式化组合
(defun my-js-format-and-lint ()
  "格式化并修复 JavaScript 代码 (Tree-sitter 兼容)"
  (interactive)
  (when (derived-mode-p 'js-ts-mode 'typescript-ts-mode 'tsx-ts-mode 
                        'json-ts-mode 'web-mode)
    (if (executable-find "eslint")
        (my-eslint-fix-buffer)
      (prettier-js))))

;; 绑定组合命令
(global-set-key (kbd "C-c f j") 'my-js-format-and-lint)

;; =============================================================================
;; Node.js 支持 (Tree-sitter 兼容)
;; =============================================================================

(use-package add-node-modules-path
  :ensure t
  :hook ((js-ts-mode . add-node-modules-path)        ; Tree-sitter JS
         (typescript-ts-mode . add-node-modules-path) ; Tree-sitter TS
         (tsx-ts-mode . add-node-modules-path)        ; Tree-sitter TSX
         (json-ts-mode . add-node-modules-path)       ; Tree-sitter JSON
         (web-mode . add-node-modules-path)))         ; Vue/HTML

;; =============================================================================
;; NPM 集成 (Tree-sitter 兼容)
;; =============================================================================

(use-package npm-mode
  :ensure t
  :hook ((js-ts-mode . npm-mode)        ; Tree-sitter JS
         (typescript-ts-mode . npm-mode) ; Tree-sitter TS
         (tsx-ts-mode . npm-mode)        ; Tree-sitter TSX
         (json-ts-mode . npm-mode)       ; Tree-sitter JSON
         (web-mode . npm-mode)))         ; Vue/HTML

;; =============================================================================
;; Tree-sitter 特定增强
;; =============================================================================

;; Tree-sitter 语法导航增强
(defun my-js-treesit-setup ()
  "JavaScript Tree-sitter 特定设置"
  (when (treesit-parser-list)
    ;; 启用语法感知的导航
    (setq-local treesit-defun-type-regexp
                (rx (or "function_declaration"
                        "method_definition" 
                        "arrow_function"
                        "function_expression"
                        "class_declaration")))
    
    ;; 启用更好的缩进
    (setq-local treesit-simple-indent-rules
                (when (fboundp 'treesit-simple-indent-rules-get)
                  (treesit-simple-indent-rules-get major-mode)))))

;; 添加到所有 JavaScript Tree-sitter 模式
(add-hook 'js-ts-mode-hook #'my-js-treesit-setup)
(add-hook 'typescript-ts-mode-hook #'my-js-treesit-setup)
(add-hook 'tsx-ts-mode-hook #'my-js-treesit-setup)

;; =============================================================================
;; 向后兼容性支持 (备选模式)
;; =============================================================================

;; 保留 js2-mode 作为备选（如果需要特殊功能）
(use-package js2-mode
  :ensure t
  :disabled t  ; 默认禁用，优先使用 Tree-sitter
  :config
  (setq js2-basic-offset 2
        js2-bounce-indent-p nil
        js2-highlight-level 2
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil))

;; 保留 typescript-mode 作为备选
(use-package typescript-mode
  :ensure t
  :disabled t  ; 默认禁用，优先使用 Tree-sitter
  :config
  (setq typescript-indent-level 2))

;; 保留 rjsx-mode 作为备选（如果 Tree-sitter 不可用）
(use-package rjsx-mode
  :ensure t
  :disabled t  ; 默认禁用，优先使用 Tree-sitter
  :config
  (setq js2-basic-offset 2
        js2-bounce-indent-p nil
        js2-highlight-level 2
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil))

;; 保留 json-mode 作为备选
(use-package json-mode
  :ensure t
  :disabled t  ; 默认禁用，优先使用 Tree-sitter
  :config
  (setq json-reformat:indent-width 2))

;; 设置键绑定的函数
(defun my-js-ts-setup-keybindings ()
  "设置JavaScript/TypeScript Tree-sitter模式的键绑定"
  (let ((mode-map (cond
                   ((derived-mode-p 'js-ts-mode) js-ts-mode-map)
                   ((derived-mode-p 'typescript-ts-mode) typescript-ts-mode-map)
                   ((derived-mode-p 'tsx-ts-mode) tsx-ts-mode-map))))
    (when mode-map
      (define-key mode-map (kbd "M-.") 'lsp-find-definition)
      (define-key mode-map (kbd "M-,") 'lsp-find-references)
      (define-key mode-map (kbd "C-c C-r") 'lsp-rename)
      (define-key mode-map (kbd "C-c C-f") 'my-eslint-fix-buffer)
      (when (derived-mode-p 'js-ts-mode)
        (define-key mode-map (kbd "M-?") 'lsp-find-references)))))

;; 添加到hook中
(add-hook 'js-ts-mode-hook #'my-js-ts-setup-keybindings)
(add-hook 'typescript-ts-mode-hook #'my-js-ts-setup-keybindings)
(add-hook 'tsx-ts-mode-hook #'my-js-ts-setup-keybindings)

(provide 'javascript-treesit-config)

;;; javascript-treesit-config.el ends here
