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
;; Volar LSP 配置在单独的文件中
(require 'volar-config)

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
        ;; 性能优化：减少实时处理
        web-mode-enable-auto-pairing nil        ; 禁用实时括号配对
        web-mode-enable-css-colorization nil    ; 禁用实时颜色显示
        web-mode-enable-current-element-highlight nil  ; 禁用实时元素高亮
        web-mode-enable-auto-closing nil        ; 禁用实时标签关闭
        web-mode-enable-auto-quoting nil)       ; 禁用实时引号处理
  
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

;; Prettier 格式化完全由 Apheleia 处理
;; prettier-js 包功能与 Apheleia 重复，已移除以避免冲突和依赖

;; =============================================================================
;; ESLint 集成配置 (统一配置)
;; =============================================================================

;; 加载统一的 ESLint 配置
(require 'eslint-config)

;; ESLint 错误检测和显示完全由 Flycheck 处理
;; 手动修复功能已移除，专注于通过 Flycheck 显示错误
;; 用户可以根据 Flycheck 提示手动修复代码问题

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
