;;; tools-lsp.el --- LSP配置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: configuration
;; Version: 1.0.0

;;; Commentary:

;; LSP Mode 配置文件

;;; Code:

;; tools/lsp.el - LSP配置

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook (
	 (lsp-mode . lsp-enable-which-key-integration)
	 (lsp-mode . flycheck-mode)
	 (c-mode . lsp-deferred)
	 (c++-mode . lsp-deferred)
	 (java-mode . lsp-deferred)
	 (js2-mode . lsp-deferred)              ; JavaScript 支持
	 (rjsx-mode . lsp-deferred)             ; React JSX 支持
	 (typescript-mode . lsp-deferred)       ; TypeScript 支持
	 (web-mode . lsp-deferred)              ; Vue/TSX/HTML 支持 (推荐用于Vue开发)
	 (json-mode . lsp-deferred)             ; JSON 支持
	 ;; Tree-sitter 模式支持
	 (js-ts-mode . lsp-deferred)            ; JavaScript Tree-sitter
	 (typescript-ts-mode . lsp-deferred)    ; TypeScript Tree-sitter (由Volar处理)
	 (tsx-ts-mode . lsp-deferred)           ; TSX Tree-sitter (由Volar处理)
	 (json-ts-mode . lsp-deferred)          ; JSON Tree-sitter
	 (lsp-mode . lsp-lens-mode)
	 (java-mode-hook lsp-java-boot-lens-mode)
	 (sh-mode . lsp)
	 )
  :init
  (setq lsp-keymap-prefix "C-c l"
	lsp-enable-file-watchers nil
	read-process-output-max (* 1024 1024)  ; 1 mb
	lsp-completion-provider :capf
	lsp-idle-delay 0.500
	;; 禁用lsp内置的flycheck，使用外部flycheck
	lsp-prefer-flymake nil
	lsp-diagnostics-provider :flycheck
	;; 确保lens功能启用
	lsp-lens-enable t                     ; 全局启用lens
	lsp-lens-place-position 'above-line   ; lens显示在行上方
	;; 启用语义高亮和宏定义置灰
	lsp-semantic-tokens-enable t          ; 启用语义标记
	lsp-enable-semantic-highlighting t    ; 启用语义高亮
	;; 性能优化设置
	lsp-auto-execute-action nil           ; 不自动执行代码动作
	lsp-eldoc-enable-hover nil            ; 减少 eldoc 干扰
	lsp-signature-auto-activate nil       ; 手动触发签名帮助
	lsp-signature-render-documentation nil
	lsp-headerline-breadcrumb-enable nil  ; 关闭面包屑导航
	lsp-modeline-code-actions-enable nil  ; 关闭模式行代码动作
	lsp-modeline-diagnostics-enable t     ; 保留诊断信息
	)
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-intelephense-multi-root nil) ; don't scan unnecessary projects
  (with-eval-after-load 'lsp-intelephense
    (setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil))
  
  ;; 智能LSP服务器选择：基于项目类型自动配置
  (with-eval-after-load 'lsp-mode
    ;; 关闭文件监听以提升性能
    (setq lsp-enable-file-watchers nil
          lsp-file-watch-threshold 2000)
    
    ;; 现代化项目类型检测
    (defun my-is-vue-project ()
      "检测当前是否为Vue项目"
      (when-let ((root (lsp-workspace-root)))
        (or (file-exists-p (expand-file-name "vue.config.js" root))
            (file-exists-p (expand-file-name "vite.config.js" root))
            (file-exists-p (expand-file-name "nuxt.config.js" root))
            (and (file-exists-p (expand-file-name "package.json" root))
                 (with-temp-buffer
                   (insert-file-contents (expand-file-name "package.json" root))
                   (goto-char (point-min))
                   (re-search-forward "\"vue\"\\|\"@vue\"\\|\"nuxt\"" nil t))))))
    
    
    ;; 简化的Volar配置：仅为Vue项目启用Take Over Mode
    (when (executable-find "vue-language-server")
      (lsp-register-client
       (make-lsp-client
        :new-connection (lsp-stdio-connection '("vue-language-server" "--stdio"))
        :major-modes '(js-ts-mode typescript-ts-mode tsx-ts-mode web-mode)
        :server-id 'volar-takeover
        :priority 10
        :activation-fn (lambda (&rest _) (my-is-vue-project))
        :initialization-options
        (lambda ()
          (let ((ts-lib (or 
                         ;; 优先使用项目本地的TypeScript
                         (when-let ((root (lsp-workspace-root)))
                           (let ((local-ts (expand-file-name "node_modules/typescript/lib/tsserverlibrary.js" root)))
                             (when (file-exists-p local-ts) local-ts)))
                         ;; 使用NVM全局TypeScript
                         (let ((nvm-ts "~/.nvm/versions/node/v22.18.0/lib/node_modules/typescript/lib/tsserverlibrary.js"))
                           (when (file-exists-p (expand-file-name nvm-ts)) (expand-file-name nvm-ts)))
                         ;; 使用系统全局TypeScript
                         (when-let ((global-ts (shell-command-to-string "npm root -g 2>/dev/null")))
                           (let ((global-path (concat (string-trim global-ts) "/typescript/lib/tsserverlibrary.js")))
                             (when (file-exists-p global-path) global-path))))))
            `(:typescript
              (:tsdk ,(file-name-directory ts-lib)
               :preferences
               (:includePackageJsonAutoImports "on"
                :includeCompletionsForModuleExports t))
              :vue
              (:hybridMode nil)))))))
    
    ;; 标准TypeScript Language Server配置
    (when (executable-find "typescript-language-server")
      (setq lsp-clients-typescript-init-opts
            '(:preferences
              (:includePackageJsonAutoImports "on"
               :includeCompletionsForModuleExports t
               :includeCompletionsWithSnippetText t)))))
  
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  ;; 添加一些有用的快捷键
  (define-key lsp-mode-map (kbd "C-c l r") 'lsp-find-references)
  (define-key lsp-mode-map (kbd "C-c l d") 'lsp-find-definition)
  (define-key lsp-mode-map (kbd "C-c l i") 'lsp-find-implementation))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-sideline-enable t)              ; 启用侧边栏
  (lsp-ui-sideline-show-hover t)          ; 显示悬停信息
  (lsp-ui-sideline-show-diagnostics t)    ; 显示诊断信息
  (lsp-ui-sideline-show-code-actions nil) ; 关闭代码动作以减少干扰
  (lsp-ui-flycheck-enable t)
  (lsp-ui-flycheck-list-position 'right)
  (lsp-ui-flycheck-live-reporting t))

(use-package lsp-ivy
  :ensure t)

(use-package consult-lsp
  :ensure t)

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :ensure t
  :commands lsp-treemacs-errors-list
  :bind (:map lsp-mode-map
         ("M-9" . lsp-treemacs-errors-list)))

(use-package treemacs
  :ensure t
  :commands (treemacs)
  :after (lsp-mode))

(provide 'tools-lsp)

;;; tools-lsp.el ends here
