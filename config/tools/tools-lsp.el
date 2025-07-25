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
