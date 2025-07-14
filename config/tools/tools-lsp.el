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
	)
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-intelephense-multi-root nil) ; don't scan unnecessary projects
  (with-eval-after-load 'lsp-intelephense
    (setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil))
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
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
