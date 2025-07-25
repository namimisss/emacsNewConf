;; completion/company.el - Company补全配置

;; YASnippet配置 - 提供代码模板功能
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  ;; 设置snippets目录
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  ;; 确保目录存在
  (unless (file-exists-p "~/.emacs.d/snippets")
    (make-directory "~/.emacs.d/snippets" t)))

(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-require-match 'never)
  ;; 优化补全性能
  (setq company-auto-complete nil)
  (setq company-show-numbers t)
  
  ;; LSP 优先配置
  (setq company-backends
        '((company-capf :with company-yasnippet)
          (company-dabbrev-code company-gtags company-etags company-keywords)
          company-files
          company-dabbrev))
  
  ;; 手动添加yasnippet支持
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  
  ;; 为非LSP模式的情况保留yasnippet支持
  (with-eval-after-load 'lsp-mode
    (setq company-backends
          '((company-capf :with company-yasnippet)
            (company-dabbrev-code company-gtags company-etags company-keywords)
            company-files
            company-dabbrev))))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-max-candidates 50)
  (setq company-box-icons-alist 'company-box-icons-all-the-icons))

(provide 'completion-company)
