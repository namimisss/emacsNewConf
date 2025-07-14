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
  ;; 手动添加yasnippet支持
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(provide 'completion-company)
