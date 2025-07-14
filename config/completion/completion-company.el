;; completion/company.el - Company补全配置

(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(provide 'completion-company)
