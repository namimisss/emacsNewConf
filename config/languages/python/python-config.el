;; languages/python/python-config.el - Python语言配置

(use-package elpy
  :ensure t
  :defer t
  :init
  (elpy-enable)
  :hook
  (elpy-mode . flycheck-mode)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

(provide 'python-config)
