;; languages/javascript/js-config.el - JavaScript语言配置

(use-package tern
  :ensure t
  :config
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil))

(provide 'javascript/javascript-config)
