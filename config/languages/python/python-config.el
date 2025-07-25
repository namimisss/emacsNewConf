;; languages/python/python-config.el - Python语言配置

;; Python 基础设置
(setq python-indent-offset 4)          ; Python缩进设置
(setq python-shell-interpreter "python3") ; 使用Python3

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
