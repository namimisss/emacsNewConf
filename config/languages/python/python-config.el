;;; python-config.el ---   -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: configuration
;; Version: 1.0.0

;;; Commentary:

;; 

;;; Code:

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

;;; python-config.el ends here
