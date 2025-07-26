;;; python-config.el --- Python语言配置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: languages, python, development
;; Version: 1.0.0

;;; Commentary:

;; Python 开发配置，包括：
;; - Elpy Python IDE功能
;; - Pyvenv 虚拟环境管理
;; - 代码补全和格式化

;;; Code:

;; Python 基础设置
(setq python-indent-offset 4)          ; Python缩进设置
(setq python-shell-interpreter "python3") ; 使用Python3

;; Pyvenv 虚拟环境管理
(use-package pyvenv
  :ensure t
  :config
  ;; 设置使用系统Python环境
  (setq pyvenv-workon "sys"))

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
