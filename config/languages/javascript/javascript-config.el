;;; javascript-config.el --- JavaScript语言配置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: languages, javascript, node
;; Version: 1.0.0

;;; Commentary:

;; JavaScript 开发配置，包括：
;; - Tern 语言服务器支持
;; - 基础编辑功能
;; - 格式化支持

;;; Code:

;; Tern - JavaScript代码补全和分析
(use-package tern
  :ensure t
  :config
  ;; 移除默认键绑定，避免与xref冲突
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil))

;; JavaScript模式钩子
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-hook 'javascript-mode-hook (lambda () (tern-mode t)))

(provide 'javascript-config)

;;; javascript-config.el ends here
