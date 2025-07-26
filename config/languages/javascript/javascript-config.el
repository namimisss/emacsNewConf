;;; javascript-config.el ---   -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: configuration
;; Version: 1.0.0

;;; Commentary:

;; 

;;; Code:

;; languages/javascript/js-config.el - JavaScript语言配置

(use-package tern
  :ensure t
  :config
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil))

(provide 'javascript-config)

;;; javascript-config.el ends here
