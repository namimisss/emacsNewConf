;;; java-config.el --- Java语言配置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: configuration
;; Version: 1.0.0

;;; Commentary:

;; 

;;; Code:

;; languages/java/java-config.el - Java语言配置

(use-package lsp-java 
  :ensure t
  :defer t
  :config (add-hook 'java-mode-hook 'lsp))

(use-package dap-java
  :ensure nil)

;; projectile integration
(defun projectile-project-find-function (dir)
  (let* ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(with-eval-after-load 'project
  (add-to-list 'project-find-functions 'projectile-project-find-function))

(provide 'java-config)

;;; java-config.el ends here
