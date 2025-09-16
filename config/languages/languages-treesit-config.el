;;; languages-treesit-config.el --- Tree-sitter 语言配置模块入口  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: languages, programming, tree-sitter
;; Version: 1.0.0

;;; Commentary:

;; Tree-sitter 语言配置统一入口

;;; Code:

;; 动态添加语言子目录到load-path
(let ((languages-dir (file-name-directory (or load-file-name buffer-file-name))))
  (dolist (lang-dir '("cpp" "java" "python" "javascript"))
    (add-to-list 'load-path (expand-file-name lang-dir languages-dir))))

(message "🌟 Loading Tree-sitter language configuration...")

(require 'cpp-treesit-config)
(require 'python-treesit-config)
(require 'java-treesit-config)
(require 'javascript-treesit-config)
(require 'cmake-config)

(message "✓ Tree-sitter language configuration loaded successfully")

(provide 'languages-treesit-config)

;;; languages-treesit-config.el ends here
