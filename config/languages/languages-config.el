;;; languages-config.el --- 语言配置模块入口  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: languages, programming, configuration
;; Version: 1.0.0

;;; Commentary:

;; 此文件是languages目录下所有配置的统一入口

;;; Code:

;; =============================================================================
;; config/languages/languages-config.el - 语言配置模块入口  
;; =============================================================================
;; 此文件是languages目录下所有配置的统一入口

;; 动态添加语言子目录到load-path
(let ((languages-dir (file-name-directory (or load-file-name buffer-file-name))))
  (dolist (lang-dir '("cpp" "java" "python" "javascript"))
    (add-to-list 'load-path (expand-file-name lang-dir languages-dir))))

;; 1. C++语言配置
(require 'cpp-config)

;; 2. Python语言配置
(require 'python-config)

;; 3. Java语言配置  
(require 'java-config)

;; 4. JavaScript语言配置
(require 'javascript-config)

;; 提供语言配置入口
(provide 'languages-config)

;;; languages-config.el ends here
