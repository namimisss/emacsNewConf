;;; init-config.el --- 配置系统入口文件  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: init, configuration
;; Version: 1.0.0

;;; Commentary:

;; 这是 Emacs 配置系统的主入口文件，负责：
;; - 版本检查
;; - 模块加载管理
;; - 错误处理和回退配置
;; - 诊断功能

;;; Code:

;; =============================================================================
;; config/init-config.el - 配置系统入口文件
;; =============================================================================

;; 版本检查配置 - 专为Emacs 30优化
(defvar fish-emacs-target-version 30
  "target Emacs Version")

;; 严格版本检查 - 只支持Emacs 30
(defun fish-check-emacs-version ()
  "检查Emacs版本是否为目标版本30"
  (let ((version emacs-major-version))
    (if (= version fish-emacs-target-version)
        (progn
          (message "Emacs Version (%d) valid" version)
          t)
      (progn
        (message "Emacs %d invalid，use Emacs %d" 
                 version fish-emacs-target-version)
        nil))))
(let ((config-dir (expand-file-name "config" user-emacs-directory)))
  (dolist (subdir '("core" "ui" "completion" "tools" "languages"))
    (add-to-list 'load-path (expand-file-name subdir config-dir))))

(defun fish-load-configuration ()
  "Load config"
  (require 'core-config)
;;  (require 'ui-config)
 (require 'completion-config)
 (require 'tools-config)
 (require 'languages-config)
  )
(defun fish-load-minimal-config ())

(if (fish-check-emacs-version)
    (fish-load-configuration)
  (fish-load-minimal-config))

(provide 'init-config)

;;; init-config.el ends here 
