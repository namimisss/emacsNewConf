;;; init-config.el --- 配置系统入口文件  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: init, configuration
;; Version: 1.0.0

;;; Commentary:

;; 这是 Emacs 配置系统的主入口文件，负责：
;; - 版本检查 (只支持 Emacs 30)
;; - Tree-sitter 支持检测
;; - 配置路径选择和加载

;;; Code:

;; =============================================================================
;; 版本检查和 Tree-sitter 检测
;; =============================================================================

(defvar fish-emacs-target-version 30
  "target Emacs Version")

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

(defun fish-check-treesitter-support ()
  "检查 Tree-sitter 支持状态"
  (if (and (fboundp 'treesit-available-p) (treesit-available-p))
      (progn
        (message "✓ Tree-sitter 可用")
        t)
    (progn
      (message "○ Tree-sitter 不可用，使用传统配置")
      nil)))

;; =============================================================================
;; 配置路径设置
;; =============================================================================
(let ((config-dir (expand-file-name "config" user-emacs-directory)))
  (dolist (subdir '("core" "ui" "completion" "tools" "languages"))
    (add-to-list 'load-path (expand-file-name subdir config-dir))))

;; =============================================================================
;; 配置加载器
;; =============================================================================

(defun fish-load-treesitter-config ()
  "加载 Tree-sitter 增强配置"
  (message "🚀 加载 Tree-sitter 配置...")
  
  ;; 加载基础配置
  (require 'core-config)
  (require 'completion-config)
  
  ;; 加载 Tree-sitter 工具配置
  (require 'tools-treesit)
  (require 'tools-flycheck)
  (require 'tools-lsp)
  (require 'tools-projectile)
  (require 'tools-misc)
  
  ;; 加载 Tree-sitter 语言配置
  (require 'languages-treesit-config)
  
  (message "✓ Tree-sitter 配置加载完成"))

(defun fish-load-traditional-config ()
  "加载传统配置"
  (message "📋 加载传统配置...")
  
  ;; 加载基础配置
  (require 'core-config)
  (require 'completion-config)
  
  ;; 加载传统工具和语言配置
  (require 'tools-config)
  (require 'languages-config)
  
  (message "✓ 传统配置加载完成"))

(defun fish-load-minimal-config ()
  "加载最小配置（版本不匹配时）"
  (message "🔧 Emacs版本不匹配，加载最小配置...")
  (require 'core-config)
  (message "✓ 最小配置加载完成"))

;; =============================================================================
;; 主配置入口
;; =============================================================================

(defun fish-load-appropriate-config ()
  "根据版本和 Tree-sitter 支持情况加载配置"
  (if (fish-check-emacs-version)
      (if (fish-check-treesitter-support)
          (fish-load-treesitter-config)
        (fish-load-traditional-config))
    (fish-load-minimal-config)))

;; 启动配置系统
(fish-load-appropriate-config)

(provide 'init-config)

;;; init-config.el ends here 
