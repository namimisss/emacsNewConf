;;; tools-treesit-config.el --- Tree-sitter 工具配置模块入口  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: tools, tree-sitter, configuration
;; Version: 1.0.0

;;; Commentary:

;; Tree-sitter 工具配置统一入口

;;; Code:

(message "🔧 Loading Tree-sitter tools configuration...")

;; Tree-sitter 工具配置
;; 1. Tree-sitter 核心配置
(require 'tools-treesit)

;; 2. Flycheck语法检查 (Tree-sitter 增强)
(require 'tools-flycheck)

;; 3. LSP配置 (Tree-sitter 集成)
(require 'tools-lsp)

;; 4. 项目管理 (Projectile)
(require 'tools-projectile)

;; 5. 其他工具
(require 'tools-misc)

(message "✓ Tree-sitter tools configuration loaded successfully")

(provide 'tools-treesit-config)

;;; tools-treesit-config.el ends here
