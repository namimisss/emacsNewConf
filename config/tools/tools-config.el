;;; tools-config.el --- 开发工具配置模块入口  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: tools, configuration
;; Version: 1.0.0

;;; Commentary:

;; 此文件是tools目录下所有配置的统一入口

;;; Code:

;; =============================================================================
;; config/tools/tools-config.el - 开发工具配置模块入口
;; =============================================================================
;; 此文件是tools目录下所有配置的统一入口

;; 传统工具配置 (由 init-config.el 统一管理时使用)
;; 1. Flycheck语法检查
(require 'tools-flycheck)

;; 2. LSP配置 
(require 'tools-lsp)

;; 3. 项目管理 (Projectile)
(require 'tools-projectile)

;; 4. 其他工具
(require 'tools-misc)

;; 提供tools配置入口
(provide 'tools-config)

;;; tools-config.el ends here
