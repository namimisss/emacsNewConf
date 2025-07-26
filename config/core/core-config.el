;;; core-config.el --- 核心配置模块入口  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: core, configuration
;; Version: 1.0.0

;;; Commentary:

;; 此文件是core目录下所有配置的统一入口

;;; Code:

;; =============================================================================
;; config/core/core-config.el - 核心配置模块入口
;; =============================================================================
;; 此文件是core目录下所有配置的统一入口

;; 1. 基础设置（优先级最高）
(require 'basic-settings)

;; 2. 包管理系统配置
(require 'package-setup)

;; 3. 编辑增强功能
(require 'base-enhancements)

;; 4. 快捷键配置
(require 'keybindings)

;; 提供核心配置入口
(provide 'core-config)

;;; core-config.el ends here
