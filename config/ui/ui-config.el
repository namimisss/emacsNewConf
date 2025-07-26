;;; ui-config.el --- 界面配置模块入口  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: ui, interface, configuration
;; Version: 1.0.0

;;; Commentary:

;; 此文件是ui目录下所有配置的统一入口

;;; Code:

;; =============================================================================
;; config/ui/ui-config.el - 界面配置模块入口
;; =============================================================================
;; 此文件是ui目录下所有配置的统一入口

;; 1. 界面设置
(require 'ui-interface)

;; 2. 主题配置  
(require 'ui-themes)

;; 提供ui配置入口
(provide 'ui-config)

;;; ui-config.el ends here
