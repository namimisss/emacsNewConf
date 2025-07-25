;; =============================================================================
;; config/core/core-config.el - 核心配置模块入口
;; =============================================================================
;; 此文件是core目录下所有配置的统一入口

;; 1. 基础Emacs设置 (不依赖任何包，必须最先加载)
(require 'basic-settings)

;; 2. 包管理系统 (必须在其他包配置之前加载)
(require 'package-setup)

;; 3. 编辑增强功能 (依赖use-package)
(require 'base-enhancements)

;; 4. 全局快捷键 (最后加载，确保所有函数都已定义)
(require 'keybindings)

;; 提供core配置入口
(provide 'core-config) 