;; =============================================================================
;; config/tools/tools-config.el - 开发工具配置模块入口
;; =============================================================================
;; 此文件是tools目录下所有配置的统一入口

;; 1. 语法检查工具
(require 'tools-flycheck)

;; 2. LSP语言服务器配置
(require 'tools-lsp)

;; 3. 项目管理工具
(require 'tools-projectile)

;; 4. 其他杂项工具
(require 'tools-misc)

;; 提供tools配置入口
(provide 'tools-config) 