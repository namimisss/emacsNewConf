;; =============================================================================
;; config/completion/completion-config.el - 补全系统配置模块入口
;; =============================================================================
;; 此文件是completion目录下所有配置的统一入口

;; 1. Company补全 (基础补全框架)
(require 'completion-company)

;; 2. Ivy补全 (主要使用的补全系统)
(require 'completion-ivy)

;; 3. Vertico补全 (可选的现代补全，与ivy冲突，暂时注释)
;; (require 'completion-vertico)

;; 4. 现代化补全系统 (consult + embark + orderless，可选)
;; (require 'completion-modern)

;; 提供completion配置入口
(provide 'completion-config) 