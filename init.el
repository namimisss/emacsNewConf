;; =============================================================================
;; init.el - Emacs Configuration Entry Point
;; =============================================================================

;; 添加配置路径到 load-path
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; 加载主配置入口
(require 'init-config)

;; Custom 设置区域（保留给 Emacs 自动管理，如有需要）
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
