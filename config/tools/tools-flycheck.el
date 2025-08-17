;;; tools-flycheck.el --- Flycheck语法检查配置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: tools, convenience
;; Version: 1.0.0

;;; Commentary:

;; Flycheck 语法检查配置，包括：
;; - 全局 Flycheck 模式
;; - Flycheck 扩展包
;; - 快捷键绑定

;;; Code:

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-checker-error-threshold 10000)
  ;; 配置flycheck与lsp-mode的集成
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq flycheck-idle-change-delay 0.5))

;; flycheck 扩展包
(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (flycheck-pos-tip-mode))

(use-package flycheck-popup-tip
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-popup-tip-mode))

;; flycheck 键绑定
;; (global-set-key (kbd "C-c f l") 'flycheck-list-errors)
;; (global-set-key (kbd "C-c f n") 'flycheck-next-error)
;; (global-set-key (kbd "C-c f p") 'flycheck-previous-error)
;; (global-set-key (kbd "C-c f v") 'flycheck-verify-setup)

(provide 'tools-flycheck)

;;; tools-flycheck.el ends here
