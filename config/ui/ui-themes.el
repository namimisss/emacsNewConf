;;; ui-themes.el ---   -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: configuration
;; Version: 1.0.0

;;; Commentary:

;; 

;;; Code:

;; ui/themes.el - 主题配置

(use-package solarized-theme
  :ensure t)

(use-package monokai-theme
  :ensure t
  :init (load-theme 'monokai t))

(use-package darkokai-theme
  :ensure t)

(use-package nerd-icons
  :ensure t
  :custom (nerd-icons-font-family "Symbols Nerd Font Mono"))

;; 移除 all-the-icons（与 nerd-icons 重复）
;; (use-package all-the-icons
;;   :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :hook
  (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-project-detection 'project))

(use-package powerline
  :ensure t)

(use-package powerline-evil
  :ensure t)

(provide 'ui-themes)

;;; ui-themes.el ends here
