;;; ui-interface.el --- 用户界面配置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: configuration
;; Version: 1.0.0

;;; Commentary:

;; 

;;; Code:

;; ui/interface.el - 界面配置

;; 禁用 ido（与 ivy 冲突）
;; (use-package ido
;;   :config
;;   (ido-mode 1))

;; (use-package ido-vertical-mode
;;   :ensure t
;;   :config
;;   (ido-vertical-mode 1)
;;   (setq ido-vertical-define-keys 'C-n-and-C-p-only)
;;   (setq ido-vertical-show-count t)
;;   (setq ido-use-faces t)
;;   (set-face-attribute 'ido-vertical-first-match-face nil
;;                       :background nil
;;                       :foreground "orange")
;;   (set-face-attribute 'ido-vertical-only-match-face nil
;;                       :background nil
;;                       :foreground nil)
;;   (set-face-attribute 'ido-vertical-match-face nil
;;                       :foreground nil))

;;(use-package marginalia
;;  :init
;;  (marginalia-mode))
(provide 'ui-interface)

;;; ui-interface.el ends here
