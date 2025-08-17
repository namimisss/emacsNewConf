;;; completion-ivy.el --- Ivy补全配置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: completion, convenience
;; Version: 1.0.0

;;; Commentary:

;; Ivy 补全系统配置，包括：
;; - Swiper 搜索工具
;; - Ivy 补全框架
;; - Counsel 增强命令
;; - 相关扩展包配置

;;; Code:

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
)

(use-package ivy-rich
  :ensure t
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package ivy-hydra
  :ensure t)

(use-package ivy-avy
  :ensure t)

(use-package prescient
  :ensure t)

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (ivy-prescient-mode 1))

(use-package amx
  :ensure t
  :config
  (amx-mode 1))

(use-package wgrep
  :ensure t)

(use-package fuzzy
  :ensure t)

;; (use-package flx
;;   :ensure t)

;; (use-package flx-ido
;;   :ensure t)

;; 可选的 ivy 增强包（根据需要启用）
;; (use-package ivy-posframe
;;   :ensure t)

;; (use-package all-the-icons-ivy
;;   :ensure t)

;; 移除不必要的 ido 相关包（已使用 ivy）
;; (use-package ido-completing-read+
;;   :ensure t)

;; (use-package ido-sort-mtime
;;   :ensure t)

;; (use-package crm-custom
;;   :ensure t)

(provide 'completion-ivy)

(use-package marginalia
 :init
 (marginalia-mode))

;;; completion-ivy.el ends here
