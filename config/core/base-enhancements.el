;;; base-enhancements.el --- config/core/base-enhancements.el现代化编辑增强功能  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: configuration
;; Version: 1.0.0

;;; Commentary:

;; config/core/base-enhancements.el现代化编辑增强功能

;;; Code:

;; =============================================================================
;; config/core/base-enhancements.el - 现代化编辑增强功能
;; =============================================================================

;; 智能选择扩展
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; 多光标编辑
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; 智能删除空白
(use-package hungry-delete
  :ensure t
  :config 
  (global-hungry-delete-mode)
  ;; 在某些模式下禁用，避免误删
  (add-to-list 'hungry-delete-except-modes 'help-mode)
  (add-to-list 'hungry-delete-except-modes 'minibuffer-mode))

;; 颜色预览
(use-package rainbow-mode
  :ensure t
  :hook ((prog-mode . rainbow-mode)
         (css-mode . rainbow-mode)
         (html-mode . rainbow-mode)))

;; 缩进指示线
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-character ?\|)
  (highlight-indent-guides-auto-odd-face-perc 15)
  (highlight-indent-guides-auto-even-face-perc 15)
  (highlight-indent-guides-auto-character-face-perc 20))

;; 增强的 undo 系统
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

;; 智能括号彩虹显示
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; 代码折叠增强
(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :bind (("C-c h" . hs-toggle-hiding)
         ("C-c H" . hs-hide-all)
         ("C-c S" . hs-show-all)))

;; 智能注释
(use-package evil-nerd-commenter
  :ensure t
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; 文件浏览增强
(use-package dired-subtree
  :ensure t
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

;; 窗口管理增强
(use-package winner
  :config (winner-mode 1)
  :bind (("C-c <left>" . winner-undo)
         ("C-c <right>" . winner-redo)))

;; 文本对象操作
(use-package change-inner
  :ensure t
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

;; 基础工具包
(use-package magit
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1))

(use-package which-key
  :ensure t
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))

(use-package recentf
  :ensure t
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 50)
  (setq recentf-max-menu-items 15))

(use-package ag
  :ensure t)

(use-package avy
  :ensure t)

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package auto-highlight-symbol
  :ensure t)

(use-package protobuf-mode
  :ensure t)

(provide 'base-enhancements) 
;;; base-enhancements.el ends here
