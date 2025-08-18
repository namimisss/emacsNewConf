;;; tools-projectile.el ---   -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: configuration
;; Version: 1.0.0

;;; Commentary:

;; 

;;; Code:

;; tools/projectile.el - 项目管理配置

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  (setq-default projectile-mode-line-prefix "Proj")
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1)
;;  (global-set-key (kbd "C-c p s g") 'counsel-projectile-grep)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq counsel-projectile-find-file-matcher 'counsel--find-file-matcher))

;; 项目树形文件浏览器增强
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

;; 保留 neotree 作为备选（可选使用）
;; (use-package neotree
;;   :ensure t
;;   :config
;;   ;; f8 to view tree structure of folder
;;   (defun neotree-project-dir ()
;;     "Open NeoTree using the git root."
;;     (interactive)
;;     (let ((project-dir (projectile-project-root))
;;           (file-name (buffer-file-name)))
;;       (neotree-toggle)
;;       (if project-dir
;;           (if (neo-global--window-exists-p)
;;               (progn
;;                 (neotree-dir project-dir)
;;                 (neotree-find file-name)))
;;         (message "Could not find git project root."))))
;;   (global-set-key [f8] 'neotree-project-dir)
;;   (setq projectile-switch-project-action 'neotree-projectile-action))

;; Treemacs 替代配置
(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-width 30)
  :bind
  (:map global-map
        ([f8] . treemacs)
        ("C-x t t" . treemacs)))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs!")
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-startup-banner 'official)
  (setq dashboard-items '((recents  . 5)
			  (bookmarks . 5)
			  (projects . 10)))
  (dashboard-setup-startup-hook))

(provide 'tools-projectile)

;;; tools-projectile.el ends here
