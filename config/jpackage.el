;; ------ 清华源 ------
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))


;; 1. install use-package
(setq package-selected-packages '(use-package))
;;(package-install-selected-packages)
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; basic
(use-package magit
  :ensure t)
(use-package flycheck
  :ensure t)
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))
(use-package auto-highlight-symbol
  :ensure t
;;  :config
;;  (global-auto-highlight-symbol-mode)
  )
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))
(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))
(use-package recentf
  :ensure t)
(use-package ag
  :ensure t)
(use-package ido
  :config
  (ido-mode 1))
(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (setq ido-vertical-show-count t)
  (setq ido-use-faces t)
  (set-face-attribute 'ido-vertical-first-match-face nil
                      :background nil
                      :foreground "orange")
  (set-face-attribute 'ido-vertical-only-match-face nil
                      :background nil
                      :foreground nil)
  (set-face-attribute 'ido-vertical-match-face nil
                      :foreground nil))

;;(use-package undo-tree
;;  :ensure t
;;  :diminish nil
;;  :config
;;  (global-undo-tree-mode)
;;  (setq undo-tree-auto-save-history t)
;;  )

(use-package marginalia
  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))
(use-package avy
  :ensure t)
(use-package multiple-cursors
  :ensure t
;;  :config
;;  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;;  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;;  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  )







;; basic/theme
(use-package solarized-theme
  :ensure t
  ;;  :init (load-theme 'solarized-dark t)
  )
(use-package darkokai-theme
  :ensure t
  :init (load-theme 'darkokai t))
(use-package all-the-icons
  :ensure t)
(use-package doom-modeline
  :ensure t
  :hook
  (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-project-detection 'project))
(use-package powerline
  :ensure t)
(use-package powerline-evil
  :ensure t)



;; complete
;; ;; complet/company
(use-package company
  :ensure t)
(use-package company-box
  :ensure t)

;; completion/ido
(use-package flx-ido
  :ensure t)
(use-package ido-completing-read+
  :ensure t)
(use-package ido-sort-mtime
  :ensure t)
(use-package crm-custom
  :ensure t)

;; completion/ivy
(use-package swiper
  :ensure t)
(use-package ivy
  :ensure t)
(use-package ivy-hydra
  :ensure t)
(use-package ivy-avy
  :ensure t)
(use-package counsel
  :ensure t)
(use-package amx
  :ensure t)
(use-package counsel-projectile
  :ensure t)
(use-package ivy-rich
  :ensure t)
(use-package wgrep
  :ensure t)
(use-package prescient
  :ensure t)
(use-package ivy-prescient
  :ensure t)
(use-package fuzzy
  :ensure t)
(use-package flx
  :ensure t)
(use-package ivy-posframe
  :ensure t)
(use-package all-the-icons-ivy
  :ensure t )

;; completion/vertico
(use-package vertico
  :ensure t)
(use-package orderless
  :ensure t)
(use-package consult
  :ensure t)
(use-package compat
  :ensure t)
(use-package consult-dir
  :ensure t)
(use-package consult-flycheck
  :ensure t)
(use-package embark
  :ensure t)
(use-package embark-consult
  :ensure t)
(use-package marginalia
  :ensure t)
(use-package wgrep
  :ensure t)
(use-package all-the-icons-completion
  :ensure t)
(use-package vertico-posframe
  :ensure t)

;; tools
;; tools/ccl
(use-package eglot
  :ensure t)
(use-package consult-eglot
  :ensure t)
(use-package lsp-mode
  :ensure t)
(use-package lsp-ui
  :ensure t)
(use-package lsp-ivy
  :ensure t)
(use-package helm
  :ensure t)
(use-package helm-lsp
  :ensure t)
(use-package consult-lsp
  :ensure t )
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  (setq-default projectile-mode-line-prefix "Proj")
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

 (use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs!") ;; 个性签名，随读者喜好设置
  (setq dashboard-projects-backend 'projectile) ;; 读者可以暂时注释掉这一行，等安装了 projectile 后再使用
  (setq dashboard-startup-banner 'official) ;; 也可以自定义图片
  (setq dashboard-items '((recents  . 5)   ;; 显示多少个最近文件
			  (bookmarks . 5)  ;; 显示多少个最近书签
			  (projects . 10))) ;; 显示多少个最近项目
  (dashboard-setup-startup-hook))

(use-package neotree
  :config
  ;; f8 to view tree strucure of folder
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
  (global-set-key [f8] 'neotree-project-dir)
  ;; switch with projectile
  (use-package projectile)
  (setq projectile-switch-project-action 'neotree-projectile-action))

;; lang
;; ;; cc
;; DONE
(use-package cmake-mode
  :ensure t)
;; DONE
(use-package demangle-mode
  :ensure t)
;; 反汇编
(use-package disaster
  :ensure t)
;; DONE
(use-package modern-cpp-font-lock
  :ensure t
  :diminish nil
  :config
  (add-to-list 'modern-c++-attributes "deprecated")
  (setq modern-c++-literal-boolean t)
  (setq modern-c++-literal-string t)
  (setq modern-c++-literal-integer t)
  (setq modern-c++-literal-null-pointer t)
  (setq modern-c++-stl-cstdint t))
;; DONE
(use-package irony
  :ensure)
;; DONE
(use-package irony-eldoc
  :ensure t)
;; DONE
(use-package company-irony
  :ensure t)
;; DONE
(use-package company-irony-c-headers
  :ensure t)
(use-package rtags
  :ensure t)
(use-package ivy-rtags
  :ensure t)
(use-package helm-rtags
  :ensure t)
(use-package rainbow-delimiters
  :ensure t)
(use-package neotree
  :ensure t)


;; lang/python
(use-package elpy
  :ensure t
  :defer t
  :init
  (elpy-enable)
  :hook
  (elpy-mode . flycheck-mode)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

(use-package py-autopep8
  :ensure t
  :hook
  (elpy-mode . py-autopep8-enable-on-save))


(provide 'jpackage)
