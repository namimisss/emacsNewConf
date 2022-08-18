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
(use-package ido-vertical-mode
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
  :ensure t)

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
  :ensure t)
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



(provide 'jpackage)
