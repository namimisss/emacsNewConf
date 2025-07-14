;; completion/ivy.el - Ivy补全配置

(use-package swiper
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
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox))

(use-package ivy-rich
  :ensure t
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :ensure t
  :bind (("C-x b" . 'counsel-switch-buffer)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

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
  :ensure t)

(use-package wgrep
  :ensure t)

(use-package fuzzy
  :ensure t)

(use-package flx
  :ensure t)

(use-package flx-ido
  :ensure t)

(use-package ivy-posframe
  :ensure t)

(use-package all-the-icons-ivy
  :ensure t)

(use-package ido-completing-read+
  :ensure t)

(use-package ido-sort-mtime
  :ensure t)

(use-package crm-custom
  :ensure t)

(provide 'completion-ivy)
