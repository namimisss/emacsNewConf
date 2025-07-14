
;; basic confi
;; Code indentation
;; https://xhcoding.cn/post/20211222180104-emacs%E7%BC%A9%E8%BF%9B%E8%AE%BE%E7%BD%AE/
(add-to-list 'c-default-style '(c++-mode . "k&r"))
(add-to-list 'c-default-style '(c-mode . "k&r"))
;; company
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-cmake))

;; powerline
;;(require 'powerline)
;;(powerline-default-theme)
;;(require 'powerline-evil)

;; ivy
;;(ivy-mode)
;;(setq ivy-use-virtual-buffers t)
;;(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
;;(global-set-key "\C-s" 'swiper-isearch)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

;; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; hydra
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

;; lsp-mode

(require 'ccls)
(setq ccls-executable "/usr/bin/ccls")

;; lsp-mode configuration
(setq lsp-clients-ccls-args '("--init={\"index\": {\"threads\": 20}}"))

;; Add C/C++ hooks - note that lsp-mode configuration in jpackage.el already includes java and sh
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'c-mode-hook 'lsp)

(defun projectile-project-find-function (dir)
  (let* ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(with-eval-after-load 'project
  (add-to-list 'project-find-functions 'projectile-project-find-function))

;; lang/cc
;; cmake-mode
(require 'cmake-mode)
;; irony
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;; irony-eldoc
(add-hook 'irony-mode-hook #'irony-eldoc)
;; company-irony
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
;; company-irony-c-headers
(require 'company-irony-c-headers)
;; Load with `irony-mode` as a grouped backend
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))
(defun j-cc-mode-hook-func()
  (rainbow-delimiters-mode)
  (demangle-mode)
  (modern-c++-font-lock-mode)
  (irony-mode)
  (hs-minor-mode)
  )

;;;###autoload
(defun j-cc-fontify-constants-h ()
  "Better fontification for preprocessor constants"
  (when (memq major-mode '(c-mode c++-mode))
    (font-lock-add-keywords
     nil '(("\\<[A-Z]*_[0-9A-Z_]+\\>" . font-lock-constant-face)
           ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face))
     t)))

(add-hook 'c++-mode-hook #'j-cc-mode-hook-func)
(add-hook 'c++-mode-hook #'j-cc-fontify-constants-h)
(add-hook 'c-mode-hook #'j-cc-mode-hook-func)
(add-hook 'c-mode-hook #'j-cc-fontify-constants-h)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(setq-default c-basic-offset 4
			  tab-width 4
			  )
(provide 'jconfig)
