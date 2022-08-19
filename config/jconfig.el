;; basic confi
;; Code indentation
;; https://xhcoding.cn/post/20211222180104-emacs%E7%BC%A9%E8%BF%9B%E8%AE%BE%E7%BD%AE/
(add-to-list 'c-default-style '(c++-mode . "k&r"))
(add-to-list 'c-default-style '(c-mode . "k&r"))

;; ivy
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper-isearch)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

;; eglot
;;(add-to-list 'eglot-server-programs '(cc-mode . ("ccls")))
;;(add-to-list 'eglot-server-programs `(cc-mode . ("ccls")))
(add-to-list 'eglot-server-programs '(c++-mode . ("ccls")))
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
;;(add-hook 'c++mode-hook 'cc-mode)

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
  (company-mode)
  (rainbow-delimiters-mode)
  (demangle-mode)
  (modern-c++-font-lock-mode)
  (irony-mode)
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


(provide 'jconfig)
