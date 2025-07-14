;; basic config
;; Code indentation
;; https://xhcoding.cn/post/20211222180104-emacs%E7%BC%A9%E8%BF%9B%E8%AE%BE%E7%BD%AE/
(require 'cc-mode)
(add-to-list 'c-default-style '(c++-mode . "k&r"))
(add-to-list 'c-default-style '(c-mode . "k&r"))

;; lsp-mode with clangd configuration
;; clangd配置 - 替代ccls
;; clangd会自动被lsp-mode检测并使用，无需额外包
;; 设置clangd的命令行参数以获得最佳性能
(setq lsp-clients-clangd-args '("-j=20"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"
                                "--header-insertion-decorators=0"))

;; 强制LSP使用clangd而不是ccls
(with-eval-after-load 'lsp-mode
  ;; 移除ccls的优先级，确保使用clangd
  (setq lsp-clients-cc-providers '(clangd))
  ;; 设置clangd可执行文件路径
  (setq lsp-clients-clangd-executable "clangd"))

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

;; clangd配置
;; clangd会自动被lsp-mode检测并使用，无需额外配置
;; 但我们可以设置一些优化参数

;; 设置clangd的命令行参数
(setq lsp-clients-clangd-args '("-j=20"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"
                                "--header-insertion-decorators=0"))

;; 强制LSP使用clangd而不是ccls
(with-eval-after-load 'lsp-mode
  ;; 移除ccls的优先级，确保使用clangd
  (setq lsp-clients-cc-providers '(clangd))
  ;; 明确设置C/C++模式使用clangd
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     (lambda () (append lsp-clients-clangd-executable lsp-clients-clangd-args)))
                    :activation-fn (lsp-activate-on "c" "cpp" "objective-c")
                    :priority 1
                    :server-id 'clangd
                    :library-folders-fn (lambda (_workspace) lsp-clients-clangd-library-directories))))

;; company
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-cmake))

;; projectile integration
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
  ;; flycheck-mode 通过全局 prog-mode-hook 启用，这里不需要重复
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
(add-hook 'c++-mode-hook #'lsp-deferred)
(add-hook 'c-mode-hook #'j-cc-mode-hook-func)
(add-hook 'c-mode-hook #'j-cc-fontify-constants-h)
(add-hook 'c-mode-hook #'lsp-deferred)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
;; 为编程模式启用flycheck
(add-hook 'prog-mode-hook 'flycheck-mode)

;; flycheck 键绑定
(global-set-key (kbd "C-c f l") 'flycheck-list-errors)
(global-set-key (kbd "C-c f n") 'flycheck-next-error)
(global-set-key (kbd "C-c f p") 'flycheck-previous-error)
(global-set-key (kbd "C-c f v") 'flycheck-verify-setup)

(setq-default c-basic-offset 4
			  tab-width 4
			  )
(provide 'jconfig)
