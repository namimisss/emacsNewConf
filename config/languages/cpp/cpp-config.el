;; languages/cpp/cpp-config.el - C++语言配置

;; C++ 基础配置
(require 'cc-mode)
(add-to-list 'c-default-style '(c++-mode . "k&r"))
(add-to-list 'c-default-style '(c-mode . "k&r"))

;; C++ 相关包
(use-package cmake-mode
  :ensure t)

(use-package demangle-mode
  :ensure t)

(use-package disaster
  :ensure t)

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

(use-package irony
  :ensure t)

(use-package irony-eldoc
  :ensure t)

(use-package company-irony
  :ensure t)

(use-package company-irony-c-headers
  :ensure t)

;; 注意: rtags已被LSP/clangd替代，不再需要
;; (use-package rtags
;;   :ensure t)
;; (use-package ivy-rtags
;;   :ensure t)

(use-package rainbow-delimiters
  :ensure t)

;; clangd配置
;; 设置clangd的命令行参数以获得最佳性能
(with-eval-after-load 'lsp-mode
  (setq lsp-clients-clangd-args '("-j=20"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--header-insertion=never"
                                  "--header-insertion-decorators=0"))
  
  ;; 强制LSP使用clangd而不是ccls
  (setq lsp-clients-cc-providers '(clangd))
  (setq lsp-clients-clangd-executable "clangd"))

;; C/C++ hook函数
(defun j-cc-mode-hook-func()
  (rainbow-delimiters-mode)
  (demangle-mode)
  (modern-c++-font-lock-mode)
  (irony-mode)
  (hs-minor-mode))

(defun j-cc-fontify-constants-h ()
  "Better fontification for preprocessor constants"
  (when (memq major-mode '(c-mode c++-mode))
    (font-lock-add-keywords
     nil '(("\\<[A-Z]*_[0-9A-Z_]+\\>" . font-lock-constant-face)
           ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face))
     t)))

;; 添加hook
(add-hook 'c++-mode-hook #'j-cc-mode-hook-func)
(add-hook 'c++-mode-hook #'j-cc-fontify-constants-h)
(add-hook 'c++-mode-hook #'lsp-deferred)
(add-hook 'c-mode-hook #'j-cc-mode-hook-func)
(add-hook 'c-mode-hook #'j-cc-fontify-constants-h)
(add-hook 'c-mode-hook #'lsp-deferred)

;; irony配置
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook #'irony-eldoc)

;; company-irony
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

;; company cmake
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-cmake))

;; 基础设置
(setq-default c-basic-offset 4
              tab-width 4)

(provide 'cpp/cpp-config)
