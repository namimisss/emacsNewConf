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

;; 配置预处理器高亮和置灰功能
(defun j-setup-preprocessor-highlight ()
  "Setup preprocessor directive highlighting and inactive region graying"
  ;; 配置 shadow face 以获得更好的置灰效果
  (set-face-attribute 'shadow nil 
                      :foreground "#666666" 
                      :background nil
                      :inherit nil)
  
  ;; 添加字体锁定关键词来识别和置灰 #if 0 区域
  (font-lock-add-keywords
   nil 
   '(;; 匹配 #if 0 ... #endif 区域
     ("^[ \t]*#[ \t]*if[ \t]+0[ \t]*\\(?://.*\\)?$\\(?:\n\\(?:.*\\)\\)*?^[ \t]*#[ \t]*endif\\(?:[ \t]*//.*\\)?[ \t]*$" 
      . 'shadow)
     ;; 匹配 #ifdef NEVER_DEFINED 风格的区域  
     ("^[ \t]*#[ \t]*ifdef[ \t]+\\(?:NEVER_DEFINED\\|DISABLED\\|__NEVER__\\)[ \t]*\\(?://.*\\)?$\\(?:\n\\(?:.*\\)\\)*?^[ \t]*#[ \t]*endif\\(?:[ \t]*//.*\\)?[ \t]*$" 
      . 'shadow))
   'append))

;; 改进的 hide-ifdef 配置
(defun j-setup-hide-ifdef ()
  "Configure hide-ifdef-mode for better preprocessor handling"
  (when (fboundp 'hide-ifdef-mode)
    ;; 设置 hide-ifdef-mode 的环境变量
    (setq hide-ifdef-env '())
    ;; 使用阴影而不是隐藏
    (setq hide-ifdef-shadow t)
    ;; 设置阴影 face
    (setq hide-ifdef-shadow-face 'shadow)))

;; clangd配置
;; 设置clangd的命令行参数以获得最佳性能
(with-eval-after-load 'lsp-mode
  (setq lsp-clients-clangd-args '("-j=20"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--header-insertion=never"
                                  "--header-insertion-decorators=0"
                                  "--log=verbose"
                                  "--pch-storage=memory"))
  
  ;; 强制LSP使用clangd而不是ccls
  (setq lsp-clients-cc-providers '(clangd))
  (setq lsp-clients-clangd-executable "clangd")
  
  ;; 启用 clangd 的语义高亮功能
  (setq lsp-clangd-binary-path "clangd"))

;; C/C++ hook函数
(defun j-cc-mode-hook-func()
  (rainbow-delimiters-mode)
  (demangle-mode)
  (modern-c++-font-lock-mode)
  (irony-mode)
  (hs-minor-mode)
  ;; 配置预处理器功能
  (j-setup-hide-ifdef)
  ;; 启用 ifdef 隐藏模式来处理宏定义
  (hide-ifdef-mode 1))

(defun j-cc-fontify-constants-h ()
  "Better fontification for preprocessor constants"
  (when (memq major-mode '(c-mode c++-mode))
    (font-lock-add-keywords
     nil '(("\\<[A-Z]*_[0-9A-Z_]+\\>" . font-lock-constant-face)
           ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face))
     t)))

;; 添加 C++ 宏定义和预处理器的特殊处理
(defun j-setup-cpp-inactive-regions ()
  "Setup inactive region highlighting for C++ preprocessor directives"
  (when (and (memq major-mode '(c-mode c++-mode))
             (bound-and-true-p lsp-mode))
    ;; 确保 LSP 语义高亮正常工作
    (lsp-semantic-tokens-mode 1)
    ;; 设置宏定义相关的颜色
    (face-remap-add-relative 'lsp-face-semhl-macro 
                             :foreground "#888888" :slant 'italic)
    (face-remap-add-relative 'lsp-face-semhl-comment 
                             :foreground "#666666")))

;; 添加hook
(add-hook 'c++-mode-hook #'j-cc-mode-hook-func)
(add-hook 'c++-mode-hook #'j-cc-fontify-constants-h)
(add-hook 'c++-mode-hook #'j-setup-cpp-inactive-regions)
(add-hook 'c++-mode-hook #'j-setup-preprocessor-highlight)
(add-hook 'c++-mode-hook #'lsp-deferred)
(add-hook 'c-mode-hook #'j-cc-mode-hook-func)
(add-hook 'c-mode-hook #'j-cc-fontify-constants-h)
(add-hook 'c-mode-hook #'j-setup-cpp-inactive-regions)
(add-hook 'c-mode-hook #'j-setup-preprocessor-highlight)
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
