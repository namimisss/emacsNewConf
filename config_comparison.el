;; 如果使用 highlight-defined 的配置（不推荐）
(use-package highlight-defined
  :ensure t
  :hook ((c-mode c++-mode) . highlight-defined-mode)
  :config
  ;; 这种配置的问题：
  ;; 1. highlight-defined 主要是为 Emacs Lisp 设计的
  ;; 2. 它会高亮已定义的符号，而不是置灰未激活的代码
  ;; 3. 对 #if 0 这样的预处理器指令没有特殊处理
  (set-face-attribute 'highlight-defined-macro-name-face nil
                      :foreground "#888888"))

;; 使用 hide-ifdef-mode 的配置（推荐）
(defun j-setup-hide-ifdef ()
  "Configure hide-ifdef-mode for better preprocessor handling"
  (when (fboundp 'hide-ifdef-mode)
    ;; 设置环境变量，告诉 hide-ifdef-mode 哪些宏是定义的
    (setq hide-ifdef-env '((DEBUG . 1)))
    ;; 使用阴影而不是完全隐藏
    (setq hide-ifdef-shadow t)
    ;; 设置阴影 face
    (setq hide-ifdef-shadow-face 'shadow)))

;; 专门处理 #if 0 的配置
(defun j-setup-preprocessor-highlight ()
  "Setup preprocessor directive highlighting"
  (font-lock-add-keywords
   nil 
   '(("^[ \t]*#[ \t]*if[ \t]+0[ \t]*\\(?://.*\\)?$\\(?:\n\\(?:.*\\)\\)*?^[ \t]*#[ \t]*endif" 
      . 'shadow))
   'append))
