;;; tools-misc.el --- 其他工具配置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: tools, convenience
;; Version: 1.0.0

;;; Commentary:

;; 这个文件包含各种开发工具的配置，包括：
;; - 代码格式化工具 (apheleia - 替代 format-all)
;; - 拼写检查工具 (flyspell)
;; - 快速运行工具 (quickrun)
;; - Hydra 快捷键管理

;;; Code:

;; =============================================================================
;; 代码格式化配置 - 使用 Apheleia (更好的 format-all 替代)
;; =============================================================================

(use-package apheleia
  :ensure t
  :config
  ;; 启用全局异步格式化模式
  (apheleia-global-mode +1)
  
  ;; 自定义格式化工具配置
  (setf (alist-get 'python-mode apheleia-mode-alist) '(black))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(black))
  (setf (alist-get 'c-mode apheleia-mode-alist) '(clang-format))
  (setf (alist-get 'c++-mode apheleia-mode-alist) '(clang-format))
  (setf (alist-get 'c-ts-mode apheleia-mode-alist) '(clang-format))
  (setf (alist-get 'c++-ts-mode apheleia-mode-alist) '(clang-format))
  (setf (alist-get 'java-mode apheleia-mode-alist) '(google-java-format))
  (setf (alist-get 'java-ts-mode apheleia-mode-alist) '(google-java-format))
  
  ;; JavaScript/TypeScript 使用 prettier
  (setf (alist-get 'js-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'js2-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'rjsx-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'typescript-mode apheleia-mode-alist) '(prettier))
  
  ;; Web-mode (HTML, Vue, TSX) 使用 prettier
  (setf (alist-get 'web-mode apheleia-mode-alist) '(prettier))
  
  ;; Tree-sitter 模式支持
  (setf (alist-get 'js-ts-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'tsx-ts-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'json-ts-mode apheleia-mode-alist) '(prettier))
  
  ;; JSON 格式化支持
  (setf (alist-get 'json-mode apheleia-mode-alist) '(prettier))
  
  ;; CSS 格式化支持
  (setf (alist-get 'css-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'css-ts-mode apheleia-mode-alist) '(prettier))
  
  ;; 启用保存时自动格式化（默认行为）
  (setq apheleia-format-on-save t))


(use-package flyspell-correct
  :ensure t
  :after flyspell)

(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell-correct)

(use-package quickrun 
  :ensure t
  :bind ("C-c r" . quickrun))

(use-package hydra
  :defer t)

;; 拼写检查配置
;; 设置使用 aspell 作为拼写检查程序
(setq ispell-program-name "aspell")
(setq ispell-dictionary "en_US")
;; aspell 额外参数
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))

;; 启用拼写检查
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'flycheck-mode)

(provide 'tools-misc)

;;; tools-misc.el ends here
