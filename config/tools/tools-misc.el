;;; tools-misc.el --- 其他工具配置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: tools, convenience
;; Version: 1.0.0

;;; Commentary:

;; 这个文件包含各种开发工具的配置，包括：
;; - 代码格式化工具 (format-all)
;; - 拼写检查工具 (flyspell)
;; - 快速运行工具 (quickrun)
;; - Hydra 快捷键管理

;;; Code:

(use-package format-all
 :ensure t
 :config
 (add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
 ;; 定义一个函数来在特定模式下添加保存时格式化
 (defun my-format-on-save ()
   "在支持的编程模式下保存时自动格式化（排除 JavaScript，由专门的 prettier 处理）"
   (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'python-mode)
     (format-all-buffer)))
 
 ;; 全局添加保存时格式化钩子
 (add-hook 'before-save-hook 'my-format-on-save)
 
 ;; 在各种编程模式下启用格式化（排除 JavaScript 相关模式）
 (add-hook 'c-mode-hook 'format-all-mode)
 (add-hook 'c++-mode-hook 'format-all-mode)
 (add-hook 'java-mode-hook 'format-all-mode)
 (add-hook 'protobuf-mode-hook 'format-all-mode)
 (add-hook 'python-mode-hook 'format-all-mode))

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
