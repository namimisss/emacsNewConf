;; tools/misc.el - 其他工具配置

(use-package format-all
 :ensure t
 :config
 (add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
 ;; 定义一个函数来在特定模式下添加保存时格式化
 (defun my-format-on-save ()
   "在支持的编程模式下保存时自动格式化"
   (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'python-mode 'js-mode 'javascript-mode)
     (format-all-buffer)))
 
 ;; 全局添加保存时格式化钩子
 (add-hook 'before-save-hook 'my-format-on-save)
 
 ;; 在各种编程模式下启用格式化
 (add-hook 'c-mode-hook 'format-all-mode)
 (add-hook 'c++-mode-hook 'format-all-mode)
 (add-hook 'java-mode-hook 'format-all-mode)
 (add-hook 'protobuf-mode-hook 'format-all-mode)
 (add-hook 'python-mode-hook 'format-all-mode)
 (add-hook 'javascript-mode-hook 'format-all-mode)
 (add-hook 'js-mode-hook 'format-all-mode))

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

;; 拼写检查
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'flycheck-mode)

(provide 'tools-misc)
