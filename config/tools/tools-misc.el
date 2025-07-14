;; tools/misc.el - 其他工具配置

(use-package format-all
 :ensure t
 :config
 (add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
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

;; 拼写检查
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'flycheck-mode)

(provide 'tools-misc)
