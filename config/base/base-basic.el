;; base/basic.el - 基础配置

;; 界面设置 - 隐藏菜单栏和工具栏
(menu-bar-mode -1)        ; 隐藏菜单栏
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))     ; 隐藏工具栏
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))   ; 隐藏滚动条

;; 基础工具包
(use-package magit
  :ensure t)

(use-package which-key
  :ensure t
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))

(use-package recentf
  :ensure t)

(use-package ag
  :ensure t)

(use-package avy
  :ensure t)

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package auto-highlight-symbol
  :ensure t)

(use-package protobuf-mode
  :ensure t)

(provide 'base-basic)
