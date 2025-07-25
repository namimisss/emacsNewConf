;; base/basic.el - 基础配置

;; 基础编辑设置
;; 注意：不启用electric-pair-mode，使用smartparens代替避免冲突
(show-paren-mode 1)                      ; 高亮匹配的括号
(setq show-paren-delay 0)               ; 立即高亮括号
(setq show-paren-style 'parenthesis)    ; 只高亮括号本身

;; 行号和列号显示
(global-display-line-numbers-mode 1)    ; 显示行号
(column-number-mode 1)                   ; 显示列号

;; 自动保存和备份设置
;;(setq auto-save-default t)               ; 启用自动保存
;;(setq make-backup-files t)               ; 启用备份文件
;;(setq backup-directory-alist '(("." . "~/.emacs.d/backup/")))

;; 删除和选择增强
(delete-selection-mode 1)                ; 选中文本后输入会替换
(setq-default truncate-lines t)          ; 不自动换行
(setq-default indent-tabs-mode nil)      ; 使用空格而不是tab
(setq-default tab-width 4)               ; tab宽度为4
(setq-default c-basic-offset 4)          ; C语言缩进为4

;; 滚动设置
(setq scroll-margin 3)                   ; 滚动边距
(setq scroll-conservatively 10000)       ; 平滑滚动

;; 界面设置 - 隐藏菜单栏和工具栏
(menu-bar-mode -1)        ; 隐藏菜单栏
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))     ; 隐藏工具栏
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))   ; 隐藏滚动条

;; 基础工具包
(use-package magit
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1))

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
