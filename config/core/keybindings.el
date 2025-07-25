;; =============================================================================
;; config/core/keybindings.el - 全局快捷键配置
;; =============================================================================
;; 此文件包含所有全局快捷键设置

;; =============================================================================
;; 基础编辑快捷键
;; =============================================================================

;; 文件操作
(global-set-key (kbd "C-x C-f") 'counsel-find-file)    ; 查找文件 (如果有ivy)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)   ; 最近文件
(global-set-key (kbd "C-x C-b") 'ibuffer)              ; 缓冲区列表

;; 窗口操作
(global-set-key (kbd "C-x o") 'ace-window)             ; 窗口跳转 (如果有ace-window)

;; 搜索和导航
(global-set-key (kbd "C-s") 'swiper)                   ; 搜索 (如果有swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)           ; 恢复ivy会话

;; =============================================================================
;; 项目管理快捷键
;; =============================================================================

;; Projectile快捷键 (如果有projectile)
(global-set-key (kbd "C-c p") 'projectile-command-map)

;; =============================================================================
;; 工具快捷键
;; =============================================================================

;; Git相关
(global-set-key (kbd "C-x g") 'magit-status)           ; Git状态

;; 快速运行
(global-set-key (kbd "C-c r") 'quickrun)               ; 快速运行代码

;; 文件树
(global-set-key (kbd "<f8>") 'treemacs)                ; 文件树

;; =============================================================================
;; 补全和导航增强
;; =============================================================================

;; 智能选择扩展
(global-set-key (kbd "C-=") 'er/expand-region)         ; 智能选择扩展

;; 多光标编辑 (如果有multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; 智能注释
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)

;; =============================================================================
;; 窗口管理
;; =============================================================================

;; 窗口历史
(global-set-key (kbd "C-c <left>") 'winner-undo)       ; 撤销窗口操作
(global-set-key (kbd "C-c <right>") 'winner-redo)      ; 重做窗口操作

;; =============================================================================
;; LSP快捷键 (如果有LSP)
;; =============================================================================

;; 这些快捷键会在lsp-mode加载时自动绑定
;; C-c l d - 跳转到定义
;; C-c l r - 查找引用
;; C-c l i - 跳转到实现
;; M-9     - LSP错误列表

;; =============================================================================
;; Hydra快捷键
;; =============================================================================

;; 缩放hydra
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "放大")
  ("l" text-scale-decrease "缩小")
  ("r" text-scale-adjust "重置"))

;; 窗口管理hydra
(defhydra hydra-window (global-map "C-c w")
  "window"
  ("h" windmove-left "左")
  ("j" windmove-down "下")
  ("k" windmove-up "上")
  ("l" windmove-right "右")
  ("v" split-window-right "垂直分割")
  ("s" split-window-below "水平分割")
  ("d" delete-window "删除窗口")
  ("o" delete-other-windows "只保留当前窗口"))

;; =============================================================================
;; 自定义函数快捷键
;; =============================================================================

(defun my-open-config-file ()
  "快速打开配置文件"
  (interactive)
  (find-file (expand-file-name "config/init-config.el" user-emacs-directory)))

(defun my-reload-config ()
  "重新加载配置"
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory))
  (message "配置已重新加载"))

(defun my-open-scratch ()
  "打开scratch缓冲区"
  (interactive)
  (switch-to-buffer "*scratch*"))

;; 绑定自定义函数
(global-set-key (kbd "C-c e c") 'my-open-config-file)  ; 打开配置
(global-set-key (kbd "C-c e r") 'my-reload-config)     ; 重载配置
(global-set-key (kbd "C-c e s") 'my-open-scratch)      ; 打开scratch

;; =============================================================================
;; 系统和调试快捷键
;; =============================================================================

(defun my-show-startup-time ()
  "显示启动时间"
  (interactive)
  (message "Emacs启动时间: %.2f秒"
           (float-time (time-subtract after-init-time before-init-time))))

(global-set-key (kbd "C-c s t") 'my-show-startup-time) ; 显示启动时间
(global-set-key (kbd "C-c s p") 'my-package-diagnosis) ; 包管理诊断

;; =============================================================================
;; 备用快捷键 (如果对应的包不存在，使用默认功能)
;; =============================================================================

;; 如果没有counsel，使用默认的查找文件
(unless (fboundp 'counsel-find-file)
  (global-set-key (kbd "C-x C-f") 'find-file))

;; 如果没有swiper，使用默认搜索
(unless (fboundp 'swiper)
  (global-set-key (kbd "C-s") 'isearch-forward))

;; 如果没有ace-window，使用默认窗口切换
(unless (fboundp 'ace-window)
  (global-set-key (kbd "C-x o") 'other-window))

;; 如果没有treemacs，可以绑定其他文件管理器
(unless (fboundp 'treemacs)
  (global-set-key (kbd "<f8>") 'dired))

(provide 'keybindings) 