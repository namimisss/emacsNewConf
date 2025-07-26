;;; basic-settings.el --- 基础Emacs设置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: configuration, settings
;; Version: 1.0.0

;;; Commentary:

;; 这个文件包含不依赖任何外部包的基础Emacs设置，包括：
;; - 启动优化
;; - 界面设置
;; - 编辑行为配置
;; - 文件处理设置

;;; Code:

;; =============================================================================
;; config/core/basic-settings.el - 基础Emacs设置
;; =============================================================================
;; 此文件包含不依赖任何外部包的基础Emacs设置

;; 启动性能优化
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))  ; 100MB

;; 启动完成后恢复垃圾回收设置
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold gc-cons-threshold-original)
            (message "🚀 Emacs启动完成，耗时 %.2f 秒，垃圾回收 %d 次"
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

;; =============================================================================
;; 界面设置
;; =============================================================================

;; 隐藏不必要的界面元素
(menu-bar-mode -1)                       ; 隐藏菜单栏
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))                    ; 隐藏工具栏
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))                  ; 隐藏滚动条

;; 显示设置
(global-display-line-numbers-mode 1)     ; 显示行号
(column-number-mode 1)                   ; 显示列号
(show-paren-mode 1)                      ; 高亮匹配括号
(setq show-paren-delay 0)               ; 立即高亮括号
(setq show-paren-style 'parenthesis)    ; 只高亮括号本身

;; 标题栏显示完整路径
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; =============================================================================
;; 编辑设置
;; =============================================================================

;; 基础编辑行为
(delete-selection-mode 1)                ; 选中文本后输入会替换
(setq-default truncate-lines t)          ; 不自动换行
(setq-default indent-tabs-mode nil)      ; 使用空格而不是tab
(setq-default tab-width 4)               ; tab宽度为4
(setq-default c-basic-offset 4)          ; C语言缩进为4

;; 文件编码
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; 滚动设置
(setq scroll-margin 3)                   ; 滚动边距
(setq scroll-conservatively 10000)       ; 平滑滚动
(setq scroll-step 1)                     ; 滚动步长
(setq scroll-preserve-screen-position t) ; 保持屏幕位置

;; 缓冲区设置
(setq enable-recursive-minibuffers t)     ; 允许递归minibuffer
(setq use-dialog-box nil)                ; 不使用对话框
(setq inhibit-startup-message t)         ; 隐藏启动消息
(setq initial-scratch-message nil)       ; 清空scratch缓冲区消息

;; =============================================================================
;; 文件和备份设置
;; =============================================================================

;; 备份文件设置
(setq make-backup-files t)                    ; 启用备份
(setq backup-directory-alist 
      '(("." . "~/.emacs.d/backups/")))       ; 备份目录
(setq backup-by-copying t)                    ; 通过复制备份
(setq delete-old-versions t)                  ; 删除旧版本
(setq kept-new-versions 6)                    ; 保留新版本数
(setq kept-old-versions 2)                    ; 保留旧版本数
(setq version-control t)                      ; 版本控制

;; 自动保存设置
(setq auto-save-default t)                    ; 启用自动保存
(setq auto-save-timeout 20)                   ; 20秒后自动保存
(setq auto-save-interval 200)                 ; 200个字符后自动保存

;; 创建备份目录
(unless (file-directory-p "~/.emacs.d/backups/")
  (make-directory "~/.emacs.d/backups/" t))

;; =============================================================================
;; 搜索和替换设置
;; =============================================================================

;; 搜索设置
(setq search-highlight t)                     ; 高亮搜索结果
(setq query-replace-highlight t)              ; 高亮替换
(setq case-fold-search t)                     ; 忽略大小写搜索

;; =============================================================================
;; 性能优化设置
;; =============================================================================

;; IO性能优化
(setq read-process-output-max (* 1024 1024)) ; 1MB
(setq process-adaptive-read-buffering nil)    ; 禁用自适应读缓冲

;; 减少不必要的UI更新
(setq redisplay-dont-pause t)                 ; 不暂停重绘
(setq fast-but-imprecise-scrolling t)         ; 快速但不精确的滚动

;; 字体锁定优化
(setq jit-lock-defer-time 0.05)              ; 延迟字体锁定
(setq jit-lock-stealth-time 1)               ; 隐形字体锁定时间

;; =============================================================================
;; 其他实用设置
;; =============================================================================

;; 时间格式
(setq display-time-24hr-format t)             ; 24小时制
(setq display-time-day-and-date t)            ; 显示日期

;; 警告和提示
(setq ring-bell-function 'ignore)             ; 禁用响铃
(setq warning-minimum-level :emergency)       ; 只显示紧急警告

;; 历史记录
(setq history-length 1000)                    ; 历史记录长度
(setq history-delete-duplicates t)            ; 删除重复历史

;; 光标设置
(setq-default cursor-type 'bar)               ; 光标样式为竖线
(blink-cursor-mode 1)                         ; 光标闪烁

;; =============================================================================
;; 内置模式配置
;; =============================================================================

;; recentf - 最近文件
(setq recentf-max-saved-items 50)             ; 最多保存50个
(setq recentf-max-menu-items 15)              ; 菜单最多显示15个

;; 自动刷新文件
(global-auto-revert-mode 1)                   ; 全局自动刷新
(setq auto-revert-verbose nil)                ; 静默刷新
(setq global-auto-revert-non-file-buffers t)  ; 刷新非文件缓冲区

;; 保存位置
(save-place-mode 1)                           ; 记住光标位置

;; 禁用不必要的功能
(setq use-file-dialog nil)                    ; 不使用文件对话框
(setq use-dialog-box nil)                     ; 不使用对话框
(setq inhibit-startup-screen t)               ; 不显示启动屏幕

(provide 'basic-settings)

;;; basic-settings.el ends here 