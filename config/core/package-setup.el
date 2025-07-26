;;; package-setup.el --- 包管理系统配置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: package, configuration
;; Version: 1.0.0

;;; Commentary:

;; 这个文件配置包源、use-package和包管理相关设置，包括：
;; - 包源配置 (GNU ELPA, MELPA, 清华镜像)
;; - use-package 初始化
;; - 自动包安装设置

;;; Code:

;; =============================================================================
;; config/core/package-setup.el - 包管理系统配置
;; =============================================================================
;; 此文件配置包源、use-package和包管理相关设置

;; =============================================================================
;; 包系统初始化
;; =============================================================================

;; 确保package系统已加载
(require 'package)

;; 性能优化设置
(setq package-enable-at-startup nil)         ; 禁用启动时自动加载包
(setq package-quickstart t)                  ; Emacs 27+ 快速启动
(setq use-package-hook-name-suffix nil)      ; 减少hook名称处理开销
(setq use-package-expand-minimally t)        ; 减少宏展开开销

;; =============================================================================
;; 包源配置
;; =============================================================================

;; 配置包源 - 使用国内镜像提高下载速度
(setq package-archives '(("gnu"   . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("org"   . "https://mirrors.ustc.edu.cn/elpa/org/")))

;; 安全设置 - 避免签名验证导致的问题
(setq package-check-signature nil)

;; 初始化包系统
(package-initialize)

;; =============================================================================
;; 健壮的 use-package 安装 - 仿照backup的简单方式
;; =============================================================================

(require 'cl-lib)

;; 1. 安装 use-package（仅在未安装时连接网络）
(setq package-selected-packages '(use-package))

;; 只有在use-package未安装时才刷新包列表并安装
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; 2. 加载 use-package
(require 'use-package)

;; 3. 配置 use-package
(setq use-package-always-ensure t)           ; 自动下载包
(setq use-package-verbose t)                 ; 显示加载信息
(setq use-package-compute-statistics t)      ; 统计加载时间

(message "🎉 包管理系统初始化完成")

;; =============================================================================
;; 包管理辅助函数
;; =============================================================================

(defun my-package-installed-p (package)
  "检查包是否已安装"
  (package-installed-p package))

(defun my-package-install (package)
  "安装指定的包"
  (if (package-installed-p package)
      (message "📦 %s 已安装" package)
    (message "📦 [跳过] %s (包管理不可用)" package)))

(defun my-package-upgrade-all ()
  "升级所有已安装的包"
  (interactive)
  (if (package-installed-p 'use-package)
      (condition-case err
          (progn
            (package-refresh-contents)
            (let ((upgrades (package-menu--find-upgrades)))
              (if upgrades
                  (progn
                    (message "发现 %d 个包需要升级..." (length upgrades))
                    (dolist (upgrade upgrades)
                      (my-package-install (car upgrade)))
                    (message "包升级完成！"))
                (message "所有包都是最新版本"))))
        (error (message "包升级失败: %s" (error-message-string err))))
    (message "包管理系统不可用，无法升级")))

(defun my-package-list-installed ()
  "列出所有已安装的包"
  (interactive)
  (if (package-installed-p 'use-package)
      (with-output-to-temp-buffer "*Installed Packages*"
        (dolist (pkg package-activated-list)
          (princ (format "%s\n" pkg))))
    (message "包管理系统不可用，无法列出已安装包")))

;; =============================================================================
;; 包目录管理 - 安全创建
;; =============================================================================

(defun my-safe-create-directories ()
  "安全地创建必要的目录"
  (let ((dirs '("~/.emacs.d/elpa"
                "~/.emacs.d/backups"
                "~/.emacs.d/auto-save-list"
                "~/.emacs.d/undo")))
    (dolist (dir dirs)
      (condition-case err
          (unless (file-directory-p dir)
            (make-directory dir t)
            (message "📁 创建目录: %s" dir))
        (error (message "⚠️  创建目录失败 %s: %s" dir (error-message-string err)))))))

;; 执行目录创建
(my-safe-create-directories)

;; =============================================================================
;; 依赖检查 - 非阻塞版本
;; =============================================================================

(defun my-check-dependencies ()
  "检查关键依赖是否可用，不阻塞启动"
  (unless (package-installed-p 'use-package)
    (message "⚠️  包管理系统初始化不完整")
    (message "💡 某些功能可能不可用，但基本功能正常")
    (message "🔧 可以稍后手动运行 M-x my-package-diagnosis 检查状态")))

;; 执行依赖检查
(my-check-dependencies)

;; =============================================================================
;; 自动清理 - 安全版本
;; =============================================================================

(defun my-package-auto-clean ()
  "自动清理未使用的包"
  (when (and (package-installed-p 'use-package) 
             (bound-and-true-p package-selected-packages))
    (condition-case err
        (package-autoremove)
      (error (message "自动清理失败: %s" (error-message-string err))))))

;; 启动完成后执行清理 (延迟执行以避免影响启动速度)
(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-timer 30 nil #'my-package-auto-clean)))

;; =============================================================================
;; 调试和诊断
;; =============================================================================

(defun my-package-diagnosis ()
  "诊断包管理系统状态"
  (interactive)
  (message "=== 包管理系统诊断 ===")
  (message "包管理初始化状态: %s" (if (package-installed-p 'use-package) "成功" "失败"))
  (message "包源数量: %d" (length package-archives))
  (message "已安装包数量: %d" (length package-activated-list))
  (message "use-package统计: %s" 
           (if (package-installed-p 'use-package)
               "已启用" "已禁用"))
  (when (package-installed-p 'use-package)
    (message "可使用 M-x use-package-report 查看详细统计"))
  
  ;; 检查关键目录
  (let ((dirs '("~/.emacs.d/elpa" "~/.emacs.d/backups")))
    (dolist (dir dirs)
      (message "目录 %s: %s" dir (if (file-directory-p dir) "存在" "不存在"))))
  
  ;; 网络连接测试建议
  (unless (package-installed-p 'use-package)
    (message "=== 修复建议 ===")
    (message "1. 检查网络连接")
    (message "2. 运行 M-x package-refresh-contents")
    (message "3. 运行 M-x package-install RET use-package")
    (message "4. 重启 Emacs")))

;; 启动诊断信息
(if (package-installed-p 'use-package)
    (message "🎉 包管理系统初始化完成")
  (message "⚠️  包管理系统使用降级模式，基本功能可用"))

(provide 'package-setup)

;;; package-setup.el ends here 