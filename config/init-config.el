;;; init-config.el --- 配置系统入口文件  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: init, configuration
;; Version: 1.0.0

;;; Commentary:

;; 这是 Emacs 配置系统的主入口文件，负责：
;; - 版本检查
;; - 模块加载管理
;; - 错误处理和回退配置
;; - 诊断功能

;;; Code:

;; =============================================================================
;; config/init-config.el - 配置系统入口文件
;; =============================================================================

;; 版本检查配置 - 专为Emacs 30优化
(defvar my-emacs-target-version 30
  "目标Emacs版本，配置专为此版本优化")

;; 严格版本检查 - 只支持Emacs 30
(defun my-check-emacs-version ()
  "检查Emacs版本是否为目标版本30"
  (let ((version emacs-major-version))
    (if (= version my-emacs-target-version)
        (progn
          (message "✅ Emacs版本 (%d) 检查通过，配置已为此版本优化" version)
          t)
      (progn
        (message "❌ 此配置专为Emacs %d优化，当前版本 %d 不受支持" 
                 my-emacs-target-version version)
        (message "💡 请升级到Emacs %d 以获得最佳体验" my-emacs-target-version)
        nil))))

;; 精简的load-path设置 - 只添加主要模块目录
(let ((config-dir (expand-file-name "config" user-emacs-directory)))
  ;; 添加主要模块目录
  (dolist (subdir '("core" "ui" "completion" "tools" "languages"))
    (add-to-list 'load-path (expand-file-name subdir config-dir))))

;; =============================================================================
;; 健壮的模块加载系统
;; =============================================================================

(defvar my-loaded-modules '()
  "成功加载的模块列表")

(defvar my-failed-modules '()
  "加载失败的模块列表")

(defun my-safe-require (module &optional description)
  "安全地加载模块，失败时不影响其他模块"
  (let ((desc (or description (symbol-name module))))
    (condition-case err
        (progn
          (require module)
          (add-to-list 'my-loaded-modules module)
          (message "✅ %s 模块加载成功" desc)
          t)
      (error
       (add-to-list 'my-failed-modules (cons module err))
       (message "⚠️  %s 模块加载失败: %s" desc (error-message-string err))
       nil))))

(defun my-load-module-with-fallback (module description fallback-fn)
  "加载模块，如果失败则执行回退函数"
  (unless (my-safe-require module description)
    (condition-case err
        (when fallback-fn
          (funcall fallback-fn)
          (message "🔧 %s 使用回退配置" description))
      (error 
       (message "⚠️  %s 回退配置也失败: %s" description (error-message-string err))))))

;; =============================================================================
;; 回退配置函数
;; =============================================================================

(defun my-fallback-basic-settings ()
  "基础设置的回退配置"
  (message "🔧 应用基础设置回退配置...")
  ;; 基本界面设置
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  ;; 基本编辑功能
  (global-display-line-numbers-mode 1)
  (column-number-mode 1)
  (show-paren-mode 1)
  (delete-selection-mode 1)
  ;; 基本快捷键
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "M-/") 'hippie-expand))

(defun my-fallback-ui-config ()
  "UI配置的回退配置"
  (message "🔧 应用UI回退配置...")
  ;; 基本主题设置
  (load-theme 'tango-dark t)
  ;; 基本字体设置（如果可用）
  (when (display-graphic-p)
    (set-face-attribute 'default nil :height 120)))

(defun my-fallback-completion-config ()
  "补全配置的回退配置"
  (message "🔧 应用补全回退配置...")
  ;; 启用内置补全
  (setq tab-always-indent 'complete)
  (setq completion-cycle-threshold 3)
  (setq completion-flex-nospace nil)
  (setq completion-pcm-complete-word-inserts-delimiters t))

(defun my-fallback-tools-config ()
  "工具配置的回退配置"
  (message "🔧 应用工具回退配置...")
  ;; 基本的编辑辅助（备份功能已在 basic-settings.el 中禁用）
  (electric-pair-mode 1)
  (save-place-mode 1))

(defun my-fallback-languages-config ()
  "语言配置的回退配置"
  (message "🔧 应用语言回退配置...")
  ;; 基本的编程模式设置
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  ;; 基本的语法高亮
  (global-font-lock-mode 1))

;; =============================================================================
;; 配置加载主函数 - 健壮版本
;; =============================================================================

(defun my-load-configuration ()
  "按顺序加载所有配置模块，每个模块独立处理失败"
  (message "📋 开始加载Emacs配置...")
  
  ;; 1. 核心配置模块 (基础设置、包管理、编辑增强、快捷键)
  (message "🔧 加载核心配置模块...")
  (my-load-module-with-fallback 'core-config "核心配置" 'my-fallback-basic-settings)
  
  ;; 2. 界面配置模块 (主题、界面组件)
  (message "🎨 加载界面配置模块...")
  (my-load-module-with-fallback 'ui-config "界面配置" 'my-fallback-ui-config)
  
  ;; 3. 补全系统配置模块 (company、ivy等)
  (message "🔤 加载补全系统模块...")
  (my-load-module-with-fallback 'completion-config "补全系统配置" 'my-fallback-completion-config)
  
  ;; 4. 开发工具配置模块 (LSP、项目管理、语法检查等)
  (message "🛠️  加载开发工具模块...")
  (my-load-module-with-fallback 'tools-config "开发工具配置" 'my-fallback-tools-config)
  
  ;; 5. 语言配置模块 (各种编程语言)
  (message "🌐 加载语言配置模块...")
  (my-load-module-with-fallback 'languages-config "语言配置" 'my-fallback-languages-config)
  
  ;; 总结加载结果
  (my-summarize-loading-results))

(defun my-summarize-loading-results ()
  "总结配置加载结果"
  (let ((total-modules 5)
        (loaded-count (length my-loaded-modules))
        (failed-count (length my-failed-modules)))
    
    (message "🎉 配置加载完成！")
    (message "📊 加载统计: %d/%d 模块成功，%d 模块失败" 
             loaded-count total-modules failed-count)
    
    (when my-loaded-modules
      (message "✅ 成功模块: %s" 
               (mapconcat (lambda (m) (symbol-name m)) my-loaded-modules ", ")))
    
    (when my-failed-modules
      (message "⚠️  失败模块: %s" 
               (mapconcat (lambda (m) (symbol-name (car m))) my-failed-modules ", "))
      (message "💡 失败的模块已使用回退配置，基本功能仍可用")
      (message "🔧 可稍后运行 M-x my-config-diagnosis 查看详细信息"))))

;; =============================================================================
;; 最小配置 (版本检查失败时的备用方案)
;; =============================================================================

(defun my-load-minimal-config ()
  "加载最小可用配置"
  (message "📋 加载最小配置...")
  (my-fallback-basic-settings)
  (message "✅ 最小配置加载完成"))

;; =============================================================================
;; 诊断和调试功能
;; =============================================================================

(defun my-config-diagnosis ()
  "诊断配置加载状态"
  (interactive)
  (message "=== Emacs 配置诊断 ===")
  (message "Emacs 版本: %d.%d" emacs-major-version emacs-minor-version)
  (message "配置目录: %s" user-emacs-directory)
  
  ;; 模块加载状态
  (message "=== 模块加载状态 ===")
  (message "成功加载的模块 (%d):" (length my-loaded-modules))
  (dolist (module my-loaded-modules)
    (message "  ✅ %s" module))
  
  (when my-failed-modules
    (message "加载失败的模块 (%d):" (length my-failed-modules))
    (dolist (module-err my-failed-modules)
      (message "  ❌ %s: %s" (car module-err) (error-message-string (cdr module-err)))))
  
  ;; 包管理状态（如果可用）
  (when (boundp 'my-package-setup-success)
    (message "=== 包管理状态 ===")
    (message "包管理系统: %s" (if my-package-setup-success "正常" "降级模式"))
    (when (boundp 'package-activated-list)
      (message "已安装包数量: %d" (length package-activated-list))))
  
  ;; 建议
  (when my-failed-modules
    (message "=== 修复建议 ===")
    (message "1. 检查网络连接并重启 Emacs")
    (message "2. 运行 M-x package-refresh-contents")
    (message "3. 运行 M-x my-package-diagnosis （如果可用）")
    (message "4. 手动安装缺失的包")))

;; =============================================================================
;; 主启动逻辑
;; =============================================================================

(if (my-check-emacs-version)
    (my-load-configuration)
  (my-load-minimal-config))

(provide 'init-config)

;;; init-config.el ends here 