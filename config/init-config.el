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

;; 配置加载主函数
(defun my-load-configuration ()
  "按顺序加载所有配置模块"
  (condition-case err
      (progn
        (message "📋 开始加载Emacs配置...")
        
        ;; 1. 核心配置模块 (基础设置、包管理、编辑增强、快捷键)
        (message "🔧 加载核心配置模块...")
        (require 'core-config)
        
        ;; 2. 界面配置模块 (主题、界面组件)
        (message "🎨 加载界面配置模块...")
        (require 'ui-config)
        
        ;; 3. 补全系统配置模块 (company、ivy等)
        (message "🔤 加载补全系统模块...")
        (require 'completion-config)
        
        ;; 4. 开发工具配置模块 (LSP、项目管理、语法检查等)
        (message "🛠️  加载开发工具模块...")
        (require 'tools-config)
        
        ;; 5. 语言配置模块 (各种编程语言)
        (message "🌐 加载语言配置模块...")
        (require 'languages-config)
        
        (message "🎉 配置加载完成！"))
    (error 
     (message "❌ 配置加载失败: %s" (error-message-string err))
     (message "🔧 将使用最小配置运行")
     ;; 最小配置
     (my-load-minimal-config))))

;; 最小配置 (配置加载失败时的备用方案)
(defun my-load-minimal-config ()
  "加载最小可用配置"
  (message "📋 加载最小配置...")
  ;; 基本界面设置
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  ;; 基本编辑功能
  (global-display-line-numbers-mode 1)
  (column-number-mode 1)
  (show-paren-mode 1)
  (delete-selection-mode 1)
  (message "✅ 最小配置加载完成"))

;; 主启动逻辑
(if (my-check-emacs-version)
    (my-load-configuration)
  (my-load-minimal-config))

(provide 'init-config) 