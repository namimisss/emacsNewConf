;; =============================================================================
;; config/core/core-config.el - 核心配置模块入口
;; =============================================================================
;; 此文件是core目录下所有配置的统一入口

(defvar my-core-loaded-modules '()
  "核心模块中成功加载的子模块列表")

(defun my-safe-require-core (module description)
  "安全地加载核心子模块"
  (condition-case err
      (progn
        (require module)
        (add-to-list 'my-core-loaded-modules module)
        (message "  ✅ %s 加载成功" description)
        t)
    (error
     (message "  ⚠️  %s 加载失败: %s" description (error-message-string err))
     nil)))

;; 1. 基础Emacs设置 (不依赖任何包，必须最先加载)
(my-safe-require-core 'basic-settings "基础设置")

;; 2. 包管理系统 (必须在其他包配置之前加载)
(my-safe-require-core 'package-setup "包管理系统")

;; 3. 编辑增强功能 (依赖use-package，但有回退)
(unless (my-safe-require-core 'base-enhancements "编辑增强功能")
  (message "  🔧 编辑增强功能使用内置替代"))

;; 4. 全局快捷键 (最后加载，确保所有函数都已定义)
(my-safe-require-core 'keybindings "全局快捷键")

;; 核心模块加载总结
(let ((loaded-count (length my-core-loaded-modules))
      (total-count 4))
  (message "🔧 核心模块加载完成: %d/%d" loaded-count total-count))

;; 提供core配置入口
(provide 'core-config) 