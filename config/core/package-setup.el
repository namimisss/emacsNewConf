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

;; 初始化包系统
(package-initialize)

;; =============================================================================
;; use-package 安装和配置
;; =============================================================================

;; 确保 cl-lib 可用 (兼容性)
(require 'cl-lib)

;; 安装 use-package (如果没有安装)
(setq package-selected-packages '(use-package))
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; 加载并配置 use-package
(require 'use-package)
(setq use-package-always-ensure t)           ; 自动下载包
(setq use-package-verbose t)                 ; 显示加载信息
(setq use-package-compute-statistics t)      ; 统计加载时间

;; =============================================================================
;; 包管理辅助函数
;; =============================================================================

(defun my-package-installed-p (package)
  "检查包是否已安装"
  (package-installed-p package))

(defun my-package-install (package)
  "安装指定的包"
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

(defun my-package-upgrade-all ()
  "升级所有已安装的包"
  (interactive)
  (package-refresh-contents)
  (let ((upgrades (package-menu--find-upgrades)))
    (if upgrades
        (progn
          (message "发现 %d 个包需要升级..." (length upgrades))
          (dolist (upgrade upgrades)
            (package-install (car upgrade)))
          (message "包升级完成！"))
      (message "所有包都是最新版本"))))

(defun my-package-list-installed ()
  "列出所有已安装的包"
  (interactive)
  (with-output-to-temp-buffer "*Installed Packages*"
    (dolist (pkg package-activated-list)
      (princ (format "%s\n" pkg)))))

;; =============================================================================
;; 包目录管理
;; =============================================================================

;; 创建必要的目录
(let ((dirs '("~/.emacs.d/elpa"
              "~/.emacs.d/backups"
              "~/.emacs.d/auto-save-list"
              "~/.emacs.d/undo")))
  (dolist (dir dirs)
    (unless (file-directory-p dir)
      (make-directory dir t))))

;; =============================================================================
;; 依赖检查
;; =============================================================================

(defun my-check-dependencies ()
  "检查关键依赖是否可用"
  (let ((required-packages '(use-package)))
    (dolist (pkg required-packages)
      (unless (package-installed-p pkg)
        (error "关键包 %s 未安装！请检查包源配置" pkg)))))

;; 执行依赖检查
(my-check-dependencies)

;; =============================================================================
;; 自动清理
;; =============================================================================

;; 启动时清理无用的包
(defun my-package-auto-clean ()
  "自动清理未使用的包"
  (when (bound-and-true-p package-selected-packages)
    (package-autoremove)))

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
  (message "包管理系统状态:")
  (message "- 包源数量: %d" (length package-archives))
  (message "- 已安装包数量: %d" (length package-activated-list))
  (message "- use-package统计: %s" 
           (if use-package-compute-statistics "已启用" "已禁用"))
  (when use-package-compute-statistics
    (message "- 可使用 M-x use-package-report 查看详细统计")))

(provide 'package-setup) 