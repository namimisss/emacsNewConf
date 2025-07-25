;; base/packages.el - 基础包管理配置
;; 包源配置和基础包管理

;; 启动时性能优化
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))

;; 更多性能优化
(setq package-enable-at-startup nil)
(setq package-quickstart t)  ; Emacs 27+ 快速启动
(setq use-package-hook-name-suffix nil)  ; 减少 hook 名称处理开销

;; 确保package系统已加载
(require 'package)

;; ------ 清华源 ------
(setq package-archives '(("gnu"   . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("org"   . "https://mirrors.ustc.edu.cn/elpa/org/")))

;; 包系统已在main-config.el中初始化

(require 'cl-lib)

;; 1. install use-package
(setq package-selected-packages '(use-package))
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-expand-minimally t)  ; 减少宏展开开销

;; 启动完成后恢复垃圾回收设置
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold gc-cons-threshold-original)
            (message "Emacs startup completed in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

(provide 'base-packages)
