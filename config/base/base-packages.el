;; base/packages.el - 基础包管理配置
;; 包源配置和基础包管理

;; 确保package系统已加载
(require 'package)

;; ------ 清华源 ------
(setq package-archives '(("gnu"   . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("org"   . "https://mirrors.ustc.edu.cn/elpa/org/")))

;; 初始化包系统
(unless (bound-and-true-p package--initialized)
  (package-initialize))

(require 'cl-lib)

;; 1. install use-package
(setq package-selected-packages '(use-package))
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(require 'use-package)
(setq use-package-always-ensure t)

(provide 'base-packages)
