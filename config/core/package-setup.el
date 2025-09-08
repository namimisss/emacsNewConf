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
;;(setq package-enable-at-startup nil)         ; 禁用启动时自动加载包
(setq package-quickstart t)                  ; Emacs 27+ 快速启动
;;(setq use-package-hook-name-suffix nil)      ; 减少hook名称处理开销
;;(setq use-package-expand-minimally t)        ; 减少宏展开开销

;; =============================================================================
;; 包源配置
;; =============================================================================

;; 配置包源 - 使用国内镜像提高下载速度
;;(setq package-archives '(("gnu"   . "https://mirrors.ustc.edu.cn/elpa/gnu/")
;;                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
;;                         ("org"   . "https://mirrors.ustc.edu.cn/elpa/org/")))
;; tuna
;;(setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
;;                         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
;; tencent
;;(setq package-archives '(("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
;;                         ("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")))
;; ustc
(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))


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

(use-package gnu-elpa-keyring-update
  :ensure t)

(provide 'package-setup)
