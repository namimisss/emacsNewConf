;;; tools-treesit.el --- Tree-sitter配置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: configuration, tree-sitter, parsing
;; Version: 1.0.0

;;; Commentary:

;; Tree-sitter 基础配置和语言支持
;; Emacs 29+ 内置了 Tree-sitter 支持

;;; Code:

;; =============================================================================
;; Tree-sitter 基础配置
;; =============================================================================

;; 检查 Tree-sitter 是否可用
(when (treesit-available-p)
  (message "✓ Tree-sitter 已初始化")
  
  ;; =============================================================================
  ;; 语言安装和配置
  ;; =============================================================================
  
  ;; =============================================================================
  ;; 本地Tree-sitter包管理系统
  ;; =============================================================================
  
  ;; 加载支持的语言配置
  (let ((supported-langs-file (expand-file-name "treesit-grammars/supported-langs.el" user-emacs-directory)))
    (when (file-exists-p supported-langs-file)
      (load-file supported-langs-file)))
  
  ;; 目录配置
  (defvar treesit-local-packages-dir
    (expand-file-name "treesit-grammars/packages" user-emacs-directory)
    "本地Tree-sitter包存放目录")
  
  (defvar treesit-install-dir
    (expand-file-name "tree-sitter" user-emacs-directory)
    "Tree-sitter语法安装目录")
  
  ;; =============================================================================
  ;; 本地包安装函数
  ;; =============================================================================
  
  (defun treesit-install-from-local-package (language)
    "检查指定语言的Tree-sitter语法是否已安装"
    (let ((install-path (expand-file-name (format "libtree-sitter-%s.so" language) treesit-install-dir)))
      
      ;; 确保安装目录存在
      (unless (file-exists-p treesit-install-dir)
        (make-directory treesit-install-dir t))
      
      ;; 检查.so文件是否存在
      (if (file-exists-p install-path)
          (progn
            (message "✅ %s 已安装: %s" language install-path)
            t)
        (error "未找到.so文件: %s" install-path))))
  
  (defun treesit-check-available-packages ()
    "检查本地可用的语言包"
    (when (file-exists-p treesit-local-packages-dir)
      (let ((available-packages '()))
        (dolist (lang-config treesit-local-supported-languages)
          (let* ((language (car lang-config))
                 (package-file (cadr lang-config))
                 (package-path (expand-file-name package-file treesit-local-packages-dir)))
            (when (file-exists-p package-path)
              (push language available-packages))))
        available-packages)))
  
  (defun treesit-install-from-local ()
    "从本地包安装所有可用且未安装的Tree-sitter语言"
    (interactive)
    (let ((available-packages (treesit-check-available-packages))
          (success-count 0)
          (skip-count 0)
          (fail-count 0))
      
      (if (not available-packages)
          (message "❌ 未找到可用的本地语言包")
        
        (message "🔍 检测到可用的语言包: %s" available-packages)
        
        (dolist (language available-packages)
          (cond
           ;; 已安装，跳过
           ((treesit-language-available-p language)
            (setq skip-count (1+ skip-count))
            (message "⏩ %s 已安装，跳过" language))
           
           ;; 未安装，尝试安装
           (t
            (message "📦 正在安装 %s..." language)
            (condition-case err
                (progn
                  (treesit-install-from-local-package language)
                  (setq success-count (1+ success-count)))
              (error
               (setq fail-count (1+ fail-count))
               (message "❌ %s 安装失败: %s" language (error-message-string err)))))))
        
        (message "🎉 本地包安装完成: %d 成功, %d 跳过, %d 失败" 
                 success-count skip-count fail-count))))
  
  ;; =============================================================================
  ;; 启动检测和自动安装
  ;; =============================================================================
  
  (defun treesit-startup-check-and-install ()
    "启动时检测并自动从本地包安装Tree-sitter语言"
    (let ((available-packages (treesit-check-available-packages)))
      (cond
       ;; 没有本地包
       ((not available-packages)
        (message "ℹ️  未找到本地Tree-sitter语言包"))
       
       ;; 有本地包，检查是否需要安装
       (t
        (let ((missing-languages '()))
          (dolist (lang available-packages)
            (unless (treesit-language-available-p lang)
              (push lang missing-languages)))
          
          (cond
           ;; 有缺失的语言，自动安装
           (missing-languages
            (message "🔄 检测到缺失的Tree-sitter语言: %s" missing-languages)
            (message "📦 开始从本地包自动安装...")
            (treesit-install-from-local))
           
           ;; 所有语言都已安装
           (t
            (message "✅ Tree-sitter语言已就绪 (%d个)" (length available-packages)))))))))
  
  ;; 启动时自动检测和安装（无延迟）
  (add-hook 'emacs-startup-hook #'treesit-startup-check-and-install)
  
  ;; =============================================================================
  ;; Tree-sitter 模式映射
  ;; =============================================================================
  
  ;; 设置主要模式重映射，优先使用 Tree-sitter 版本
  (setq major-mode-remap-alist
        '((python-mode . python-ts-mode)
          (javascript-mode . js-ts-mode)
          (js2-mode . js-ts-mode)           ; 重要：js2-mode 自动切换到 js-ts-mode
          (typescript-mode . typescript-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (java-mode . java-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (sh-mode . bash-ts-mode)))
  
  ;; =============================================================================
  ;; Tree-sitter 配置优化
  ;; =============================================================================
  
  ;; 增强语法高亮
  (setq treesit-font-lock-level 4)  ; 最详细的语法高亮 (1-4)
  
  ;; Tree-sitter 调试
  (setq treesit-extra-load-path
        (list (expand-file-name "tree-sitter" user-emacs-directory)))
  
  ;; =============================================================================
  ;; 通用 Tree-sitter 配置函数
  ;; =============================================================================
  
  (defun setup-treesit-common ()
    "Tree-sitter 模式通用配置"
    ;; 启用语法高亮
    (when (treesit-parser-list)
      (treesit-font-lock-recompute-features))
    
    ;; 启用Tree-sitter缩进（如果支持）
    (when (fboundp 'treesit-simple-indent-rules-get)
      (setq-local treesit-simple-indent-rules
                  (treesit-simple-indent-rules-get major-mode)))
    
    ;; 启用语法导航（如果支持）
    (when (fboundp 'treesit-defun-type-regexp-get)
      (setq-local treesit-defun-type-regexp
                  (treesit-defun-type-regexp-get major-mode))))
  
  ;; 添加到所有 Tree-sitter 模式
  (dolist (mode '(python-ts-mode-hook
                  js-ts-mode-hook
                  typescript-ts-mode-hook
                  tsx-ts-mode-hook
                  c-ts-mode-hook
                  c++-ts-mode-hook
                  java-ts-mode-hook
                  json-ts-mode-hook
                  css-ts-mode-hook
                  bash-ts-mode-hook))
    (add-hook mode #'setup-treesit-common))
  
  ;; =============================================================================
  ;; Tree-sitter 实用工具
  ;; =============================================================================
  
  (defun treesit-explore-node-at-point ()
    "探索光标处的 Tree-sitter 节点"
    (interactive)
    (when (treesit-parser-list)
      (let ((node (treesit-node-at (point))))
        (if node
            (message "节点类型: %s, 文本: %s"
                     (treesit-node-type node)
                     (treesit-node-text node))
          (message "光标处没有 Tree-sitter 节点")))))
  
  (defun treesit-show-parser-info ()
    "显示当前缓冲区的 Tree-sitter 解析器信息"
    (interactive)
    (if (treesit-parser-list)
        (let ((parsers (treesit-parser-list)))
          (message "当前缓冲区的解析器: %s"
                   (mapconcat (lambda (parser)
                                (format "%s" (treesit-parser-language parser)))
                              parsers ", ")))
      (message "当前缓冲区没有 Tree-sitter 解析器")))
  
  ;; =============================================================================
  ;; 管理命令和键绑定
  ;; =============================================================================
  
  (defun treesit-show-package-status ()
    "显示Tree-sitter包状态信息"
    (interactive)
    (let ((available-packages (treesit-check-available-packages))
          (installed-count 0)
          (available-count 0))
      
      (message "=== Tree-sitter 包状态 ===")
      
      (if (not available-packages)
          (message "❌ 未找到本地语言包")
        
        (setq available-count (length available-packages))
        (dolist (lang available-packages)
          (when (treesit-language-available-p lang)
            (setq installed-count (1+ installed-count))))
        
        (message "📦 本地可用包: %d" available-count)
        (message "✅ 已安装语言: %d" installed-count)
        (message "📋 可用语言: %s" available-packages)
        
        (when (< installed-count available-count)
          (message "💡 运行 M-x treesit-install-from-local 安装缺失语言")))))
  
  (defun treesit-list-supported-languages ()
    "列出所有支持的语言"
    (interactive)
    (if (boundp 'treesit-local-supported-languages)
        (progn
          (message "=== 支持的Tree-sitter语言 ===")
          (dolist (lang-config treesit-local-supported-languages)
            (let ((lang (car lang-config))
                  (file (cadr lang-config))
                  (desc (caddr lang-config)))
              (message "%s: %s (%s)" lang file desc))))
      (message "❌ 语言配置未加载")))
  
  ;; 键绑定
  (global-set-key (kbd "C-c t n") #'treesit-explore-node-at-point)
  (global-set-key (kbd "C-c t i") #'treesit-show-parser-info)
  (global-set-key (kbd "C-c t s") #'treesit-show-package-status)
  (global-set-key (kbd "C-c t l") #'treesit-list-supported-languages)
  (global-set-key (kbd "C-c t I") #'treesit-install-from-local)
  
  ;; =============================================================================
  ;; Tree-sitter 性能监控
  ;; =============================================================================
  
  (defun treesit-benchmark-parsing ()
    "测试 Tree-sitter 解析性能"
    (interactive)
    (when (treesit-parser-list)
      (let ((start-time (current-time)))
        (treesit-buffer-root-node)
        (let ((elapsed (float-time (time-subtract (current-time) start-time))))
          (message "Tree-sitter 解析耗时: %.3f 秒" elapsed)))))
  
  (global-set-key (kbd "C-c t b") #'treesit-benchmark-parsing))

;; 如果 Tree-sitter 不可用的提示
(unless (treesit-available-p)
  (message "Tree-sitter 不可用。请确保使用 Emacs 29+ 并正确编译了 Tree-sitter 支持"))

(provide 'tools-treesit)

;;; tools-treesit.el ends here
