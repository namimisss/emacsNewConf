;;; tools-treesit-auto.el --- Tree-sitter 自动安装配置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: configuration, tree-sitter, parsing
;; Version: 1.0.0

;;; Commentary:

;; 使用 Emacs 内置的 treesit-install-language-grammar 替代 .so 文件加载
;; 这种方式可以避免 ABI 版本不匹配的问题
;; 特点：首次启动自动安装，后续使用缓存

;;; Code:

;; =============================================================================
;; Tree-sitter 语法源配置
;; =============================================================================

(when (treesit-available-p)
  (message "✓ Tree-sitter auto-install initialized")
  
  ;; 配置语法源地址 (使用兼容 ABI 14 的版本)
  (setq treesit-language-source-alist
        '(;; 核心编程语言 - 使用兼容 ABI 14 的标签/提交
          (c "https://github.com/tree-sitter/tree-sitter-c" "v0.20.7")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.20.3")
          (python "https://github.com/tree-sitter/tree-sitter-python" "v0.20.4")
          (java "https://github.com/tree-sitter/tree-sitter-java" "v0.20.2")
          
          ;; Web 开发 - 使用兼容 ABI 14 的版本
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src")
          (json "https://github.com/tree-sitter/tree-sitter-json" "v0.20.2")
          (css "https://github.com/tree-sitter/tree-sitter-css" "v0.20.0")
          (html "https://github.com/tree-sitter/tree-sitter-html" "v0.20.1")
          
          ;; 脚本和配置 - 使用兼容版本
          (bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.20.4")
          (cmake "https://github.com/uyha/tree-sitter-cmake" "v0.4.1")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "v0.2.0")
          
          ;; 其他语言 - 使用兼容版本
          (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.20.4")
          (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp" "1.3.0")))
  
  ;; =============================================================================
  ;; 自动安装函数
  ;; =============================================================================
  
  (defun treesit-auto-install-language (language)
    "自动安装指定的 Tree-sitter 语言语法"
    (unless (treesit-language-available-p language)
      (message "📦 正在安装 %s 语法..." language)
      (condition-case err
          (progn
            (treesit-install-language-grammar language)
            (message "✅ %s 语法安装成功" language)
            t)
        (error
         (message "❌ %s 语法安装失败: %s" language (error-message-string err))
         nil))))
  
  (defun treesit-install-missing-languages ()
    "只安装缺失的语言语法"
    (interactive)
    (let ((missing-languages '())
          (success-count 0)
          (fail-count 0))
      (dolist (lang-config treesit-language-source-alist)
        (let ((language (car lang-config)))
          (unless (treesit-language-available-p language)
            (push language missing-languages))))
      
      (if missing-languages
          (progn
            (message "🔍 发现缺失的语言: %s" missing-languages)
            (dolist (language missing-languages)
              (if (treesit-auto-install-language language)
                  (setq success-count (1+ success-count))
                (setq fail-count (1+ fail-count))))
            (message "🎉 安装完成: %d 成功, %d 失败" success-count fail-count)
            ;; 安装完成后更新缓存
            (treesit-update-install-cache))
        (message "✅ 所有配置的语言都已安装"))))
  
  ;; =============================================================================
  ;; 启动时自动检测和安装
  ;; =============================================================================
  
  (defun treesit-startup-auto-install ()
    "启动时自动检测并安装缺失的语言语法（缓存优化版本）"
    ;; 使用缓存文件记录安装状态，避免重复检查
    (let* ((cache-file (expand-file-name "treesit-install-cache.el" user-emacs-directory))
           (missing-languages '())
           (cache-exists (file-exists-p cache-file)))
      
      ;; 只在缓存文件不存在时进行检测和安装
      (when (not cache-exists)
        
        (dolist (lang-config treesit-language-source-alist)
          (let ((language (car lang-config)))
            (unless (treesit-language-available-p language)
              (push language missing-languages))))
        
        (cond
         ;; 有缺失的语言，自动安装（不询问）
         (missing-languages
          (message "🔄 检测到 %d 个缺失的 Tree-sitter 语法，开始自动安装..." (length missing-languages))
          (treesit-install-missing-languages)
          (treesit-update-install-cache))
         
         ;; 所有语言都已安装，创建缓存
         (t
          (treesit-update-install-cache)
          (message "✅ All Tree-sitter grammars ready (%d languages)" 
                   (length treesit-language-source-alist)))))
      
      ;; 如果缓存存在，直接使用缓存（快速启动）
      (when cache-exists
        (message "💾 使用 Tree-sitter 缓存，快速启动完成"))))
  
  (defun treesit-update-install-cache ()
    "更新安装缓存文件"
    (let ((cache-file (expand-file-name "treesit-install-cache.el" user-emacs-directory))
          (installed-languages '()))
      
      (dolist (lang-config treesit-language-source-alist)
        (let ((language (car lang-config)))
          (when (treesit-language-available-p language)
            (push language installed-languages))))
      
      (with-temp-file cache-file
        (insert (format ";;; Tree-sitter installation cache - %s\n" (current-time-string)))
        (insert (format "(setq treesit-cached-languages '%S)\n" installed-languages))
        (insert (format "(setq treesit-cache-timestamp %S)\n" (current-time))))
      
      (message "💾 Tree-sitter 缓存已更新 (%d 个语言)" (length installed-languages))))
  
  ;; 启动时快速检测（只在首次缺失时安装）
  (add-hook 'emacs-startup-hook #'treesit-startup-auto-install)
  
  ;; =============================================================================
  ;; 模式映射（与原配置保持一致）
  ;; =============================================================================
  
  (setq major-mode-remap-alist
        '((python-mode . python-ts-mode)
          (javascript-mode . js-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (java-mode . java-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (sh-mode . bash-ts-mode)))
  
  ;; =============================================================================
  ;; 管理命令
  ;; =============================================================================
  
  (defun treesit-show-installation-status ()
    "显示 Tree-sitter 语法安装状态"
    (interactive)
    (let ((total-count (length treesit-language-source-alist))
          (installed-count 0)
          (missing-languages '()))
      
      (message "=== Tree-sitter Grammar Installation Status ===")
      
      (dolist (lang-config treesit-language-source-alist)
        (let ((language (car lang-config)))
          (if (treesit-language-available-p language)
              (progn
                (setq installed-count (1+ installed-count))
                (message "✅ %s: installed" language))
            (progn
              (push language missing-languages)
              (message "❌ %s: not installed" language)))))
      
      (message "📊 Summary: %d/%d installed" installed-count total-count)
      
      (when missing-languages
        (message "💡 Run M-x treesit-install-missing-languages to install missing grammars"))))
  
  (defun treesit-reinstall-language (language)
    "重新安装指定的语言语法"
    (interactive 
     (list (intern (completing-read "选择要重新安装的语言: "
                                   (mapcar (lambda (x) (symbol-name (car x)))
                                           treesit-language-source-alist)))))
    (message "🔄 Reinstalling %s grammar..." language)
    (condition-case err
        (progn
          (treesit-install-language-grammar language t) ; 强制重新安装
          (message "✅ %s grammar reinstalled successfully" language))
      (error
       (message "❌ Failed to reinstall %s grammar: %s" language (error-message-string err)))))
  
  ;; =============================================================================
  ;; 键绑定
  ;; =============================================================================
  
  (global-set-key (kbd "C-c t m") #'treesit-install-missing-languages)
  (global-set-key (kbd "C-c t S") #'treesit-show-installation-status)
  (global-set-key (kbd "C-c t r") #'treesit-reinstall-language)
  
  ;; =============================================================================
  ;; 通用配置（复用原有的设置）
  ;; =============================================================================
  
  ;; 增强语法高亮
  (setq treesit-font-lock-level 4)
  
  ;; 通用 Tree-sitter 配置函数
  (defun setup-treesit-common-auto ()
    "Tree-sitter 模式通用配置（自动安装版本）"
    ;; 启用语法高亮
    (when (treesit-parser-list)
      (treesit-font-lock-recompute-features))
    
    ;; 启用语法导航
    (when (fboundp 'treesit-defun-type-regexp-get)
      (setq-local treesit-defun-type-regexp
                  (treesit-defun-type-regexp-get major-mode)))
    
    ;; 缩进配置
    (electric-indent-local-mode 1)
    (when (and (fboundp 'treesit-indent)
               (treesit-parser-list))
      (setq-local indent-line-function 'treesit-indent))
    
    ;; 键绑定
    (local-set-key (kbd "RET") 'newline-and-indent)
    (local-set-key (kbd "TAB") 'indent-for-tab-command))
  
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
    (add-hook mode #'setup-treesit-common-auto)))

;; 如果 Tree-sitter 不可用的提示
(unless (treesit-available-p)
  (message "Tree-sitter unavailable. Please ensure you're using Emacs 29+ with proper Tree-sitter support compiled"))

(provide 'tools-treesit-auto)

;;; tools-treesit-auto.el ends here
