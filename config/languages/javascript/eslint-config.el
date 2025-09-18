;;; eslint-config.el --- ESLint 统一配置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: languages, javascript, eslint, flycheck
;; Version: 1.0.0

;;; Commentary:

;; ESLint 与 Flycheck 集成的统一配置
;; 这个文件被传统版本和 Tree-sitter 版本共同使用，避免重复配置

;;; Code:

;; ESLint 与 Flycheck 集成配置
;; 这是 JavaScript 开发的核心错误检查配置

(defvar eslint-config-loaded nil
  "防止重复加载 ESLint 配置")

(unless eslint-config-loaded
  (with-eval-after-load 'flycheck
    ;; 禁用一些不需要的 JavaScript 检查器（防止重复添加）
    (unless (member 'javascript-jshint flycheck-disabled-checkers)
      (setq-default flycheck-disabled-checkers
                    (append flycheck-disabled-checkers
                            '(javascript-jshint json-jsonlist))))
    
    ;; 优先使用项目本地的 ESLint
    (defun my-use-eslint-from-node-modules ()
      "使用项目本地的 ESLint
这个函数会在 flycheck-mode 启动时自动检测项目目录下的 ESLint，
优先使用项目本地版本而不是全局版本，确保使用正确的规则配置。"
      (let* ((root (locate-dominating-file
                    (or (buffer-file-name) default-directory)
                    "node_modules"))
             (eslint (and root
                          (expand-file-name "node_modules/.bin/eslint"
                                            root))))
        (when (and eslint (file-executable-p eslint))
          (setq-local flycheck-javascript-eslint-executable eslint)
          (message "Using local ESLint: %s" eslint))))
    
    ;; 添加到 flycheck-mode-hook
    (add-hook 'flycheck-mode-hook #'my-use-eslint-from-node-modules)
    
    ;; 设置已加载标志
    (setq eslint-config-loaded t)
    
    (message "✓ ESLint configuration loaded and integrated with Flycheck")))

;; ESLint 相关的实用函数

(defun eslint-config-check-setup ()
  "检查 ESLint 配置状态"
  (interactive)
  (let ((eslint-global (executable-find "eslint"))
        (eslint-local (when (buffer-file-name)
                        (let* ((root (locate-dominating-file
                                      (buffer-file-name)
                                      "node_modules"))
                               (local-eslint (and root
                                                  (expand-file-name "node_modules/.bin/eslint"
                                                                    root))))
                          (when (and local-eslint (file-executable-p local-eslint))
                            local-eslint))))
        (config-files (when (buffer-file-name)
                        (let ((root (locate-dominating-file
                                     (buffer-file-name)
                                     (lambda (dir)
                                       (or (file-exists-p (expand-file-name ".eslintrc.js" dir))
                                           (file-exists-p (expand-file-name ".eslintrc.json" dir))
                                           (file-exists-p (expand-file-name ".eslintrc" dir))
                                           (file-exists-p (expand-file-name "package.json" dir)))))))
                          (when root
                            (remove nil
                                    (list
                                     (when (file-exists-p (expand-file-name ".eslintrc.js" root))
                                       ".eslintrc.js")
                                     (when (file-exists-p (expand-file-name ".eslintrc.json" root))
                                       ".eslintrc.json")
                                     (when (file-exists-p (expand-file-name ".eslintrc" root))
                                       ".eslintrc")
                                     (when (file-exists-p (expand-file-name "package.json" root))
                                       "package.json"))))))))
    
    (message "=== ESLint Setup Status ===")
    (message "Global ESLint: %s" (or eslint-global "Not found"))
    (message "Local ESLint: %s" (or eslint-local "Not found"))
    (message "Config files: %s" (if config-files 
                                     (string-join config-files ", ")
                                   "None found"))
    (message "Flycheck ESLint: %s" (if (bound-and-true-p flycheck-javascript-eslint-executable)
                                       flycheck-javascript-eslint-executable
                                     "Default"))
    (when (and eslint-local config-files)
      (message "✓ ESLint properly configured"))))

(provide 'eslint-config)

;;; eslint-config.el ends here
