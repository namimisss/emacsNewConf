;;; python-treesit-config.el --- Python Tree-sitter配置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: languages, python, tree-sitter
;; Version: 1.0.0

;;; Commentary:

;; Python 开发配置 (Tree-sitter 版本)，包括：
;; - Tree-sitter 语法支持 (python-ts-mode)
;; - LSP 语言服务器支持 
;; - 虚拟环境管理
;; - 代码格式化支持
;; - 保持向后兼容性

;;; Code:

;; =============================================================================
;; Python Tree-sitter 配置 (优先使用)
;; =============================================================================

;; Python Tree-sitter 模式配置
(use-package python-ts-mode
  :ensure nil  ; 内置模式
  :mode "\\.py\\'"
  :interpreter ("python" "python3")
  :hook ((python-ts-mode . lsp-deferred)
         (python-ts-mode . flycheck-mode))
  :config
  ;; Python 缩进设置
  (setq python-indent-offset 4)
  (setq python-shell-interpreter "python3")
  
  ;; 键绑定设置将在mode hook中处理
  )

;; =============================================================================
;; Python 虚拟环境管理
;; =============================================================================

;; Pyvenv 虚拟环境管理
(use-package pyvenv
  :ensure t
  :hook (python-ts-mode . pyvenv-mode)
  :config
  ;; 设置使用系统Python环境
  (setq pyvenv-workon "sys")
  
  ;; 自动检测和激活虚拟环境
  (defun my-pyvenv-auto-activate ()
    "自动检测并激活虚拟环境"
    (let ((venv-dir (locate-dominating-file default-directory "venv")))
      (when venv-dir
        (pyvenv-activate (expand-file-name "venv" venv-dir)))))
  
  (add-hook 'python-ts-mode-hook #'my-pyvenv-auto-activate))

;; =============================================================================
;; Python 代码格式化
;; =============================================================================

;; 代码格式化函数
(defun my-python-format-buffer ()
  "使用 black 或 autopep8 格式化 Python 代码"
  (interactive)
  (cond
   ((executable-find "black")
    (call-process "black" nil nil nil "--quiet" (buffer-file-name))
    (revert-buffer t t t)
    (message "使用 black 格式化完成"))
   ((executable-find "autopep8")
    (call-process "autopep8" nil nil nil "--in-place" "--aggressive" 
                  (buffer-file-name))
    (revert-buffer t t t)
    (message "使用 autopep8 格式化完成"))
   (t (message "请安装 black 或 autopep8 进行代码格式化"))))

;; 保存时自动格式化（可选）
(defun my-python-format-on-save ()
  "Python 文件保存时自动格式化"
  (when (and (derived-mode-p 'python-ts-mode)
             (or (executable-find "black") (executable-find "autopep8")))
    (my-python-format-buffer)))

;; 如果需要保存时自动格式化，取消注释下面这行
;; (add-hook 'before-save-hook 'my-python-format-on-save)

;; =============================================================================
;; Python LSP 特定配置
;; =============================================================================

;; Python LSP 增强配置
(with-eval-after-load 'lsp-mode
  ;; Python LSP 服务器配置
  (setq lsp-python-ms-auto-install-server t)
  
  ;; Pylsp 配置（如果使用 python-lsp-server）
  (setq lsp-pylsp-plugins-flake8-enabled t)
  (setq lsp-pylsp-plugins-pycodestyle-enabled nil)  ; 避免与 flake8 冲突
  (setq lsp-pylsp-plugins-autopep8-enabled nil)     ; 使用外部格式化工具
  (setq lsp-pylsp-plugins-yapf-enabled nil)
  
  ;; Pyright 配置（如果使用 pyright）
  (setq lsp-pyright-auto-import-completions t)
  (setq lsp-pyright-auto-search-paths t))

;; =============================================================================
;; Python 开发工具集成
;; =============================================================================

;; Python 测试集成
(defun my-python-run-pytest ()
  "运行 pytest"
  (interactive)
  (if (executable-find "pytest")
      (compile "pytest -v")
    (message "请安装 pytest")))

(defun my-python-run-current-test ()
  "运行当前文件的测试"
  (interactive)
  (if (executable-find "pytest")
      (compile (format "pytest -v %s" (buffer-file-name)))
    (message "请安装 pytest")))

;; Python 调试支持
(defun my-python-debug-current-file ()
  "使用 pdb 调试当前文件"
  (interactive)
  (compile (format "python -m pdb %s" (buffer-file-name))))

;; =============================================================================
;; Tree-sitter 特定增强
;; =============================================================================

;; Python Tree-sitter 特定设置
(defun my-python-treesit-setup ()
  "Python Tree-sitter 特定设置"
  (when (treesit-parser-list)
    ;; 启用语法感知的导航
    (setq-local treesit-defun-type-regexp
                (rx (or "function_definition"
                        "class_definition"
                        "method_definition")))
    
    ;; Python 特定的 Tree-sitter 功能
    (setq-local treesit-font-lock-level 4)))  ; 最详细的语法高亮

;; 添加到 Python Tree-sitter 模式
(add-hook 'python-ts-mode-hook #'my-python-treesit-setup)

;; =============================================================================
;; Python 包管理工具集成
;; =============================================================================

;; pip 工具集成
(defun my-python-pip-install (package)
  "安装 Python 包"
  (interactive "sPackage name: ")
  (compile (format "pip install %s" package)))

(defun my-python-pip-list ()
  "列出已安装的 Python 包"
  (interactive)
  (compile "pip list"))

(defun my-python-requirements-install ()
  "从 requirements.txt 安装依赖"
  (interactive)
  (if (file-exists-p "requirements.txt")
      (compile "pip install -r requirements.txt")
    (message "当前目录中没有找到 requirements.txt")))

;; Poetry 集成（如果使用 Poetry）
(defun my-python-poetry-install ()
  "使用 Poetry 安装依赖"
  (interactive)
  (if (file-exists-p "pyproject.toml")
      (compile "poetry install")
    (message "当前目录中没有找到 pyproject.toml")))

;; =============================================================================
;; Python REPL 增强
;; =============================================================================

;; 增强的 Python REPL 配置
(defun my-python-setup-repl ()
  "设置 Python REPL"
  (setq python-shell-interpreter "python3")
  (setq python-shell-interpreter-args "-i")
  (setq python-shell-prompt-detect-failure-warning nil)
  (setq python-shell-completion-native-enable nil))

(add-hook 'python-ts-mode-hook #'my-python-setup-repl)

;; =============================================================================
;; 向后兼容性支持 (备选模式)
;; =============================================================================

;; 保留 elpy 作为备选（如果需要完整的 Python IDE 功能）
(use-package elpy
  :ensure t
  :disabled t  ; 默认禁用，优先使用 Tree-sitter + LSP
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

;; 保留传统 python-mode 作为备选
(use-package python-mode
  :ensure t
  :disabled t  ; 默认禁用，优先使用 Tree-sitter
  :config
  (setq python-indent-offset 4))

;; =============================================================================
;; Python 文档和帮助
;; =============================================================================

;; Python 文档查看
(defun my-python-doc-at-point ()
  "查看光标处符号的文档"
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (lsp-describe-thing-at-point)
    (python-describe-at-point)))

;; 设置键绑定的函数
(defun my-python-ts-setup-keybindings ()
  "设置Python Tree-sitter模式的键绑定"
  (define-key python-ts-mode-map (kbd "M-.") 'lsp-find-definition)
  (define-key python-ts-mode-map (kbd "M-,") 'lsp-find-references)
  (define-key python-ts-mode-map (kbd "C-c C-r") 'lsp-rename)
  (define-key python-ts-mode-map (kbd "C-c C-f") 'my-python-format-buffer)
  (define-key python-ts-mode-map (kbd "C-c t t") 'my-python-run-pytest)
  (define-key python-ts-mode-map (kbd "C-c t f") 'my-python-run-current-test)
  (define-key python-ts-mode-map (kbd "C-c d d") 'my-python-debug-current-file)
  (define-key python-ts-mode-map (kbd "C-c h h") 'my-python-doc-at-point))

;; 添加到hook中
(add-hook 'python-ts-mode-hook #'my-python-ts-setup-keybindings)

;; =============================================================================
;; Python 项目模板
;; =============================================================================

(defun my-python-create-project-structure ()
  "创建标准的 Python 项目结构"
  (interactive)
  (let ((project-name (read-string "项目名称: ")))
    (make-directory project-name)
    (make-directory (concat project-name "/src"))
    (make-directory (concat project-name "/tests"))
    (make-directory (concat project-name "/docs"))
    (with-temp-file (concat project-name "/README.md")
      (insert (format "# %s\n\n项目描述\n" project-name)))
    (with-temp-file (concat project-name "/requirements.txt")
      (insert "# Python 依赖列表\n"))
    (with-temp-file (concat project-name "/.gitignore")
      (insert "__pycache__/\n*.pyc\n*.pyo\n*.pyd\n.Python\nvenv/\n.env\n"))
    (message "Python 项目结构创建完成: %s" project-name)))

(provide 'python-treesit-config)

;;; python-treesit-config.el ends here
