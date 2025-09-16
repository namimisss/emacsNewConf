;;; cpp-treesit-config.el --- C++ Tree-sitter配置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: languages, cpp, c++, tree-sitter
;; Version: 1.0.0

;;; Commentary:

;; C++语言配置 (Tree-sitter 版本)，包括：
;; - Tree-sitter 语法支持 (c-ts-mode, c++-ts-mode)
;; - LSP 支持 (clangd)
;; - 现代 C++ 功能
;; - 项目管理和构建工具
;; - 保持向后兼容性

;;; Code:

;; =============================================================================
;; C/C++ Tree-sitter 配置 (优先使用)
;; =============================================================================

;; C Tree-sitter 模式配置
(use-package c-ts-mode
  :ensure nil  ; 内置模式
  :mode "\\.c\\'"
  :hook ((c-ts-mode . lsp-deferred)
         (c-ts-mode . flycheck-mode))
  :config
  ;; C 风格设置
  (setq c-ts-mode-indent-offset 4)
  
  ;; 键绑定设置将在mode hook中处理
  )

;; C++ Tree-sitter 模式配置
(use-package c++-ts-mode
  :ensure nil  ; 内置模式
  :mode (("\\.cpp\\'" . c++-ts-mode)
         ("\\.cxx\\'" . c++-ts-mode)
         ("\\.cc\\'" . c++-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode)
         ("\\.hxx\\'" . c++-ts-mode)
         ("\\.h\\'" . c++-ts-mode))  ; 假设 .h 文件是 C++
  :hook ((c++-ts-mode . lsp-deferred)
         (c++-ts-mode . flycheck-mode))
  :config
  ;; C++ 风格设置
  (setq c++-ts-mode-indent-offset 4)
  
  ;; 键绑定设置将在mode hook中处理
  )

;; =============================================================================
;; C++ 相关包 (Tree-sitter 兼容)
;; =============================================================================

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package demangle-mode
  :ensure t
  :hook ((c-ts-mode . demangle-mode)
         (c++-ts-mode . demangle-mode)))

(use-package disaster
  :ensure t)

;; 现代 C++ 语法高亮（与 Tree-sitter 配合使用）
(use-package modern-cpp-font-lock
  :ensure t
  :hook ((c++-ts-mode . modern-c++-font-lock-mode))
  :config
  (add-to-list 'modern-c++-attributes "deprecated")
  (setq modern-c++-literal-boolean t)
  (setq modern-c++-literal-string t)
  (setq modern-c++-literal-integer t)
  (setq modern-c++-literal-null-pointer t)
  (setq modern-c++-stl-cstdint t))

(use-package rainbow-delimiters
  :ensure t
  :hook ((c-ts-mode . rainbow-delimiters-mode)
         (c++-ts-mode . rainbow-delimiters-mode)))

;; =============================================================================
;; clangd LSP 配置 (Tree-sitter 兼容)
;; =============================================================================

;; 动态检测系统C++标准库路径的函数
(defun my-get-system-include-paths ()
  "动态获取系统的C++标准库路径"
  (let ((output (shell-command-to-string "echo | g++ -v -x c++ - 2>&1 | sed -n '/^#include <\\.\\.\\.> search starts here:/,/^End of search list/p' | grep '^[[:space:]]\\+/' | sed 's/^[[:space:]]\\+//'")))
    (seq-filter (lambda (path)
                  (and path (not (string-empty-p path))))
                (split-string output "\n" t))))

;; clangd配置 - 自动发现标准库路径
(with-eval-after-load 'lsp-mode
  ;; clangd 参数配置 - 精简配置，让 clangd 自动发现标准库
  (setq lsp-clients-clangd-args '("-j=20"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--header-insertion=iwyu"
                                  "--log=error"
                                  "--query-driver=/usr/bin/g++,/usr/bin/gcc,/usr/bin/c++,/usr/bin/clang++"
                                  "--enable-config"))
  
  ;; 强制LSP使用clangd
  (setq lsp-clients-cc-providers '(clangd))
  (setq lsp-clients-clangd-executable "clangd")
  
  ;; 启用 clangd 的语义高亮功能
  (setq lsp-clangd-binary-path "clangd")
  
  ;; 让 clangd 自动检测编译数据库
  (setq lsp-auto-guess-root t)
  
  ;; 动态设置初始化选项，包含系统路径
  (defun my-clangd-initialization-options ()
    "动态生成clangd初始化选项"
    (let ((include-paths (my-get-system-include-paths)))
      `(:compilationDatabasePath "."
        :fallbackFlags ,(vconcat ["-std=c++17" "-Wall" "-Wextra"]
                                (mapcar (lambda (path) (concat "-I" path)) include-paths))
        :clangdFileStatus t)))
  
  ;; 设置动态初始化选项
  (setq lsp-clients-clangd-initialization-options #'my-clangd-initialization-options)
  
  ;; 启用所有 clangd 特性
  (setq lsp-clangd-binary-path "clangd"
        lsp-clangd-version "18.0.0"))

;; 设置环境变量让 clangd 能找到正确的编译器
(setenv "CC" "/usr/bin/gcc")
(setenv "CXX" "/usr/bin/g++")

;; =============================================================================
;; C++ 代码格式化
;; =============================================================================

;; C++ 代码格式化函数
(defun my-cpp-format-buffer ()
  "使用 clang-format 格式化 C++ 代码"
  (interactive)
  (if (executable-find "clang-format")
      (progn
        (call-process "clang-format" nil nil nil "-i" (buffer-file-name))
        (revert-buffer t t t)
        (message "Formatting completed with clang-format"))
    (message "Please install clang-format")))

;; 保存时自动格式化（可选）
(defun my-cpp-format-on-save ()
  "C++ 文件保存时自动格式化"
  (when (and (derived-mode-p 'c-ts-mode 'c++-ts-mode)
             (executable-find "clang-format"))
    (my-cpp-format-buffer)))

;; 如果需要保存时自动格式化，取消注释下面这行
;; (add-hook 'before-save-hook 'my-cpp-format-on-save)

;; =============================================================================
;; 宏展开功能 (Tree-sitter 兼容)
;; =============================================================================

;; 使用 C 预处理器展开整个文件的宏
(defun my-preprocess-file ()
  "使用 C 预处理器展开当前文件中的所有宏"
  (interactive)
  (let* ((file-name (buffer-file-name))
         (output-buffer (get-buffer-create "*C Preprocessor Output*"))
         (cpp-command (cond
                       ((string-match "\\.cpp\\|\\." file-name) "g++ -E")
                       ((string-match "\\.c\\'" file-name) "gcc -E")
                       (t "cpp"))))
    (if file-name
        (progn
          (shell-command (format "%s %s" cpp-command file-name) output-buffer)
          (with-current-buffer output-buffer
            (c++-ts-mode)  ; 使用 Tree-sitter 模式
            (goto-char (point-min)))
          (display-buffer output-buffer))
      (message "Current buffer has no associated file"))))

;; 展开选中区域的宏
(defun my-preprocess-region (start end)
  "展开选中区域的宏"
  (interactive "r")
  (let* ((region-text (buffer-substring-no-properties start end))
         (temp-file (make-temp-file "emacs-cpp-" nil ".c"))
         (output-buffer (get-buffer-create "*Macro Expansion*")))
    (with-temp-file temp-file
      (insert region-text))
    (shell-command (format "gcc -E %s" temp-file) output-buffer)
    (with-current-buffer output-buffer
      (c++-ts-mode)  ; 使用 Tree-sitter 模式
      (goto-char (point-min))
      ;; 删除预处理器生成的行号信息
      (while (re-search-forward "^# [0-9]+ \".*\".*$" nil t)
        (delete-region (line-beginning-position) (1+ (line-end-position))))
      (goto-char (point-min)))
    (display-buffer output-buffer)
    (delete-file temp-file)))

;; 智能宏展开：尝试多种方法
(defun my-cpp-expand-macro ()
  "智能宏展开：优先使用 LSP，否则使用预处理器"
  (interactive)
  (cond
   ;; 如果有选中区域，展开选中区域
   ((use-region-p)
    (my-preprocess-region (region-beginning) (region-end)))
   ;; 如果启用了 LSP，尝试展开光标处的宏
   ((bound-and-true-p lsp-mode)
    (condition-case err
        (lsp-clangd-expand-macro)
      (error 
       (message "LSP macro expansion failed, trying preprocessor method...")
       (my-preprocess-file))))
   ;; 否则展开整个文件
   (t (my-preprocess-file))))

;; =============================================================================
;; Tree-sitter 特定增强
;; =============================================================================

;; C++ Tree-sitter 特定设置
(defun my-cpp-treesit-setup ()
  "C++ Tree-sitter 特定设置"
  (when (treesit-parser-list)
    ;; 启用语法感知的导航
    (setq-local treesit-defun-type-regexp
                (rx (or "function_definition"
                        "method_definition"
                        "class_specifier"
                        "struct_specifier"
                        "namespace_definition")))
    
    ;; 启用更好的缩进
    (setq-local treesit-simple-indent-rules
                (when (fboundp 'treesit-simple-indent-rules-get)
                  (treesit-simple-indent-rules-get major-mode)))
    
    ;; C++ 特定的 Tree-sitter 功能
    (setq-local treesit-font-lock-level 4)))  ; 最详细的语法高亮

;; C Tree-sitter 特定设置
(defun my-c-treesit-setup ()
  "C Tree-sitter 特定设置"
  (when (treesit-parser-list)
    ;; 启用语法感知的导航
    (setq-local treesit-defun-type-regexp
                (rx (or "function_definition"
                        "struct_specifier")))
    
    ;; 启用更好的缩进
    (setq-local treesit-simple-indent-rules
                (when (fboundp 'treesit-simple-indent-rules-get)
                  (treesit-simple-indent-rules-get major-mode)))
    
    ;; C 特定的 Tree-sitter 功能
    (setq-local treesit-font-lock-level 4)))

;; 添加到 C/C++ Tree-sitter 模式
(add-hook 'c-ts-mode-hook #'my-c-treesit-setup)
(add-hook 'c++-ts-mode-hook #'my-cpp-treesit-setup)

;; =============================================================================
;; C++ 项目管理和构建工具
;; =============================================================================

;; CMake 项目支持
(defun my-cmake-build ()
  "构建 CMake 项目"
  (interactive)
  (let ((build-dir (expand-file-name "build" (lsp-workspace-root))))
    (unless (file-directory-p build-dir)
      (make-directory build-dir))
    (compile (format "cd %s && cmake .. && make -j$(nproc)" build-dir))))

;; clangd 编译数据库生成辅助函数
(defun my-generate-compile-commands ()
  "为当前项目生成 compile_commands.json"
  (interactive)
  (let ((project-root (lsp-workspace-root)))
    (if project-root
        (let ((cmake-dir (file-name-as-directory (expand-file-name "build" project-root)))
              (has-cmake (file-exists-p (expand-file-name "CMakeLists.txt" project-root))))
          (cond
           ;; CMake 项目
           (has-cmake
            (unless (file-directory-p cmake-dir)
              (make-directory cmake-dir))
            (let ((default-directory cmake-dir))
              (shell-command "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..")
              (when (file-exists-p "compile_commands.json")
                (copy-file "compile_commands.json" 
                          (expand-file-name "compile_commands.json" project-root) t)
                (message "Generated compile_commands.json"))))
           ;; 其他项目类型
           (t
            (message "Current project is not a CMake project, please configure compilation database manually"))))
      (message "Project root directory not found"))))

;; C++ 开发常用命令
(defun my-cpp-run-current-file ()
  "编译并运行当前 C++ 文件"
  (interactive)
  (let* ((file-name (buffer-file-name))
         (exe-name (file-name-sans-extension file-name)))
    (compile (format "g++ -std=c++17 -o %s %s && %s" exe-name file-name exe-name))))

;; =============================================================================
;; C++ 通用配置
;; =============================================================================

;; C/C++ hook函数
(defun my-cc-ts-mode-hook-func()
  "C/C++ Tree-sitter 模式的通用设置"
  (rainbow-delimiters-mode)
  (demangle-mode)
  (when (derived-mode-p 'c++-ts-mode)
    (modern-c++-font-lock-mode))
  (hs-minor-mode)
  
  ;; 设置键绑定
  (local-set-key (kbd "C-c c b") #'my-cmake-build)
  (local-set-key (kbd "C-c c r") #'my-cpp-run-current-file)
  (local-set-key (kbd "C-c c g") #'my-generate-compile-commands))

;; 添加hook
(add-hook 'c++-ts-mode-hook #'my-cc-ts-mode-hook-func)
(add-hook 'c-ts-mode-hook #'my-cc-ts-mode-hook-func)

;; 基础设置
(setq-default c-basic-offset 4
              tab-width 4)

;; =============================================================================
;; 向后兼容性支持 (备选模式)
;; =============================================================================

;; 保留传统 cc-mode 作为备选
(use-package cc-mode
  :ensure nil
  :disabled t  ; 默认禁用，优先使用 Tree-sitter
  :config
  (add-to-list 'c-default-style '(c++-mode . "k&r"))
  (add-to-list 'c-default-style '(c-mode . "k&r")))

;; =============================================================================
;; C++ 开发辅助工具
;; =============================================================================

;; C++ 代码模板
(defun my-cpp-create-class-template (class-name)
  "创建 C++ 类模板"
  (interactive "s类名: ")
  (let ((header-file (concat class-name ".hpp"))
        (source-file (concat class-name ".cpp")))
    ;; 创建头文件
    (with-temp-file header-file
      (insert (format "#pragma once\n\nclass %s {\npublic:\n    %s();\n    ~%s();\n\nprivate:\n    // 私有成员\n};\n" 
                      class-name class-name class-name)))
    ;; 创建源文件
    (with-temp-file source-file
      (insert (format "#include \"%s\"\n\n%s::%s() {\n    // 构造函数实现\n}\n\n%s::~%s() {\n    // 析构函数实现\n}\n" 
                      header-file class-name class-name class-name class-name)))
    (message "C++ class template created: %s.hpp and %s.cpp" class-name class-name)))

;; 设置键绑定的函数
(defun my-cpp-ts-setup-keybindings ()
  "设置C/C++ Tree-sitter模式的键绑定"
  (when (derived-mode-p 'c-ts-mode 'c++-ts-mode)
    (let ((mode-map (if (derived-mode-p 'c++-ts-mode) c++-ts-mode-map c-ts-mode-map)))
      (define-key mode-map (kbd "M-.") 'lsp-find-definition)
      (define-key mode-map (kbd "M-,") 'lsp-find-references)
      (define-key mode-map (kbd "C-c C-r") 'lsp-rename)
      (define-key mode-map (kbd "C-c m e") 'my-cpp-expand-macro)
      (define-key mode-map (kbd "C-c c f") 'my-cpp-format-buffer))))

;; 添加到hook中
(add-hook 'c-ts-mode-hook #'my-cpp-ts-setup-keybindings)
(add-hook 'c++-ts-mode-hook #'my-cpp-ts-setup-keybindings)

(provide 'cpp-treesit-config)

;;; cpp-treesit-config.el ends here
