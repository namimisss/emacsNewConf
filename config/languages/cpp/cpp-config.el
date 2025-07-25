;; languages/cpp/cpp-config.el - C++语言配置 (使用clangd，移除irony)

;; C++ 基础配置
(require 'cc-mode)
(add-to-list 'c-default-style '(c++-mode . "k&r"))
(add-to-list 'c-default-style '(c-mode . "k&r"))

;; C++ 相关包
(use-package cmake-mode
  :ensure t)

(use-package demangle-mode
  :ensure t)

(use-package disaster
  :ensure t)

(use-package modern-cpp-font-lock
  :ensure t
  :diminish nil
  :config
  (add-to-list 'modern-c++-attributes "deprecated")
  (setq modern-c++-literal-boolean t)
  (setq modern-c++-literal-string t)
  (setq modern-c++-literal-integer t)
  (setq modern-c++-literal-null-pointer t)
  (setq modern-c++-stl-cstdint t))

(use-package rainbow-delimiters
  :ensure t)

;; 动态检测系统C++标准库路径的函数
(defun j-get-system-include-paths ()
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
  (defun j-clangd-initialization-options ()
    "动态生成clangd初始化选项"
    (let ((include-paths (j-get-system-include-paths)))
      `(:compilationDatabasePath "."
        :fallbackFlags ,(vconcat ["-std=c++17" "-Wall" "-Wextra"]
                                (mapcar (lambda (path) (concat "-I" path)) include-paths))
        :clangdFileStatus t)))
  
  ;; 设置动态初始化选项
  (setq lsp-clients-clangd-initialization-options #'j-clangd-initialization-options)
  
  ;; 启用所有 clangd 特性
  (setq lsp-clangd-binary-path "clangd"
        lsp-clangd-version "18.0.0"))

;; 设置环境变量让 clangd 能找到正确的编译器
(setenv "CC" "/usr/bin/gcc")
(setenv "CXX" "/usr/bin/g++")

;; C/C++ hook函数 (移除irony相关)
(defun j-cc-mode-hook-func()
  (rainbow-delimiters-mode)
  (demangle-mode)
  (modern-c++-font-lock-mode)
  (hs-minor-mode))

;; 添加hook
(add-hook 'c++-mode-hook #'j-cc-mode-hook-func)
(add-hook 'c++-mode-hook #'lsp-deferred)
(add-hook 'c-mode-hook #'j-cc-mode-hook-func)
(add-hook 'c-mode-hook #'lsp-deferred)

;; 基础设置
(setq-default c-basic-offset 4
              tab-width 4)

;; ============================================================================
;; 宏展开功能
;; ============================================================================

;; 使用 C 预处理器展开整个文件的宏
(defun j-preprocess-file ()
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
            (c-mode)
            (goto-char (point-min)))
          (display-buffer output-buffer))
      (message "当前缓冲区没有关联的文件"))))

;; 展开选中区域的宏
(defun j-preprocess-region (start end)
  "展开选中区域的宏"
  (interactive "r")
  (let* ((region-text (buffer-substring-no-properties start end))
         (temp-file (make-temp-file "emacs-cpp-" nil ".c"))
         (output-buffer (get-buffer-create "*Macro Expansion*")))
    (with-temp-file temp-file
      (insert region-text))
    (shell-command (format "gcc -E %s" temp-file) output-buffer)
    (with-current-buffer output-buffer
      (c-mode)
      (goto-char (point-min))
      ;; 删除预处理器生成的行号信息
      (while (re-search-forward "^# [0-9]+ \".*\".*$" nil t)
        (delete-region (line-beginning-position) (1+ (line-end-position))))
      (goto-char (point-min)))
    (display-buffer output-buffer)
    (delete-file temp-file)))

;; 智能宏展开：尝试多种方法
(defun j-smart-expand-macro ()
  "智能宏展开：优先使用 LSP，否则使用预处理器"
  (interactive)
  (cond
   ;; 如果有选中区域，展开选中区域
   ((use-region-p)
    (j-preprocess-region (region-beginning) (region-end)))
   ;; 如果启用了 LSP，尝试展开光标处的宏
   ((bound-and-true-p lsp-mode)
    (condition-case err
        (lsp-clangd-expand-macro)
      (error 
       (message "LSP 宏展开失败，尝试预处理器方法...")
       (j-preprocess-file))))
   ;; 否则展开整个文件
   (t (j-preprocess-file))))

;; 列出当前文件中的所有宏定义
(defun j-list-macros-in-file ()
  "列出当前文件中的所有宏定义"
  (interactive)
  (let ((macro-list '())
        (output-buffer (get-buffer-create "*Macro Definitions*")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#[ \t]*define[ \t]+\\([A-Za-z_][A-Za-z0-9_]*\\)" nil t)
        (let ((macro-name (match-string 1))
              (line-num (line-number-at-pos)))
          (push (cons macro-name line-num) macro-list))))
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert "当前文件中的宏定义:\n")
      (insert "==================\n\n")
      (dolist (macro (reverse macro-list))
        (insert (format "%-20s (第 %d 行)\n" (car macro) (cdr macro))))
      (goto-char (point-min))
      (read-only-mode 1))
    (display-buffer output-buffer)))

;; ============================================================================
;; C++ 模式钩子
;; ============================================================================

(defun setup-cpp-mode ()
  "C++ 模式的设置"
  (setq lsp-enable-semantic-highlighting t)
  
  ;; 设置宏展开键绑定
  (local-set-key (kbd "C-c m e") #'j-smart-expand-macro)
  (local-set-key (kbd "C-c m f") #'j-preprocess-file) 
  (local-set-key (kbd "C-c m r") #'j-preprocess-region)
  (local-set-key (kbd "C-c m l") #'j-list-macros-in-file)
  
  ;; 启用 LSP
  (lsp-deferred))

(add-hook 'c-mode-hook #'setup-cpp-mode)
(add-hook 'c++-mode-hook #'setup-cpp-mode)

;; ============================================================================
;; clangd 编译数据库生成辅助函数
;; ============================================================================

(defun j-generate-compile-commands ()
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
                (message "已生成 compile_commands.json"))))
           ;; 其他项目类型
           (t
            (message "当前项目不是 CMake 项目，请手动配置编译数据库"))))
      (message "未找到项目根目录"))))

(provide 'cpp/cpp-config)
