;;; volar-config.el --- Vue.js Volar LSP 配置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: languages, vue, volar, lsp
;; Version: 1.0.0

;;; Commentary:

;; Vue.js Volar LSP 服务器配置
;; 这是公共配置，不区分 Tree-sitter 模式，适用于所有 JavaScript 配置

;;; Code:

;; =============================================================================
;; Vue 项目检测
;; =============================================================================

(defun my-is-vue-project ()
  "检测当前是否为Vue项目"
  (when-let ((root (and (fboundp 'lsp-workspace-root) (lsp-workspace-root))))
    (or (file-exists-p (expand-file-name "vue.config.js" root))
        (file-exists-p (expand-file-name "vite.config.js" root))
        (file-exists-p (expand-file-name "nuxt.config.js" root))
        (and (file-exists-p (expand-file-name "package.json" root))
             (with-temp-buffer
               (insert-file-contents (expand-file-name "package.json" root))
               (goto-char (point-min))
               (re-search-forward "\"vue\"\\|\"@vue\"\\|\"nuxt\"" nil t))))))

;; =============================================================================
;; TypeScript 库路径动态检测
;; =============================================================================

(defun my-find-typescript-lib ()
  "动态查找 TypeScript 库路径"
  (or 
   ;; 1. 优先使用项目本地的 TypeScript
   (when-let ((root (and (fboundp 'lsp-workspace-root) (lsp-workspace-root))))
     (let ((local-ts (expand-file-name "node_modules/typescript/lib/tsserverlibrary.js" root)))
       (when (file-exists-p local-ts) local-ts)))
   
   ;; 2. 通过 npm 命令查找全局 TypeScript
   (when-let ((npm-global-root (ignore-errors 
                                 (string-trim (shell-command-to-string "npm root -g 2>/dev/null")))))
     (let ((global-ts (expand-file-name "typescript/lib/tsserverlibrary.js" npm-global-root)))
       (when (file-exists-p global-ts) global-ts)))
   
   ;; 3. 通过 which typescript-language-server 推断路径
   (when-let ((tslsp-path (executable-find "typescript-language-server")))
     (let* ((tslsp-dir (file-name-directory tslsp-path))
            ;; 假设 typescript-language-server 和 typescript 在同一个 node_modules
            (possible-paths (list
                             ;; 全局安装的情况
                             (expand-file-name "../lib/node_modules/typescript/lib/tsserverlibrary.js" tslsp-dir)
                             ;; npm 全局目录结构
                             (expand-file-name "../../typescript/lib/tsserverlibrary.js" tslsp-dir)
                             ;; 其他可能的路径
                             (expand-file-name "../typescript/lib/tsserverlibrary.js" tslsp-dir))))
       (seq-find #'file-exists-p possible-paths)))
   
   ;; 4. 通过 node 查找 TypeScript 模块
   (ignore-errors
     (let ((ts-path (string-trim 
                     (shell-command-to-string 
                      "node -e \"try { console.log(require.resolve('typescript/lib/tsserverlibrary.js')); } catch(e) { process.exit(1); }\" 2>/dev/null"))))
       (when (and ts-path (file-exists-p ts-path)) ts-path)))
   
   ;; 5. 检查常见的系统路径
   (seq-find #'file-exists-p
             (list
              "/usr/lib/node_modules/typescript/lib/tsserverlibrary.js"
              "/usr/local/lib/node_modules/typescript/lib/tsserverlibrary.js"))))

;; =============================================================================
;; Volar LSP 配置
;; =============================================================================

(defun my-setup-volar-lsp ()
  "配置 Volar LSP 服务器"
  (when (executable-find "vue-language-server")
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection '("vue-language-server" "--stdio"))
      :major-modes '(js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode web-mode)
      :server-id 'volar-takeover
      :priority 10
      :activation-fn (lambda (&rest _) (my-is-vue-project))
      :initialization-options
      (lambda ()
        (let ((ts-lib (my-find-typescript-lib)))
          (if ts-lib
              (progn
                (message "✓ Found TypeScript library: %s" ts-lib)
                `(:typescript
                  (:tsdk ,(file-name-directory ts-lib)
                   :preferences
                   (:includePackageJsonAutoImports "on"
                    :includeCompletionsForModuleExports t))
                  :vue
                  (:hybridMode nil)))
            (progn
              (message "⚠️  TypeScript library not found, using default configuration")
              `(:vue (:hybridMode nil))))))))))

;; =============================================================================
;; 初始化
;; =============================================================================

;; 在 LSP 模式加载后设置 Volar
(with-eval-after-load 'lsp-mode
  (my-setup-volar-lsp))

;; 如果 LSP 已经加载，立即设置
(when (featurep 'lsp-mode)
  (my-setup-volar-lsp))

(provide 'volar-config)

;;; volar-config.el ends here
