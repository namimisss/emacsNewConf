;; config/main-config.el - 主配置文件
;; 这是新的统一配置入口，替代原来的 jpackage.el, jconfig.el, jinit.el

;; 确保包系统已初始化
(require 'package)
(package-initialize)

;; 添加配置路径到load-path
(let ((config-dir (expand-file-name "config" user-emacs-directory)))
  (add-to-list 'load-path (expand-file-name "base" config-dir))
  (add-to-list 'load-path (expand-file-name "ui" config-dir))
  (add-to-list 'load-path (expand-file-name "completion" config-dir))
  (add-to-list 'load-path (expand-file-name "tools" config-dir))
  (add-to-list 'load-path (expand-file-name "languages/cpp" config-dir))
  (add-to-list 'load-path (expand-file-name "languages/java" config-dir))
  (add-to-list 'load-path (expand-file-name "languages/python" config-dir))
  (add-to-list 'load-path (expand-file-name "languages/javascript" config-dir)))

;; 1. 基础配置
(require 'base-packages)  ; 包管理
(require 'base-basic)     ; 基础工具

;; 2. UI配置
(require 'ui-themes)      ; 主题
(require 'ui-interface)   ; 界面

;; 3. 补全配置
(require 'completion-company)  ; Company补全
(require 'completion-ivy)      ; Ivy补全
(require 'completion-vertico)  ; Vertico补全

;; 4. 工具配置
(require 'tools-flycheck)   ; 语法检查
(require 'tools-lsp)        ; LSP
(require 'tools-projectile) ; 项目管理
(require 'tools-misc)       ; 其他工具

;; 5. 语言配置
(require 'cpp-config)       ; C++
(require 'java-config)      ; Java
(require 'python-config)    ; Python
(require 'js-config)        ; JavaScript

;; 全局键绑定
(global-set-key (kbd "C-c C-r") 'ivy-resume)

;; multiple-cursors (如果需要的话)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; hydra配置
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(provide 'main-config)
