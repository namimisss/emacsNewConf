;; config/main-config.el - Main Configuration File
;; This is the new unified configuration entry, replacing the original jpackage.el, jconfig.el, jinit.el
;; Note: Version checking is completed in init.el, configuration loading starts directly here

;; =============================================================================
;; Configuration Loading Start
;; =============================================================================

;; Ensure package system is initialized
(require 'package)
(package-initialize)

;; Add configuration paths to load-path
(let ((config-dir (expand-file-name "config" user-emacs-directory)))
  (add-to-list 'load-path (expand-file-name "base" config-dir))
  (add-to-list 'load-path (expand-file-name "ui" config-dir))
  (add-to-list 'load-path (expand-file-name "completion" config-dir))
  (add-to-list 'load-path (expand-file-name "tools" config-dir))
  (add-to-list 'load-path (expand-file-name "languages/cpp" config-dir))
  (add-to-list 'load-path (expand-file-name "languages/java" config-dir))
  (add-to-list 'load-path (expand-file-name "languages/python" config-dir))
  (add-to-list 'load-path (expand-file-name "languages/javascript" config-dir)))

;; 1. Base Configuration
(require 'base-packages)  ; Package management
(require 'base-basic)     ; Basic tools

;; 2. UI Configuration
(require 'ui-themes)      ; Themes
(require 'ui-interface)   ; Interface

;; 3. Completion Configuration
(require 'completion-company)  ; Company completion
(require 'completion-ivy)      ; Ivy completion
;; (require 'completion-vertico)  ; Vertico completion - conflicts with ivy, temporarily commented

;; 4. Tools Configuration
(require 'tools-flycheck)   ; Syntax checking
(require 'tools-lsp)        ; LSP
(require 'tools-projectile) ; Project management
(require 'tools-misc)       ; Other tools

;; 5. Language Configuration
(require 'cpp-config)       ; C++
(require 'java-config)      ; Java
(require 'python-config)    ; Python
(require 'js-config)        ; JavaScript

;; Global key bindings
(global-set-key (kbd "C-c C-r") 'ivy-resume)

;; multiple-cursors (if needed)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; hydra configuration
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(provide 'main-config)
