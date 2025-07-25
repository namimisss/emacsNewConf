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
  (add-to-list 'load-path (expand-file-name "languages" config-dir))
  (add-to-list 'load-path (expand-file-name "languages/cpp" config-dir))
  (add-to-list 'load-path (expand-file-name "languages/java" config-dir))
  (add-to-list 'load-path (expand-file-name "languages/python" config-dir))
  (add-to-list 'load-path (expand-file-name "languages/javascript" config-dir)))

;; 1. Base Configuration
(require 'base-packages)  ; Package management
(require 'base-basic)     ; Basic tools
(require 'base-enhancements)  ; Modern editing enhancements

;; 2. UI Configuration
(require 'ui-themes)      ; Themes
(require 'ui-interface)   ; Interface

;; 3. Completion Configuration
(require 'completion-company)  ; Company completion
(require 'completion-ivy)      ; Ivy completion
;; (require 'completion-vertico)  ; Vertico completion - conflicts with ivy, temporarily commented
;; (require 'completion-modern)   ; Modern completion (consult + embark + orderless) - 可选，与ivy并存

;; 4. Tools Configuration
(require 'tools-flycheck)   ; Syntax checking
(require 'tools-lsp)        ; LSP
(require 'tools-projectile) ; Project management
(require 'tools-misc)       ; Other tools

;; 5. Language Configuration
(require 'languages-config)   ; Load language configurations

;; Global key bindings
(global-set-key (kbd "C-c C-r") 'ivy-resume)

;; multiple-cursors - 已在 base-enhancements 中配置

;; hydra configuration
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(provide 'main-config)
