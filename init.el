;; =============================================================================
;; init.el - Emacs Configuration Entry File
;; =============================================================================

;; Add configuration path (must be first)
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; Load version configuration module
(require 'version-config)

;; Use version configuration functions for validation
(if (my-should-load-config)
    (progn
      ;; Version check passed, load full configuration
      (message "📋 Version check passed, loading full configuration...")
      (condition-case err
          (require 'main-config)
        (error 
         (message "❌ Configuration loading failed: %s" (error-message-string err))
         (message "🔧 Will run with default configuration")
         ;; Can add minimal fallback configuration here
         )))
  (progn
    ;; Version incompatible, run with default configuration
    (message "🚫 Version incompatible, will run with default configuration")
    (message "📋 Starting minimal Emacs environment...")
    ;; Add some basic configuration here to make Emacs at least usable
    (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
    (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
    (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
    (global-display-line-numbers-mode 1)
    (column-number-mode 1)
    (message "✅ Minimal configuration loading complete")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cmake-mode-cmake-executable "/usr/bin/cmake")
 '(package-selected-packages nil)
 '(python-indent-offset 4)
 '(pyvenv-workon "sys"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
