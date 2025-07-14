;; languages/examples/custom-language-setup.el - Custom Language Setup Examples
;; Examples of how to customize language configurations

;; Example 1: Minimal C++ setup only
;; (setq my-enabled-languages '(cpp))

;; Example 2: Web development setup
;; (setq my-enabled-languages '(javascript typescript))

;; Example 3: Data science setup
;; (setq my-enabled-languages '(python))

;; Example 4: Full development environment
;; (setq my-enabled-languages '(cpp java python javascript typescript go rust))

;; Example 5: Mobile development
;; (setq my-enabled-languages '(java javascript typescript))

;; Example 6: Systems programming
;; (setq my-enabled-languages '(cpp rust go))

;; Dynamic language management examples:

;; Enable a language at runtime
;; (my-enable-language 'rust)
;; (my-load-language-config 'rust)

;; Disable a language
;; (my-disable-language 'java)

;; Check what languages are available
;; (my-list-available-languages)

;; To use any of these examples:
;; 1. Copy the desired configuration to language-settings.el
;; 2. Restart Emacs or reload the configuration

(provide 'custom-language-setup)
