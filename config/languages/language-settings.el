;; languages/language-settings.el - Language Configuration Settings
;; Centralized settings for language configurations

;; =============================================================================
;; Language Configuration Settings
;; =============================================================================

;; Define which languages to enable by default
(setq my-enabled-languages '(cpp java python javascript))

;; You can customize which languages to load by modifying the list above
;; Available languages: cpp, java, python, javascript, go, rust, typescript

;; Example configurations for different use cases:

;; For C++ development only:
;; (setq my-enabled-languages '(cpp))

;; For web development:
;; (setq my-enabled-languages '(javascript typescript))

;; For full-stack development:
;; (setq my-enabled-languages '(cpp java python javascript typescript))

;; For systems programming:
;; (setq my-enabled-languages '(cpp rust go))

(provide 'language-settings)
