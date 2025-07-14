;; languages/languages-config.el - Language Configuration Loader
;; Simple language configuration loading

;; =============================================================================
;; Language Configuration Loader
;; =============================================================================

;; Load language configurations
(require 'cpp/cpp-config)
(require 'java/java-config)
(require 'python/python-config)
(require 'javascript/javascript-config)

(provide 'languages-config)
