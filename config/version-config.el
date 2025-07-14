;; version-config.el - Emacs Version Configuration
;; Provides version checking and validation functions

;; =============================================================================
;; Version Configuration Parameters
;; =============================================================================

(defvar my-emacs-minimum-version 27
  "Minimum supported Emacs major version. Configuration loading will be rejected for versions below this.")

(defvar my-emacs-maximum-version 30
  "Maximum supported Emacs major version. Versions above this will show warnings.")

(defvar my-emacs-strict-mode t
  "Strict mode. If t, reject loading for versions above maximum; if nil, allow user choice.")

;; =============================================================================
;; Version Compatibility Information
;; =============================================================================

(defconst my-emacs-version-compatibility
  '((26 . "❌ Not supported, version too old")
    (27 . "✅ Full support, recommended version")
    (28 . "✅ Full support, recommended version") 
    (29 . "✅ Full support, recommended version")
    (30 . "⚠️  New version, basic support")
    (31 . "❓ Untested version, may have compatibility issues"))
  "Version compatibility descriptions")

;; =============================================================================
;; Core Version Checking Functions
;; =============================================================================

(defun my-get-version-status (version)
  "Get compatibility status description for specified version."
  (or (cdr (assoc version my-emacs-version-compatibility))
      "❓ Unknown version, compatibility uncertain"))

(defun my-check-version-compatibility (&optional min-version max-version)
  "Check if current Emacs version is compatible.
Arguments:
  MIN-VERSION - Minimum version requirement (optional, defaults to my-emacs-minimum-version)
  MAX-VERSION - Maximum version requirement (optional, defaults to my-emacs-maximum-version)
Return values:
  'compatible    - Version fully compatible
  'too-low       - Version too low
  'too-high      - Version too high
  'invalid-range - Invalid version range (min > max)
  'unknown       - Unknown status"
  (let ((current-version emacs-major-version)
        (min-ver (or min-version my-emacs-minimum-version))
        (max-ver (or max-version my-emacs-maximum-version)))
    (cond
     ;; Check for invalid range first
     ((> min-ver max-ver) 'invalid-range)
     ;; Version too low
     ((< current-version min-ver) 'too-low)
     ;; Version too high
     ((> current-version max-ver) 'too-high)
     ;; Version compatible (including exact match when min == max)
     (t 'compatible))))

(defun my-should-load-config (&optional min-version max-version)
  "Determine whether configuration should be loaded.
Arguments:
  MIN-VERSION - Minimum version requirement (optional)
  MAX-VERSION - Maximum version requirement (optional)
Return values:
  t   - Should load configuration
  nil - Should not load configuration"
  (let ((compatibility (my-check-version-compatibility min-version max-version))
        (current-version emacs-major-version))
    (cond
     ;; Invalid version range
     ((eq compatibility 'invalid-range)
      (message "❌ Invalid version range: minimum (%d) > maximum (%d)" 
               (or min-version my-emacs-minimum-version)
               (or max-version my-emacs-maximum-version))
      nil)
     
     ;; Version too low, reject loading
     ((eq compatibility 'too-low)
      (message "❌ Emacs %d version too low, requires Emacs %d or higher" 
               current-version (or min-version my-emacs-minimum-version))
      (message "   Status: %s" (my-get-version-status current-version))
      nil)
     
     ;; Version too high handling
     ((eq compatibility 'too-high)
      (message "⚠️  Emacs %d version exceeds tested range, maximum supported version is Emacs %d" 
               current-version (or max-version my-emacs-maximum-version))
      (message "   Status: %s" (my-get-version-status current-version))
      (if my-emacs-strict-mode
          (progn
            (message "🚫 Strict mode: Rejecting untested version configuration")
            nil)
        (if noninteractive
            (progn
              (message "🤖 Batch mode: Auto-rejecting load")
              nil)
          (y-or-n-p "Continue loading configuration? (May have compatibility issues) "))))
     
     ;; Version compatible, normal loading
     ((eq compatibility 'compatible)
      (message "✅ Emacs %d version check passed - %s" 
               current-version (my-get-version-status current-version))
      t)
     
     ;; Unknown status, handle cautiously
     (t
      (message "❓ Emacs %d version status unknown" current-version)
      (if noninteractive
          nil
        (y-or-n-p "Version status unknown, continue loading configuration?"))))))

(defun my-display-version-info ()
  "Display version information and configuration status."
  (message "🔍 Emacs Version Information:")
  (message "   Current version: Emacs %d.%d" emacs-major-version emacs-minor-version)
  (message "   Minimum required: Emacs %d" my-emacs-minimum-version)
  (message "   Maximum supported: Emacs %d" my-emacs-maximum-version)
  (message "   Strict mode: %s" (if my-emacs-strict-mode "Enabled" "Disabled"))
  (message "   Version status: %s" (my-get-version-status emacs-major-version)))

;; =============================================================================
;; Configuration Customization Functions
;; =============================================================================

(defun my-set-version-range (min-version max-version &optional strict)
  "Set version range.
Arguments:
  MIN-VERSION - Minimum version
  MAX-VERSION - Maximum version  
  STRICT      - Whether to enable strict mode (optional)"
  (if (> min-version max-version)
      (message "❌ Invalid version range: minimum (%d) cannot be greater than maximum (%d)" 
               min-version max-version)
    (setq my-emacs-minimum-version min-version)
    (setq my-emacs-maximum-version max-version)
    (when strict
      (setq my-emacs-strict-mode strict))
    (message "✅ Version range updated: Emacs %d-%d%s" 
             min-version max-version 
             (if my-emacs-strict-mode " (strict mode)" ""))))

(provide 'version-config)
