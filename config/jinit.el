(setq ring-bell-function 'ignore)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode 0)
(display-time-mode 1)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq whitespace-line-column 1000) 
;; Enable soft-wrap
(global-visual-line-mode 1)
;; Maintain a list of recent files opened
(recentf-mode 1)            
(setq recentf-max-saved-items 50)
;; Move all the backup files to specific cache directory
;; This way you won't have annoying temporary files starting with ~(tilde) in each directory
;; Following setting will move temporary files to specific folders inside cache directory in EMACS_DIR
(setq user-cache-directory (concat user-emacs-directory "cache"))

(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq inhibit-startup-message t)
(electric-pair-mode t)
(setq-default tab-width 4)

(setq make-backup-files nil)
;; set global line minor mode
(global-linum-mode 1)
;; set global highlight line minor mode
(global-hl-line-mode 1)

(column-number-mode t)
(show-paren-mode t)

;; (add-to-list 'default-frame-alist '(font . "Source Code Pro-18"))
;; (set-frame-font "Source Code Pro-18")

;; theme
;;(load-theme 'solarized-light t)

;; Keeping buffers automatically up-to-date.
(require 'autorevert)
(global-auto-revert-mode 1)
(setq auto-revert-verbose t
      auto-revert-use-notify nil
      auto-revert-stop-on-user-input nil)

(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))

(provide 'jinit)
