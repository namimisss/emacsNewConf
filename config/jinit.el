(tool-bar-mode -1)
(menu-bar-mode 0)
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq inhibit-startup-message t)
(electric-pair-mode t)

(setq make-backup-files nil)
;; set global line minor mode
(global-linum-mode 1)
;; set global highlight line minor mode
;;(global-hl-line-mode 1)

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

(provide 'jinit)
