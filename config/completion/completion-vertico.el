;;; completion-vertico.el ---   -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: configuration
;; Version: 1.0.0

;;; Commentary:

;; 

;;; Code:

;; completion/vertico.el - Vertico补全配置

(use-package vertico
  :ensure t)

(use-package orderless
  :ensure t)

(use-package consult
  :ensure t)

(use-package compat
  :ensure t)

(use-package consult-dir
  :ensure t)

(use-package consult-flycheck
  :ensure t)

(use-package embark
  :ensure t)

(use-package embark-consult
  :ensure t)

(use-package all-the-icons-completion
  :ensure t)

(use-package vertico-posframe
  :ensure t)

(provide 'completion-vertico)

;;; completion-vertico.el ends here
