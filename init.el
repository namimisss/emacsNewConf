;; 添加配置路径
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; 使用新的统一配置
(require 'main-config)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cmake-mode-cmake-executable "/usr/bin/cmake")
 '(package-selected-packages
   '(helm-rtags ivy-rtags rtags company-irony-c-headers company-irony irony-eldoc irony modern-cpp-font-lock disaster demangle cmake-mode use-package))
 '(python-indent-offset 4)
 '(pyvenv-workon "sys"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
