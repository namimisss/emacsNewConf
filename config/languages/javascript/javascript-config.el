;;; javascript-config.el --- JavaScript语言配置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: languages, javascript, node
;; Version: 1.0.0

;;; Commentary:

;; JavaScript 开发配置，包括：
;; - Tern 语言服务器支持
;; - 基础编辑功能
;; - 格式化支持

;;; Code:

;; Tern - JavaScript代码补全和分析

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :config
  (setq js2-basic-offset 2
        js2-bounce-indent-p nil)
  (setq js2-highlight-level 2)
  (setq js2-mode-show-parse-errors t)
  )

(use-package js2-refactor
  :ensure t
  :hook (js2-mode . js2-refactor-mode)
  :config
  )

(use-package prettier-js
  :ensure t
  :hook ((js2-mode . prettier-js-mode)
         )
  :config
  )


(provide 'javascript-config)

;;; javascript-config.el ends here
