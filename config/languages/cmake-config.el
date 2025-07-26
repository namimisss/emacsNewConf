;;; cmake-config.el --- CMake语言配置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: languages, cmake, build-system
;; Version: 1.0.0

;;; Commentary:

;; CMake 构建系统配置，包括：
;; - 语法高亮和编辑支持
;; - company-cmake 提供的补全功能
;; - 基本编辑功能

;;; Code:

;; CMake 模式配置
(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :config
  ;; 基本编辑设置
  (setq cmake-tab-width 4))

;; CMake 补全支持 - 传统方式
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-cmake))

;; CMake 语法高亮增强
(use-package cmake-font-lock
  :ensure t
  :after cmake-mode
  :config
  (cmake-font-lock-activate))

;; CMake 项目文件模板
(defun my-cmake-create-cmakelists ()
  "创建基本的 CMakeLists.txt 模板"
  (interactive)
  (let ((template "cmake_minimum_required(VERSION 3.10)

project(MyProject)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# 添加可执行文件
add_executable(${PROJECT_NAME} main.cpp)

# 或者添加库
# add_library(${PROJECT_NAME} STATIC src/library.cpp)

# 链接库（如果需要）
# target_link_libraries(${PROJECT_NAME} some_library)

# 包含目录
# target_include_directories(${PROJECT_NAME} PRIVATE include)
"))
    (insert template)))

;; CMake 模式钩子
(add-hook 'cmake-mode-hook
          (lambda ()
            ;; 基本编辑设置
            (setq-local tab-width 4)
            (setq-local indent-tabs-mode nil)
            
            ;; 启用 company 补全
            (when (fboundp 'company-mode)
              (company-mode 1))
            
            ;; 启用语法检查
            (when (fboundp 'flycheck-mode)
              (flycheck-mode 1))
            
            ;; 显示行号
            (when (fboundp 'display-line-numbers-mode)
              (display-line-numbers-mode 1))))

;; CMake 快捷键绑定
(with-eval-after-load 'cmake-mode
  (define-key cmake-mode-map (kbd "C-c C-t") 'my-cmake-create-cmakelists)
  (define-key cmake-mode-map (kbd "C-c C-f") 'format-all-buffer))

(provide 'cmake-config)

;;; cmake-config.el ends here 