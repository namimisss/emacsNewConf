;;; java-treesit-config.el --- Java Tree-sitter配置  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: languages, java, tree-sitter
;; Version: 1.0.0

;;; Commentary:

;; Java 开发配置 (Tree-sitter 版本)，包括：
;; - Tree-sitter 语法支持 (java-ts-mode)
;; - LSP 语言服务器支持 (Eclipse JDT)
;; - Maven/Gradle 项目支持
;; - 代码格式化支持
;; - 保持向后兼容性

;;; Code:

;; =============================================================================
;; Java Tree-sitter 配置 (优先使用)
;; =============================================================================

;; Java Tree-sitter 模式配置
(use-package java-ts-mode
  :ensure nil  ; 内置模式
  :mode "\\.java\\'"
  :hook ((java-ts-mode . lsp-deferred)
         (java-ts-mode . flycheck-mode))
  :config
  ;; Java 缩进设置
  (setq java-ts-mode-indent-offset 4)
  
  ;; 键绑定设置将在mode hook中处理
  )

;; =============================================================================
;; Java LSP 配置 (Eclipse JDT Language Server)
;; =============================================================================

(use-package lsp-java
  :ensure t
  :hook ((java-ts-mode . lsp-deferred))
  :config
  ;; Java LSP 服务器配置
  (setq lsp-java-server-install-dir (expand-file-name "lsp/eclipse.jdt.ls/" user-emacs-directory))
  (setq lsp-java-workspace-dir (expand-file-name "lsp-java-workspace/" user-emacs-directory))
  
  ;; 启用 Java 特定功能
  (setq lsp-java-save-actions-organize-imports t)
  (setq lsp-java-format-enabled t)
  (setq lsp-java-format-on-type-enabled t)
  (setq lsp-java-signature-help-enabled t)
  (setq lsp-java-import-maven-enabled t)
  (setq lsp-java-import-gradle-enabled t)
  
  ;; Maven 配置
  (setq lsp-java-maven-download-sources t)
  (setq lsp-java-maven-update-snapshots t)
  
  ;; 代码生成配置
  (setq lsp-java-code-generation-use-blocks t)
  (setq lsp-java-code-generation-generate-comments t)
  
  ;; JVM 参数
  (setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms100m")))

;; =============================================================================
;; Java 代码格式化
;; =============================================================================

;; Java 代码格式化函数
(defun my-java-format-buffer ()
  "格式化 Java 代码"
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (lsp-format-buffer)
    (message "请启用 LSP 模式进行格式化")))

;; 保存时自动格式化（可选）
(defun my-java-format-on-save ()
  "Java 文件保存时自动格式化"
  (when (derived-mode-p 'java-ts-mode)
    (my-java-format-buffer)))

;; 如果需要保存时自动格式化，取消注释下面这行
;; (add-hook 'before-save-hook 'my-java-format-on-save)

;; =============================================================================
;; Maven 项目支持
;; =============================================================================

(defun my-java-maven-compile ()
  "编译 Maven 项目"
  (interactive)
  (compile "mvn compile"))

(defun my-java-maven-test ()
  "运行 Maven 测试"
  (interactive)
  (compile "mvn test"))

(defun my-java-maven-package ()
  "打包 Maven 项目"
  (interactive)
  (compile "mvn package"))

(defun my-java-maven-clean ()
  "清理 Maven 项目"
  (interactive)
  (compile "mvn clean"))

(defun my-java-maven-install ()
  "安装 Maven 项目"
  (interactive)
  (compile "mvn install"))

;; =============================================================================
;; Gradle 项目支持
;; =============================================================================

(defun my-java-gradle-build ()
  "构建 Gradle 项目"
  (interactive)
  (compile "gradle build"))

(defun my-java-gradle-test ()
  "运行 Gradle 测试"
  (interactive)
  (compile "gradle test"))

(defun my-java-gradle-clean ()
  "清理 Gradle 项目"
  (interactive)
  (compile "gradle clean"))

(defun my-java-gradle-run ()
  "运行 Gradle 项目"
  (interactive)
  (compile "gradle run"))

;; =============================================================================
;; Java 开发工具
;; =============================================================================

;; 运行 Java 主类
(defun my-java-run-main ()
  "运行当前 Java 文件的 main 方法"
  (interactive)
  (let* ((file-name (buffer-file-name))
         (class-name (file-name-sans-extension (file-name-nondirectory file-name))))
    (compile (format "cd %s && javac %s && java %s" 
                     (file-name-directory file-name)
                     (file-name-nondirectory file-name)
                     class-name))))

;; 运行测试
(defun my-java-run-tests ()
  "运行测试"
  (interactive)
  (cond
   ((file-exists-p "pom.xml") (my-java-maven-test))
   ((file-exists-p "build.gradle") (my-java-gradle-test))
   (t (message "未检测到 Maven 或 Gradle 项目"))))

;; 创建 Java 类
(defun my-java-create-class (class-name package-name)
  "创建新的 Java 类"
  (interactive "s类名: \ns包名: ")
  (let* ((package-dir (replace-regexp-in-string "\\." "/" package-name))
         (dir-path (concat "src/main/java/" package-dir))
         (file-path (concat dir-path "/" class-name ".java")))
    (make-directory dir-path t)
    (with-temp-file file-path
      (insert (format "package %s;\n\n" package-name))
      (insert (format "public class %s {\n" class-name))
      (insert "    public static void main(String[] args) {\n")
      (insert "        // TODO: 实现主方法\n")
      (insert "    }\n")
      (insert "}\n"))
    (find-file file-path)
    (message "Java 类创建完成: %s" file-path)))

;; =============================================================================
;; Tree-sitter 特定增强
;; =============================================================================

;; Java Tree-sitter 特定设置
(defun my-java-treesit-setup ()
  "Java Tree-sitter 特定设置"
  (when (treesit-parser-list)
    ;; 启用语法感知的导航
    (setq-local treesit-defun-type-regexp
                (rx (or "method_declaration"
                        "constructor_declaration"
                        "class_declaration"
                        "interface_declaration"
                        "enum_declaration")))
    
    ;; 启用更好的缩进
    (setq-local treesit-simple-indent-rules
                (when (fboundp 'treesit-simple-indent-rules-get)
                  (treesit-simple-indent-rules-get major-mode)))
    
    ;; Java 特定的 Tree-sitter 功能
    (setq-local treesit-font-lock-level 4)))  ; 最详细的语法高亮

;; 添加到 Java Tree-sitter 模式
(add-hook 'java-ts-mode-hook #'my-java-treesit-setup)

;; =============================================================================
;; Java 项目模板
;; =============================================================================

(defun my-java-create-maven-project (project-name group-id artifact-id)
  "创建 Maven 项目结构"
  (interactive "s项目名称: \sGroup ID: \sArtifact ID: ")
  (let ((base-dir project-name))
    ;; 创建目录结构
    (make-directory (concat base-dir "/src/main/java") t)
    (make-directory (concat base-dir "/src/main/resources") t)
    (make-directory (concat base-dir "/src/test/java") t)
    (make-directory (concat base-dir "/src/test/resources") t)
    
    ;; 创建 pom.xml
    (with-temp-file (concat base-dir "/pom.xml")
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
      (insert "<project xmlns=\"http://maven.apache.org/POM/4.0.0\"\n")
      (insert "         xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n")
      (insert "         xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd\">\n")
      (insert "    <modelVersion>4.0.0</modelVersion>\n\n")
      (insert (format "    <groupId>%s</groupId>\n" group-id))
      (insert (format "    <artifactId>%s</artifactId>\n" artifact-id))
      (insert "    <version>1.0.0</version>\n")
      (insert "    <packaging>jar</packaging>\n\n")
      (insert (format "    <name>%s</name>\n" project-name))
      (insert "    <description>Java project created with Emacs</description>\n\n")
      (insert "    <properties>\n")
      (insert "        <maven.compiler.source>11</maven.compiler.source>\n")
      (insert "        <maven.compiler.target>11</maven.compiler.target>\n")
      (insert "        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>\n")
      (insert "    </properties>\n\n")
      (insert "    <dependencies>\n")
      (insert "        <dependency>\n")
      (insert "            <groupId>junit</groupId>\n")
      (insert "            <artifactId>junit</artifactId>\n")
      (insert "            <version>4.13.2</version>\n")
      (insert "            <scope>test</scope>\n")
      (insert "        </dependency>\n")
      (insert "    </dependencies>\n")
      (insert "</project>\n"))
    
    ;; 创建 README.md
    (with-temp-file (concat base-dir "/README.md")
      (insert (format "# %s\n\n" project-name))
      (insert "Java Maven 项目\n\n")
      (insert "## 构建和运行\n\n")
      (insert "```bash\n")
      (insert "mvn compile\n")
      (insert "mvn test\n")
      (insert "mvn package\n")
      (insert "```\n"))
    
    (message "Maven 项目创建完成: %s" base-dir)))

;; =============================================================================
;; Java 键绑定配置
;; =============================================================================

;; 构建工具和项目键绑定已在上面的hook函数中处理

;; =============================================================================
;; 向后兼容性支持 (备选模式)
;; =============================================================================

;; 保留传统 java-mode 作为备选
(use-package java-mode
  :ensure nil
  :disabled t  ; 默认禁用，优先使用 Tree-sitter
  :config
  (setq c-basic-offset 4))

;; =============================================================================
;; 额外的 Java 开发工具
;; =============================================================================

;; Java 文档查看
(defun my-java-doc-at-point ()
  "查看光标处符号的文档"
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (lsp-describe-thing-at-point)
    (message "请启用 LSP 模式查看文档")))

;; 设置键绑定的函数
(defun my-java-ts-setup-keybindings ()
  "设置Java Tree-sitter模式的键绑定"
  (define-key java-ts-mode-map (kbd "M-.") 'lsp-find-definition)
  (define-key java-ts-mode-map (kbd "M-,") 'lsp-find-references)
  (define-key java-ts-mode-map (kbd "C-c C-r") 'lsp-rename)
  (define-key java-ts-mode-map (kbd "C-c C-f") 'my-java-format-buffer)
  (define-key java-ts-mode-map (kbd "C-c t t") 'my-java-run-tests)
  (define-key java-ts-mode-map (kbd "C-c j r") 'my-java-run-main)
  (define-key java-ts-mode-map (kbd "C-c m c") 'my-java-maven-compile)
  (define-key java-ts-mode-map (kbd "C-c m t") 'my-java-maven-test)
  (define-key java-ts-mode-map (kbd "C-c m p") 'my-java-maven-package)
  (define-key java-ts-mode-map (kbd "C-c m i") 'my-java-maven-install)
  (define-key java-ts-mode-map (kbd "C-c m l") 'my-java-maven-clean)
  (define-key java-ts-mode-map (kbd "C-c g b") 'my-java-gradle-build)
  (define-key java-ts-mode-map (kbd "C-c g t") 'my-java-gradle-test)
  (define-key java-ts-mode-map (kbd "C-c g r") 'my-java-gradle-run)
  (define-key java-ts-mode-map (kbd "C-c g l") 'my-java-gradle-clean)
  (define-key java-ts-mode-map (kbd "C-c j c") 'my-java-create-class)
  (define-key java-ts-mode-map (kbd "C-c j m") 'my-java-create-maven-project)
  (define-key java-ts-mode-map (kbd "C-c h h") 'my-java-doc-at-point)
  (define-key java-ts-mode-map (kbd "C-c s r") 'my-java-spring-boot-run))

;; 添加到hook中
(add-hook 'java-ts-mode-hook #'my-java-ts-setup-keybindings)

;; Spring Boot 项目支持
(defun my-java-spring-boot-run ()
  "运行 Spring Boot 项目"
  (interactive)
  (cond
   ((file-exists-p "pom.xml") (compile "mvn spring-boot:run"))
   ((file-exists-p "build.gradle") (compile "gradle bootRun"))
   (t (message "未检测到 Spring Boot 项目"))))

(provide 'java-treesit-config)

;;; java-treesit-config.el ends here
