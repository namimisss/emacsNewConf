# Emacs 配置

这是一个模块化的Emacs配置，已经过优化和重构。

## 目录结构

```
.emacs.d/
├── config/              # 配置文件目录
│   ├── base/           # 基础配置
│   │   ├── base-basic.el       # 基础工具配置
│   │   └── base-packages.el    # 包管理配置
│   ├── completion/     # 补全配置
│   │   ├── completion-company.el   # Company补全
│   │   ├── completion-ivy.el       # Ivy补全
│   │   └── completion-vertico.el   # Vertico补全(已禁用)
│   ├── languages/      # 语言特定配置
│   │   ├── cpp/        # C++配置
│   │   ├── java/       # Java配置
│   │   ├── javascript/ # JavaScript配置
│   │   └── python/     # Python配置
│   ├── tools/          # 工具配置
│   │   ├── tools-flycheck.el   # 语法检查
│   │   ├── tools-lsp.el        # LSP配置
│   │   ├── tools-misc.el       # 其他工具
│   │   └── tools-projectile.el # 项目管理
│   ├── ui/             # 界面配置
│   │   ├── ui-interface.el     # 界面设置
│   │   └── ui-themes.el        # 主题配置
│   └── main-config.el   # 主配置文件
├── backup/             # 旧配置备份
├── init.el             # Emacs入口文件
└── README.md           # 本文件
```

## 特性

- ✅ 模块化配置结构
- ✅ LSP支持 (clangd, python-lsp-server等)
- ✅ 智能补全 (Company + LSP)
- ✅ 语法检查 (Flycheck)
- ✅ 项目管理 (Projectile)
- ✅ 现代UI (主题、图标等)
- ✅ 启动性能优化
- ✅ C++、Java、Python、JavaScript支持

## 使用说明

1. 启动Emacs后，配置会自动加载
2. C++文件会自动启用LSP模式
3. 使用 `C-c l` 前缀访问LSP功能
4. 使用 `C-c p` 前缀访问Projectile功能

## 性能优化

- 启动时临时提高垃圾回收阈值
- LSP性能调优
- 禁用不必要的功能
- 延迟加载包配置

## 维护

配置采用模块化设计，每个功能都在独立的文件中，便于维护和扩展。

## 📦 推荐插件

以下是基于当前配置推荐的优秀插件，按优先级和功能分类：

### 🚀 优先级1 - 立即推荐

#### 文件搜索和导航
```elisp
;; 超强的文件搜索（比ag更快）
(use-package rg
  :ensure t)

;; 搜索替换计数显示
(use-package anzu
  :ensure t
  :config (global-anzu-mode 1))
```

#### 文本编辑增强
```elisp
;; 快速切换命名风格（camelCase, snake_case等）
(use-package string-inflection
  :ensure t
  :bind ("C-c c i" . string-inflection-cycle))

;; 平滑滚动
(use-package smooth-scrolling
  :ensure t
  :config (smooth-scrolling-mode 1))
```

### ⚡ 优先级2 - 编程效率提升

#### 代码编辑和格式化
```elisp
;; 强大的代码折叠
(use-package origami
  :ensure t
  :config (global-origami-mode 1))

;; 异步代码格式化（比format-all更好）
(use-package apheleia
  :ensure t
  :config (apheleia-global-mode 1))

;; 更美观的eldoc显示
(use-package eldoc-box
  :ensure t)
```

#### 项目和文件管理
```elisp
;; 可视化书签系统
(use-package bm
  :ensure t
  :bind (("C-c b m" . bm-toggle)
         ("C-c b n" . bm-next)
         ("C-c b p" . bm-previous)))

;; 快速访问最近文件/目录
(use-package fasd
  :ensure t)

;; 增强的kill-ring浏览
(use-package browse-kill-ring
  :ensure t
  :bind ("C-c y" . browse-kill-ring))
```

### 🔧 优先级3 - 特定功能增强

#### LSP和开发工具增强
```elisp
;; LSP性能提升
(use-package lsp-booster
  :ensure t)

;; 更多代码片段
(use-package yasnippet-snippets
  :ensure t)

;; 统一的调试界面
(use-package realgud
  :ensure t)
```

#### 终端和系统集成
```elisp
;; 最好的终端模拟器
(use-package vterm
  :ensure t)

;; 环境变量管理（Mac/Linux）
(use-package exec-path-from-shell
  :ensure t
  :config 
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; 系统通知
(use-package alert
  :ensure t)
```

### 🌐 Web开发专用

```elisp
;; 统一的Web开发模式
(use-package web-mode
  :ensure t)

;; 浏览器交互
(use-package skewer-mode
  :ensure t)

;; HTTP请求测试
(use-package restclient
  :ensure t)
```

### 📊 数据和文档工具

```elisp
;; CSV编辑
(use-package csv-mode
  :ensure t)

;; SQL查询
(use-package emacsql
  :ensure t)

;; 文档查询
(use-package dash-at-point
  :ensure t)
```

### 🎨 界面美化

```elisp
;; 更好的块高亮
(use-package rainbow-blocks
  :ensure t)

;; 现代的缩进线
(use-package indent-bars
  :ensure t)

;; 智能窗口布局
(use-package shackle
  :ensure t)
```

### 🏢 工作区管理

```elisp
;; 工作区分离
(use-package perspective
  :ensure t)

;; 项目特定shell
(use-package project-shells
  :ensure t)
```

### 📝 高级文本操作

```elisp
;; 智能括号操作（Lisp特化）
(use-package lispy
  :ensure t)

;; 结合avy的zap功能
(use-package avy-zap
  :ensure t)

;; 持久化历史
(use-package savehist
  :ensure t
  :config (savehist-mode 1))
```

### 🧪 测试和覆盖率

```elisp
;; 测试运行器
(use-package ert-runner
  :ensure t)

;; 显示代码覆盖率
(use-package coverlay
  :ensure t)
```

## 💡 安装指南

### 手动安装步骤

1. **选择插件**：从上述列表中选择需要的插件
2. **确定位置**：根据功能将配置添加到对应的模块文件中：
   - 编辑增强 → `config/core/base-enhancements.el`
   - UI美化 → `config/ui/ui-themes.el` 或 `config/ui/ui-interface.el`
   - 开发工具 → `config/tools/tools-misc.el`
   - 语言特定 → `config/languages/*/`

3. **添加配置**：复制对应的 `use-package` 块到目标文件
4. **重启测试**：重启Emacs或使用 `C-c e r` 重载配置

### 推荐安装顺序

1. **第一批**：`rg`, `anzu`, `string-inflection`, `smooth-scrolling`
2. **第二批**：`origami`, `bm`, `browse-kill-ring`
3. **按需添加**：根据具体开发需求添加特定插件

### 注意事项

- 🔍 某些插件可能需要外部依赖（如ripgrep, fasd等）
- ⚡ 一次性添加太多插件可能影响启动性能
- 🧪 建议先添加少量插件测试，确认无冲突后再继续
- 📚 部分插件可能需要额外的按键绑定配置

### 安装工具

#### 基础工具
```bash
sudo apt install aspell ripgrep

C-c p s r    ; counsel-projectile-rg (超快文本搜索)
C-c p s g    ; projectile-grep (您已配置的传统grep)
C-c p s s    ; projectile-ag (如果需要可安装 silversearcher-ag)
C-c p f

```

#### JavaScript/TypeScript/React/Vue 开发环境
```bash
# LSP 服务器
npm install -g typescript-language-server typescript

# 代码格式化和检查工具
npm install -g prettier eslint

# 或者使用Volar (Vue官方推荐，同时支持Vue 2和Vue 3)
npm install -g @vue/language-server

# React 开发相关 (如果需要额外的检查规则)
npm install -g eslint-plugin-react eslint-plugin-react-hooks

# python
pip install python-lsp-server[all]
# 或者
npm install -g pyright
```

#### 支持的文件类型和模式说明
- **JavaScript**: `.js` 文件 → `rjsx-mode` (支持JSX语法)
- **React JSX**: `.jsx` 文件 → `rjsx-mode` 
- **ES6 模块**: `.mjs` 文件 → `js2-mode` (纯JavaScript，无JSX)
- **TypeScript**: `.ts` 文件 → `typescript-mode`
- **TypeScript JSX**: `.tsx` 文件 → `web-mode` (配置为JSX模式)
- **Vue.js**: `.vue` 文件 → `web-mode` + Volar LSP (同时支持Vue 2和Vue 3)
- **JSON**: `.json` 文件 → `json-mode`
- **HTML/CSS**: `.html`, `.htm` 文件 → `web-mode`

> **注意**: 
> - 现在 `.js` 和 `.jsx` 文件都使用 `rjsx-mode`，这样可以在普通JS文件中也支持JSX语法
> - 如果您只想写纯JavaScript（不包含JSX），可以使用 `.mjs` 扩展名
> - `.vue` 文件使用 `web-mode` + Volar LSP，同时支持Vue 2和Vue 3（包括Composition API、setup语法糖等）
> - 所有模式都自动启用LSP支持，提供智能补全、错误检查等功能

#### Vue.js 开发配置说明

本配置采用 **web-mode + Volar LSP** 方案，具有以下优势：

- ✅ **同时支持Vue 2和Vue 3**：无需切换配置
- ✅ **官方支持**：Volar是Vue官方推荐的语言服务器
- ✅ **完整TypeScript集成**：更好的类型检查和智能提示
- ✅ **无第三方依赖**：不依赖社区维护的vue-mode包
- ✅ **统一的web-mode**：同时处理HTML、CSS、JS、TSX、Vue

**Volar LSP特性：**
- 模板类型检查和智能提示
- 组件props自动补全
- Vue 3 Composition API完整支持
- `<script setup>`语法糖支持
- 跨文件的组件引用跳转
