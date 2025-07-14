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
