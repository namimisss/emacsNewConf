#!/bin/bash
# install_nerd_fonts.sh - 安装Nerd字体

echo "Installing Nerd Fonts for proper icon display..."

# 创建字体目录
mkdir -p ~/.local/share/fonts

# 下载FiraCode Nerd Font
echo "Downloading FiraCode Nerd Font..."
cd ~/.local/share/fonts
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.2/FiraCode.zip
unzip FiraCode.zip
rm FiraCode.zip

# 下载FiraMono Nerd Font (你当前使用的)
echo "Downloading FiraMono Nerd Font..."
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.2/FiraMono.zip
unzip FiraMono.zip
rm FiraMono.zip

# 刷新字体缓存
echo "Refreshing font cache..."
fc-cache -fv

echo "Nerd Fonts installed successfully!"
echo "Available Nerd Fonts:"
fc-list | grep -i "fira.*nerd" | head -5
