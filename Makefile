# 简单的Makefile示例，展示如何与clangd集成

CXX = g++
CXXFLAGS = -std=c++17 -Wall -Wextra -g -O2
TARGET = test_clangd
SOURCES = test_clangd.cpp

# 默认目标
$(TARGET): $(SOURCES)
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(SOURCES)

# 清理
clean:
	rm -f $(TARGET)

# 为clangd生成compile_commands.json
# 需要安装bear: sudo apt install bear
compile_commands:
	bear -- make clean $(TARGET)

.PHONY: clean compile_commands
