#!/usr/bin/env python3

import os
import sys
import unused_module  # 未使用的导入

def test_function():
    """测试函数"""
    x = 10
    y = 20
    # 未使用的变量
    unused_var = x + y
    
    # 缺少返回语句
    print("Testing flycheck with Python")

def another_function(arg1, arg2):
    """另一个测试函数"""
    if arg1 > arg2:
        return arg1
    # 缺少else分支的返回

if __name__ == "__main__":
    test_function()
    result = another_function(5, 3)
    print(f"Result: {result}")
