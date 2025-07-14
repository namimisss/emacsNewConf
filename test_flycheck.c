#include <stdio.h>
#include <stdlib.h>

// 故意添加一些可能的问题来测试flycheck

int main() {
    int unused_variable = 42;  // 未使用的变量
    printf("Hello, World!\n");
    
    // 缺少返回值类型的函数
    int *ptr = malloc(sizeof(int));
    // 忘记检查malloc是否成功
    *ptr = 10;
    printf("Value: %d\n", *ptr);
    
    // 忘记释放内存
    // free(ptr);
    
    return 0;
}
