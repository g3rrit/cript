
let header = {|
#include <stdio.h>

#define bool int
#define true 1
#define false 0

int i101(int a, int b)
{
    return a + b;
}

void i102(int a)
{
    printf("%d\n", a);
}

|}