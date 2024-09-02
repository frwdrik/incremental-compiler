#include <stdio.h>

extern int scheme_entry();

int main(){
    int ret = scheme_entry();
    printf("%d\n", ret);
    return 0;
}
