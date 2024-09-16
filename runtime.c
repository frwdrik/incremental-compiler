#include <stdio.h>

extern int scheme_entry();

#define bool_t 0x6F
#define bool_f 0x2F

int fixnum(int x) {
  return (x & 3) == 0;
}

int toFixnum(int x) {
  return (x >> 2);
}

int otherImmediate(int x) {
  return (x & 15) == 15;
}

int main(){
    int ret = scheme_entry();
    /* If x is fixnum type
     then print content as integer */
    if (fixnum(ret)) {
      printf("%d\n", toFixnum(ret));
    }
    if (ret == bool_t) {
      printf("true\n");
    }
    if (ret == bool_f) {
      printf("false\n");
    }
    return 0;
}
