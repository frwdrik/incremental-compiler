#include <stdio.h>

extern int scheme_entry();

#define bool_t 0x6F
#define bool_f 0x2F
#define nil    0x3F

int fixnum(int x) {
  return (x & 3) == 0;
}

int fromFixnum(int x) {
  return (x >> 2);
}

int isChar(int x) {
  return (x & 0xFF) == 15;
}

int fromChar(int x) {
  return (x >> 8);
}

int main(){
    int ret = scheme_entry();
    /* If x is fixnum type
     then print content as integer */
    if (fixnum(ret)) {
      printf("%d\n", fromFixnum(ret));
    }
    if (ret == bool_t) {
      printf("true\n");
    }
    if (ret == bool_f) {
      printf("false\n");
    }
    if (ret == nil) {
      printf("()\n");
    }
    if (isChar(ret)) {
      printf("#\\%c\n", fromChar(ret));
    }
    return 0;
}
