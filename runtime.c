#include <stdio.h>

extern int scheme_entry();

int fixnum(int x) {
  return (x & 3) == 0;
}

int toFixnum(int x) {
  return (x >> 2);
}

int main(){
    int ret = scheme_entry();
    /* If x is fixnum type
     then print content as integer */
    if (fixnum(ret)) {
      printf("%d\n", toFixnum(ret));
    }
    return 0;
}
