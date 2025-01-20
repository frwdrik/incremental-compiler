#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/mman.h>

// 1st arg: rdi
// 2nd arg: rsi
// 3rd arg: rdx
extern int scheme_entry(char *stack_base);

#define bool_t 0x6F
#define bool_f 0x2F
#define nil    0x3F

typedef struct {
void* eax; /* 0 scratch */
void* ebx; /* 8 preserve */
void* ecx; /* 16 scratch */
void* edx; /* 24 scratch */
void* esi; /* 32 preserve */
void* edi; /* 40 preserve */
void* ebp; /* 48 preserve */
void* esp; /* 56 preserve */
} context;



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

static char *allocate_protected_space(int size) {
    int page = getpagesize();
    int status;
    int aligned_size = ((size + page - 1) / page) * page;
    char *p = mmap(0, aligned_size + 2 * page,
                   PROT_READ | PROT_WRITE,
                   MAP_ANONYMOUS | MAP_PRIVATE,
                   0, 0);
    if (p == MAP_FAILED) {
        exit(1);
    }
    status = mprotect(p, page, PROT_NONE);
    if (status != 0)
        exit(1);
    status = mprotect(p + aligned_size + page, page, PROT_NONE);
    if (status != 0)
        exit(1);
    return p + page;
}

static void deallocate_protected_space(char *p, int size) {
    int page = getpagesize();
    int status;
    int aligned_size = ((size + page - 1) / page) * page;
    status = munmap(p - page, aligned_size + 2 * page);
    if (status != 0)
        exit(1);
}

int main() {
    int stack_size = (16 * 4096); // 16Kb
    char *stack_top = allocate_protected_space(stack_size);
    char *stack_base = stack_top + stack_size;

    int heap_size = (16 * 4096);
    char *heap = allocate_protected_space(heap_size);

    context ctxt;

    //    p                 p+stack_size (= esp)
    //
    // | 1000 | 1001 | .... | 4000 |

    int ret = scheme_entry(&ctxt, stack_base, heap);
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

    deallocate_protected_space(stack_top, stack_size);
    deallocate_protected_space(heap, heap_size);

    return 0;
}
