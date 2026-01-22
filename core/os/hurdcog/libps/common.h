#include <sys/mman.h>
#define ABS(x) ((x) < 0 ? -(x) : (x))
#define MAX(x, y) ((x) < (y) ? (y) : (x))
#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define NEW(type) ((type *)malloc(sizeof(type)))
#define NEWVEC(type,len) ((type *)malloc(sizeof(type)*(len)))
#define GROWVEC(old,type,len) \
((type *)realloc((void *)(old),(unsigned)(sizeof(type)*(len))))
#define FREE(x) (void)free((void *)x)
#define VMFREE(x, len) munmap((caddr_t)x, len)
#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif