#ifndef __I386_UNALIGNED_H
#define __I386_UNALIGNED_H
#define get_unaligned(ptr) (*(ptr))
#define put_unaligned(val, ptr) ((void)( *(ptr) = (val) ))
#endif