#ifndef _HACK_ASM_UACCESS_H_
#define _HACK_ASM_UACCESS_H_
#include <linux/mm.h>
#define MAKE_MM_SEG(s) XXX
#define KERNEL_DS XXX
#define USER_DS XXX
#define get_ds() XXX
#define get_fs() XXX
#define set_fs(x) XXX
#define segment_eq(a,b) XXX
extern int __verify_write(const void *, unsigned long);
#define __verify_write XXX
#define __addr_ok(addr) (1)
#define __range_ok(addr,size) (1)
#define access_ok(type,addr,size) (1)
#define put_user(x,ptr) (*(ptr) = (x), 0)
#define get_user(x,ptr) ((x) = *(ptr), 0)
#define __get_user(x,ptr) get_user((x), (ptr))
struct __large_struct { unsigned long buf[100]; };
#define __m(x) (*(struct __large_struct *)(x))
#define put_user_ret(x,ptr,ret) ({ if (put_user(x,ptr)) return ret; })
#define get_user_ret(x,ptr,ret) ({ if (get_user(x,ptr)) return ret; })
#define copy_to_user(to,from,n) (memcpy ((to), (from), (n)), 0)
#define copy_from_user(to,from,n) (memcpy ((to), (from), (n)), 0)
#define clear_user(mem, len) (memset ((mem), 0, (len)), 0)
#define copy_to_user_ret(to,from,n,retval) ({ if (copy_to_user(to,from,n)) return retval; })
#define copy_from_user_ret(to,from,n,retval) ({ if (copy_from_user(to,from,n)) return retval; })
#endif