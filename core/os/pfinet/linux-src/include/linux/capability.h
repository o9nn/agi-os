#ifndef _LINUX_CAPABILITY_H
#define _LINUX_CAPABILITY_H
#include <linux/types.h>
#include <linux/fs.h>
#define _LINUX_CAPABILITY_VERSION 0x19980330
typedef struct __user_cap_header_struct {
__u32 version;
int pid;
} *cap_user_header_t;
typedef struct __user_cap_data_struct {
__u32 effective;
__u32 permitted;
__u32 inheritable;
} *cap_user_data_t;
#ifdef __KERNEL__
#ifdef STRICT_CAP_T_TYPECHECKS
typedef struct kernel_cap_struct {
__u32 cap;
} kernel_cap_t;
#else
typedef __u32 kernel_cap_t;
#endif
#define _USER_CAP_HEADER_SIZE (2*sizeof(__u32))
#define _KERNEL_CAP_T_SIZE (sizeof(kernel_cap_t))
#endif
#define CAP_CHOWN 0
#define CAP_DAC_OVERRIDE 1
#define CAP_DAC_READ_SEARCH 2
#define CAP_FOWNER 3
#define CAP_FSETID 4
#define CAP_FS_MASK 0x1f
#define CAP_KILL 5
#define CAP_SETGID 6
#define CAP_SETUID 7
#define CAP_SETPCAP 8
#define CAP_LINUX_IMMUTABLE 9
#define CAP_NET_BIND_SERVICE 10
#define CAP_NET_BROADCAST 11
#define CAP_NET_ADMIN 12
#define CAP_NET_RAW 13
#define CAP_IPC_LOCK 14
#define CAP_IPC_OWNER 15
#define CAP_SYS_MODULE 16
#define CAP_SYS_RAWIO 17
#define CAP_SYS_CHROOT 18
#define CAP_SYS_PTRACE 19
#define CAP_SYS_PACCT 20
#define CAP_SYS_ADMIN 21
#define CAP_SYS_BOOT 22
#define CAP_SYS_NICE 23
#define CAP_SYS_RESOURCE 24
#define CAP_SYS_TIME 25
#define CAP_SYS_TTY_CONFIG 26
#ifdef __KERNEL__
extern kernel_cap_t cap_bset;
#ifdef STRICT_CAP_T_TYPECHECKS
#define to_cap_t(x) { x }
#define cap_t(x) (x).cap
#else
#define to_cap_t(x) (x)
#define cap_t(x) (x)
#endif
#define CAP_EMPTY_SET to_cap_t(0)
#define CAP_FULL_SET to_cap_t(~0)
#define CAP_INIT_EFF_SET to_cap_t(~0 & ~CAP_TO_MASK(CAP_SETPCAP))
#define CAP_INIT_INH_SET to_cap_t(~0 & ~CAP_TO_MASK(CAP_SETPCAP))
#define CAP_TO_MASK(x) (1 << (x))
#define cap_raise(c, flag) (cap_t(c) |= CAP_TO_MASK(flag))
#define cap_lower(c, flag) (cap_t(c) &= ~CAP_TO_MASK(flag))
#define cap_raised(c, flag) (cap_t(c) & CAP_TO_MASK(flag) & cap_bset)
static inline kernel_cap_t cap_combine(kernel_cap_t a, kernel_cap_t b)
{
kernel_cap_t dest;
cap_t(dest) = cap_t(a) | cap_t(b);
return dest;
}
static inline kernel_cap_t cap_intersect(kernel_cap_t a, kernel_cap_t b)
{
kernel_cap_t dest;
cap_t(dest) = cap_t(a) & cap_t(b);
return dest;
}
static inline kernel_cap_t cap_drop(kernel_cap_t a, kernel_cap_t drop)
{
kernel_cap_t dest;
cap_t(dest) = cap_t(a) & ~cap_t(drop);
return dest;
}
static inline kernel_cap_t cap_invert(kernel_cap_t c)
{
kernel_cap_t dest;
cap_t(dest) = ~cap_t(c);
return dest;
}
#define cap_isclear(c) (!cap_t(c))
#define cap_issubset(a,set) (!(cap_t(a) & ~cap_t(set)))
#define cap_clear(c) do { cap_t(c) = 0; } while(0)
#define cap_set_full(c) do { cap_t(c) = ~0; } while(0)
#define cap_mask(c,mask) do { cap_t(c) &= cap_t(mask); } while(0)
#define cap_is_fs_cap(c) (CAP_TO_MASK(c) & CAP_FS_MASK)
#endif
#endif