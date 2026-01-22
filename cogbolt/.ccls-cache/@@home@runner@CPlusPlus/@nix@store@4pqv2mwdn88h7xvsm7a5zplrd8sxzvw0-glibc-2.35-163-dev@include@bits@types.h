#ifndef _BITS_TYPES_H
#define _BITS_TYPES_H 1
#include <features.h>
#include <bits/wordsize.h>
#include <bits/timesize.h>
typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;
typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;
#if __WORDSIZE == 64
typedef signed long int __int64_t;
typedef unsigned long int __uint64_t;
#else
__extension__ typedef signed long long int __int64_t;
__extension__ typedef unsigned long long int __uint64_t;
#endif
typedef __int8_t __int_least8_t;
typedef __uint8_t __uint_least8_t;
typedef __int16_t __int_least16_t;
typedef __uint16_t __uint_least16_t;
typedef __int32_t __int_least32_t;
typedef __uint32_t __uint_least32_t;
typedef __int64_t __int_least64_t;
typedef __uint64_t __uint_least64_t;
#if __WORDSIZE == 64
typedef long int __quad_t;
typedef unsigned long int __u_quad_t;
#else
__extension__ typedef long long int __quad_t;
__extension__ typedef unsigned long long int __u_quad_t;
#endif
#if __WORDSIZE == 64
typedef long int __intmax_t;
typedef unsigned long int __uintmax_t;
#else
__extension__ typedef long long int __intmax_t;
__extension__ typedef unsigned long long int __uintmax_t;
#endif
#define __S16_TYPE short int
#define __U16_TYPE unsigned short int
#define __S32_TYPE int
#define __U32_TYPE unsigned int
#define __SLONGWORD_TYPE long int
#define __ULONGWORD_TYPE unsigned long int
#if __WORDSIZE == 32
# define __SQUAD_TYPE __int64_t
# define __UQUAD_TYPE __uint64_t
# define __SWORD_TYPE int
# define __UWORD_TYPE unsigned int
# define __SLONG32_TYPE long int
# define __ULONG32_TYPE unsigned long int
# define __S64_TYPE __int64_t
# define __U64_TYPE __uint64_t
# define __STD_TYPE __extension__ typedef
#elif __WORDSIZE == 64
# define __SQUAD_TYPE long int
# define __UQUAD_TYPE unsigned long int
# define __SWORD_TYPE long int
# define __UWORD_TYPE unsigned long int
# define __SLONG32_TYPE int
# define __ULONG32_TYPE unsigned int
# define __S64_TYPE long int
# define __U64_TYPE unsigned long int
# define __STD_TYPE typedef
#else
# error
#endif
#include <bits/typesizes.h>
#include <bits/time64.h>
__STD_TYPE __DEV_T_TYPE __dev_t;
__STD_TYPE __UID_T_TYPE __uid_t;
__STD_TYPE __GID_T_TYPE __gid_t;
__STD_TYPE __INO_T_TYPE __ino_t;
__STD_TYPE __INO64_T_TYPE __ino64_t;
__STD_TYPE __MODE_T_TYPE __mode_t;
__STD_TYPE __NLINK_T_TYPE __nlink_t;
__STD_TYPE __OFF_T_TYPE __off_t;
__STD_TYPE __OFF64_T_TYPE __off64_t;
__STD_TYPE __PID_T_TYPE __pid_t;
__STD_TYPE __FSID_T_TYPE __fsid_t;
__STD_TYPE __CLOCK_T_TYPE __clock_t;
__STD_TYPE __RLIM_T_TYPE __rlim_t;
__STD_TYPE __RLIM64_T_TYPE __rlim64_t;
__STD_TYPE __ID_T_TYPE __id_t;
__STD_TYPE __TIME_T_TYPE __time_t;
__STD_TYPE __USECONDS_T_TYPE __useconds_t;
__STD_TYPE __SUSECONDS_T_TYPE __suseconds_t;
__STD_TYPE __SUSECONDS64_T_TYPE __suseconds64_t;
__STD_TYPE __DADDR_T_TYPE __daddr_t;
__STD_TYPE __KEY_T_TYPE __key_t;
__STD_TYPE __CLOCKID_T_TYPE __clockid_t;
__STD_TYPE __TIMER_T_TYPE __timer_t;
__STD_TYPE __BLKSIZE_T_TYPE __blksize_t;
__STD_TYPE __BLKCNT_T_TYPE __blkcnt_t;
__STD_TYPE __BLKCNT64_T_TYPE __blkcnt64_t;
__STD_TYPE __FSBLKCNT_T_TYPE __fsblkcnt_t;
__STD_TYPE __FSBLKCNT64_T_TYPE __fsblkcnt64_t;
__STD_TYPE __FSFILCNT_T_TYPE __fsfilcnt_t;
__STD_TYPE __FSFILCNT64_T_TYPE __fsfilcnt64_t;
__STD_TYPE __FSWORD_T_TYPE __fsword_t;
__STD_TYPE __SSIZE_T_TYPE __ssize_t;
__STD_TYPE __SYSCALL_SLONG_TYPE __syscall_slong_t;
__STD_TYPE __SYSCALL_ULONG_TYPE __syscall_ulong_t;
typedef __off64_t __loff_t;
typedef char *__caddr_t;
__STD_TYPE __SWORD_TYPE __intptr_t;
__STD_TYPE __U32_TYPE __socklen_t;
typedef int __sig_atomic_t;
#if __TIMESIZE == 64 && defined __LIBC
# define __time64_t __time_t
#elif __TIMESIZE != 64
__STD_TYPE __TIME64_T_TYPE __time64_t;
#endif
#undef __STD_TYPE
#endif