#ifndef	_SYS_TYPES_H
#define	_SYS_TYPES_H	1
#include <features.h>
__BEGIN_DECLS
#include <bits/types.h>
#ifdef	__USE_MISC
# ifndef __u_char_defined
typedef __u_char u_char;
typedef __u_short u_short;
typedef __u_int u_int;
typedef __u_long u_long;
typedef __quad_t quad_t;
typedef __u_quad_t u_quad_t;
typedef __fsid_t fsid_t;
#  define __u_char_defined
# endif
typedef __loff_t loff_t;
#endif
#ifndef __ino_t_defined
# ifndef __USE_FILE_OFFSET64
typedef __ino_t ino_t;
# else
typedef __ino64_t ino_t;
# endif
# define __ino_t_defined
#endif
#if defined __USE_LARGEFILE64 && !defined __ino64_t_defined
typedef __ino64_t ino64_t;
# define __ino64_t_defined
#endif
#ifndef __dev_t_defined
typedef __dev_t dev_t;
# define __dev_t_defined
#endif
#ifndef __gid_t_defined
typedef __gid_t gid_t;
# define __gid_t_defined
#endif
#ifndef __mode_t_defined
typedef __mode_t mode_t;
# define __mode_t_defined
#endif
#ifndef __nlink_t_defined
typedef __nlink_t nlink_t;
# define __nlink_t_defined
#endif
#ifndef __uid_t_defined
typedef __uid_t uid_t;
# define __uid_t_defined
#endif
#ifndef __off_t_defined
# ifndef __USE_FILE_OFFSET64
typedef __off_t off_t;
# else
typedef __off64_t off_t;
# endif
# define __off_t_defined
#endif
#if defined __USE_LARGEFILE64 && !defined __off64_t_defined
typedef __off64_t off64_t;
# define __off64_t_defined
#endif
#ifndef __pid_t_defined
typedef __pid_t pid_t;
# define __pid_t_defined
#endif
#if (defined __USE_XOPEN || defined __USE_XOPEN2K8) \
&& !defined __id_t_defined
typedef __id_t id_t;
# define __id_t_defined
#endif
#ifndef __ssize_t_defined
typedef __ssize_t ssize_t;
# define __ssize_t_defined
#endif
#ifdef	__USE_MISC
# ifndef __daddr_t_defined
typedef __daddr_t daddr_t;
typedef __caddr_t caddr_t;
#  define __daddr_t_defined
# endif
#endif
#if (defined __USE_MISC || defined __USE_XOPEN) && !defined __key_t_defined
typedef __key_t key_t;
# define __key_t_defined
#endif
#if defined __USE_XOPEN || defined __USE_XOPEN2K8
# include <bits/types/clock_t.h>
#endif
#include <bits/types/clockid_t.h>
#include <bits/types/time_t.h>
#include <bits/types/timer_t.h>
#ifdef __USE_XOPEN
# ifndef __useconds_t_defined
typedef __useconds_t useconds_t;
#  define __useconds_t_defined
# endif
# ifndef __suseconds_t_defined
typedef __suseconds_t suseconds_t;
#  define __suseconds_t_defined
# endif
#endif
#define	__need_size_t
#include <stddef.h>
#ifdef __USE_MISC
typedef unsigned long int ulong;
typedef unsigned short int ushort;
typedef unsigned int uint;
#endif
#include <bits/stdint-intn.h>
typedef __uint8_t u_int8_t;
typedef __uint16_t u_int16_t;
typedef __uint32_t u_int32_t;
typedef __uint64_t u_int64_t;
#if __GNUC_PREREQ (2, 7)
typedef int register_t __attribute__ ((__mode__ (__word__)));
#else
typedef int register_t;
#endif
#define __BIT_TYPES_DEFINED__	1
#ifdef	__USE_MISC
# include <endian.h>
# include <sys/select.h>
#endif
#if (defined __USE_UNIX98 || defined __USE_XOPEN2K8) \
&& !defined __blksize_t_defined
typedef __blksize_t blksize_t;
# define __blksize_t_defined
#endif
#ifndef __USE_FILE_OFFSET64
# ifndef __blkcnt_t_defined
typedef __blkcnt_t blkcnt_t;
#  define __blkcnt_t_defined
# endif
# ifndef __fsblkcnt_t_defined
typedef __fsblkcnt_t fsblkcnt_t;
#  define __fsblkcnt_t_defined
# endif
# ifndef __fsfilcnt_t_defined
typedef __fsfilcnt_t fsfilcnt_t;
#  define __fsfilcnt_t_defined
# endif
#else
# ifndef __blkcnt_t_defined
typedef __blkcnt64_t blkcnt_t;
#  define __blkcnt_t_defined
# endif
# ifndef __fsblkcnt_t_defined
typedef __fsblkcnt64_t fsblkcnt_t;
#  define __fsblkcnt_t_defined
# endif
# ifndef __fsfilcnt_t_defined
typedef __fsfilcnt64_t fsfilcnt_t;
#  define __fsfilcnt_t_defined
# endif
#endif
#ifdef __USE_LARGEFILE64
typedef __blkcnt64_t blkcnt64_t;
typedef __fsblkcnt64_t fsblkcnt64_t;
typedef __fsfilcnt64_t fsfilcnt64_t;
#endif
#if defined __USE_POSIX199506 || defined __USE_UNIX98
# include <bits/pthreadtypes.h>
#endif
__END_DECLS
#endif