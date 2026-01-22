#ifndef	_MACH_SA_SYS_TYPES_H_
#define	_MACH_SA_SYS_TYPES_H_
#include <mach/machine/vm_types.h>
#include <stdint.h>
#include <stddef.h>
typedef	unsigned short	dev_t;
typedef	unsigned long	gid_t;
typedef	unsigned long	ino_t;
typedef	unsigned short	mode_t;
typedef	unsigned short	nlink_t;
typedef	natural_t	off_t;
typedef	unsigned long	uid_t;
#ifndef _TIME_T
#define	_TIME_T
typedef	unsigned long long	time_t;
#endif
#ifndef _POSIX_SOURCE
typedef	unsigned char	u_char;
typedef	unsigned short	u_short;
typedef	unsigned int	u_int;
typedef unsigned long	u_long;
#define	major(i)	(((i) >> 8) & 0xFF)
#define	minor(i)	((i) & 0xFF)
#define	makedev(i,j)	((((i) & 0xFF) << 8) | ((j) & 0xFF))
#endif
#endif