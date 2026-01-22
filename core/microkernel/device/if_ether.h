#ifndef	_DEVICE_IF_ETHER_H_
#define _DEVICE_IF_ETHER_H_
#include <sys/types.h>
struct	ether_header {
u_char	ether_dhost[6];
u_char	ether_shost[6];
u_short	ether_type;
};
#ifdef	KERNEL
extern char *	ether_sprintf(const u_char *);
#endif
#endif