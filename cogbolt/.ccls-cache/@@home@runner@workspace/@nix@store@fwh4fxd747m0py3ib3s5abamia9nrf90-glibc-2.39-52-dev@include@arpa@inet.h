#ifndef _ARPA_INET_H
#define	_ARPA_INET_H	1
#include <features.h>
#include <netinet/in.h>
#ifndef __socklen_t_defined
typedef __socklen_t socklen_t;
# define __socklen_t_defined
#endif
__BEGIN_DECLS
extern in_addr_t inet_addr (const char *__cp) __THROW;
extern in_addr_t inet_lnaof (struct in_addr __in) __THROW;
extern struct in_addr inet_makeaddr (in_addr_t __net, in_addr_t __host)
__THROW;
extern in_addr_t inet_netof (struct in_addr __in) __THROW;
extern in_addr_t inet_network (const char *__cp) __THROW;
extern char *inet_ntoa (struct in_addr __in) __THROW;
extern int inet_pton (int __af, const char *__restrict __cp,
void *__restrict __buf) __THROW;
extern const char *inet_ntop (int __af, const void *__restrict __cp,
char *__restrict __buf, socklen_t __len)
__THROW;
#ifdef __USE_MISC
extern int inet_aton (const char *__cp, struct in_addr *__inp) __THROW;
extern char *inet_neta (in_addr_t __net, char *__buf, size_t __len) __THROW
__attribute_deprecated_msg__ ("Use inet_ntop instead");
extern char *inet_net_ntop (int __af, const void *__cp, int __bits,
char *__buf, size_t __len) __THROW;
extern int inet_net_pton (int __af, const char *__cp,
void *__buf, size_t __len) __THROW;
extern unsigned int inet_nsap_addr (const char *__cp,
unsigned char *__buf, int __len) __THROW;
extern char *inet_nsap_ntoa (int __len, const unsigned char *__cp,
char *__buf) __THROW;
#endif
__END_DECLS
#endif