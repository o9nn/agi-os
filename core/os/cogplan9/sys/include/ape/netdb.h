#ifndef __NETDB_H__
#define __NETDB_H__
#ifndef _BSD_EXTENSION
This header file is an extension to ANSI/POSIX
#endif
#pragma lib "/$M/lib/ape/libbsd.a"
#ifdef __cplusplus
extern "C" {
#endif
struct hostent {
char *h_name;
char **h_aliases;
int h_addrtype;
int h_length;
char **h_addr_list;
#define h_addr h_addr_list[0]
};
struct netent {
char *n_name;
char **n_aliases;
int n_addrtype;
unsigned long n_net;
};
struct servent {
char *s_name;
char **s_aliases;
int s_port;
char *s_proto;
};
struct protoent {
char *p_name;
char **p_aliases;
int p_proto;
};
struct rpcent {
char *r_name;
char **r_aliases;
int r_number;
};
extern struct hostent *gethostbyname(const char *),
*gethostbyaddr(const void *, int, int),
*gethostent(void);
extern struct netent *getnetbyname(const char *),
*getnetbyaddr(long, int),
*getnetent(void);
extern struct servent *getservbyname(const char *, const char *),
*getservbyport(int, const char *),
*getservent(void);
extern struct protoent *getprotobyname(const char *),
*getprotobynumber(int),
*getprotoent(void);
extern struct rpcent *getrpcbyname(const char *),
*getrpcbynumber(int),
*getrpcent(void);
extern void sethostent(int), endhostent(void),
setnetent(int), endnetent(void),
setservent(int), endservent(void),
setprotoent(int), endprotoent(void),
setrpcent(int), endrpcent(void);
extern int h_errno;
extern void herror(const char *);
extern char *hstrerror(int);
#define HOST_NOT_FOUND 1
#define TRY_AGAIN 2
#define NO_RECOVERY 3
#define NO_DATA 4
#define NO_ADDRESS NO_DATA
#define __HOST_SVC_NOT_AVAIL 99
#ifdef __cplusplus
}
#endif
#endif