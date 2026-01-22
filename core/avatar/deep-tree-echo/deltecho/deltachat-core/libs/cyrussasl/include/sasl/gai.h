#ifndef _GAI_H_
#define _GAI_H_
#ifndef NI_MAXHOST
#define	NI_MAXHOST	1025
#endif
#ifndef NI_MAXSERV
#define	NI_MAXSERV	32
#endif
#ifndef EAI_NODATA
#define EAI_NODATA	1
#define EAI_MEMORY	2
#define EAI_FAMILY	5
#define EAI_SERVICE	9
#endif
#ifndef AI_PASSIVE
#define AI_PASSIVE	1
#define AI_CANONNAME	2
struct addrinfo {
int	ai_flags;
int	ai_family;
int	ai_socktype;
int	ai_protocol;
size_t	ai_addrlen;
char	*ai_canonname;
struct sockaddr *ai_addr;
struct addrinfo *ai_next;
};
#endif
#ifdef __cplusplus
extern "C" {
#endif
#ifndef HAVE_GETNAMEINFO
int	getnameinfo(const struct sockaddr *, socklen_t, char *,
size_t, char *, size_t, int);
#endif
#ifndef HAVE_GETADDRINFO
int	getaddrinfo(const char *, const char *,
const struct addrinfo *, struct addrinfo **);
void	freeaddrinfo(struct addrinfo *);
char	*gai_strerror(int);
#endif
#ifdef __cplusplus
}
#endif
#endif