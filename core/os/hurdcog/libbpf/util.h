#ifndef UTIL_H
#define UTIL_H
#include <stdio.h>
#include <execinfo.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/ip.h>
#include <mach.h>
#ifdef DEBUG
#define debug(format, ...) do \
{ \
char buf[1024]; \
snprintf (buf, 1024, "multiplexer: %s: %s\n", __func__, format); \
fprintf (stderr , buf, ## __VA_ARGS__); \
fflush (stderr); \
} while (0)
#else
#define debug(format, ...) do {} while (0)
#endif
#define print_backtrace() do \
{ \
size_t size; \
void *array[30]; \
size = backtrace (array, sizeof (array)); \
debug ("the depth of the stack: %d", size); \
backtrace_symbols_fd(array, size, fileno (stderr)); \
} while (0)
#define ETH_ALEN 6
struct ethhdr
{
unsigned char h_dest[ETH_ALEN];
unsigned char h_source[ETH_ALEN];
unsigned short h_proto;
};
static inline void
print_pack (char *packet, int len)
{
#ifdef DEBUG
#define ETH_P_IP 0x0800
struct ethhdr *ethh = (struct ethhdr *) packet;
struct iphdr *iph = (struct iphdr *)(ethh + 1);
char src_str[INET_ADDRSTRLEN];
char dst_str[INET_ADDRSTRLEN];
if (ntohs (ethh->h_proto) == ETH_P_IP
&& len >= sizeof (struct ethhdr) + sizeof (struct iphdr))
{
debug ("multiplexer: get a IP packet from %s to %s\n",
inet_ntop (AF_INET, &iph->saddr, src_str, INET_ADDRSTRLEN),
inet_ntop (AF_INET, &iph->daddr, dst_str, INET_ADDRSTRLEN));
}
else
{
debug ("multiplexer: get a non-IP packet\n");
}
#endif
}
#endif