#include <string.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
char *
canon_host (char *host)
{
struct hostent *he = gethostbyname (host);
if (he)
{
char *addr = 0;
switch (he->h_addrtype)
{
case AF_INET:
addr = inet_ntoa (*(struct in_addr *)he->h_addr);
break;
}
if (addr && strcmp (he->h_name, addr) == 0)
he = gethostbyaddr (he->h_addr, he->h_length, he->h_addrtype);
if (he)
return he->h_name;
}
return 0;
}