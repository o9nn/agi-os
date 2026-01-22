#include <asm/uaccess.h>
#include <asm/system.h>
#include <linux/types.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/mm.h>
#include <linux/socket.h>
#include <linux/in.h>
#include <linux/errno.h>
#include <linux/stat.h>
#include <stdarg.h>
#include <linux/inet.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <net/ip.h>
#include <net/protocol.h>
#include <net/tcp.h>
#include <linux/skbuff.h>
char *in_ntoa(__u32 in)
{
static char buff[18];
char *p;
p = (char *) &in;
sprintf(buff, "%d.%d.%d.%d",
(p[0] & 255), (p[1] & 255), (p[2] & 255), (p[3] & 255));
return(buff);
}
__u32 in_aton(const char *str)
{
unsigned long l;
unsigned int val;
int i;
l = 0;
for (i = 0; i < 4; i++)
{
l <<= 8;
if (*str != '\0')
{
val = 0;
while (*str != '\0' && *str != '.')
{
val *= 10;
val += *str - '0';
str++;
}
l |= val;
if (*str != '\0')
str++;
}
}
return(htonl(l));
}