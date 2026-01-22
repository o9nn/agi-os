#include <u.h>
#include <libc.h>
#include <fcall.h>
#include <thread.h>
#include <libsec.h>
#include <9p.h>
extern char *Debug;
typedef struct Pingcache Pingcache;
struct Pingcache {
Pingcache*next;
long	rtt;
char	*host;
long	expire;
};
typedef struct {
uchar	vihl;
uchar	tos;
uchar	length[2];
uchar	id[2];
uchar	frag[2];
uchar	ttl;
uchar	proto;
uchar	ipcksum[2];
uchar	src[4];
uchar	dst[4];
uchar	type;
uchar	code;
uchar	cksum[2];
uchar	icmpid[2];
uchar	seq[2];
uchar	data[1];
} Icmp;
enum {
EchoReply	= 0,
Unreachable	= 3,
SrcQuench	= 4,
EchoRequest	= 8,
TimeExceed	= 11,
Timestamp	= 13,
TimestampReply	= 14,
InfoRequest	= 15,
InfoReply	= 16,
ICMP_IPSIZE	= 20,
ICMP_HDRSIZE	= 8,
Npings		= 8,
Payload		= 32,
Cachetime	= 60,
};
static Pingcache *Cache;
int
ping(char *host, int timeout)
{
int rtt, fd, i, seq;
long now;
vlong then;
uchar buf[128];
Icmp *ip;
Pingcache *c;
now = time(nil);
for(c = Cache; c; c = c->next)
if(strcmp(c->host, host) == 0 && now < c->expire){
if(Debug && strstr(Debug, "dfs") != nil)
print("\t\tping host=%s timeout=%d - cache hit\n",
host, timeout);
return c->rtt;
}
rtt = -1;
ip = (Icmp*)buf;
if((fd = dial(netmkaddr(host, "icmp", "1"), 0, 0, 0)) == -1)
goto fail;
for(seq = 0; seq < Npings; seq++){
then = nsec();
for(i = Payload; i < sizeof buf; i++)
buf[i] = i + seq;
ip->type = EchoRequest;
ip->code = 0;
ip->seq[0] = seq;
ip->seq[1] = seq;
alarm(timeout);
if(write(fd, ip, sizeof buf) != sizeof buf ||
read(fd, ip, sizeof buf) != sizeof buf)
goto fail;
alarm(0);
if(ip->type != EchoReply || ip->code != 0 ||
ip->seq[0] != seq || ip->seq[1] != seq)
goto fail;
for(i = Payload; i < sizeof buf; i++)
if((uchar)buf[i] != (uchar)(i + seq))
goto fail;
rtt = (rtt + nsec() - then) / 2;
}
fail:
if(fd != -1)
close(fd);
if(Debug && strstr(Debug, "dfs") != nil)
print("\t\tping host=%s timeout=%d rtt=%d - failed\n",
host, timeout, rtt);
for(c = Cache; c; c = c->next)
if(strcmp(c->host, host) == 0)
break;
if(c == nil){
c = emalloc9p(sizeof(Pingcache));
c->host = estrdup9p(host);
c->next = Cache;
Cache = c;
}
c->rtt = rtt;
c->expire = now+Cachetime;
return rtt;
}