#include <linux/config.h>
#include <linux/types.h>
#include <linux/string.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/random.h>
#include <linux/init.h>
#include <linux/utsname.h>
#include <linux/in.h>
#include <linux/if.h>
#include <linux/inet.h>
#include <linux/netdevice.h>
#include <linux/if_arp.h>
#include <linux/skbuff.h>
#include <linux/ip.h>
#include <linux/socket.h>
#include <linux/route.h>
#include <linux/udp.h>
#include <net/arp.h>
#include <net/ip.h>
#include <net/ipconfig.h>
#include <asm/segment.h>
#include <asm/uaccess.h>
#include <asm/checksum.h>
#undef IPCONFIG_DEBUG
#ifdef IPCONFIG_DEBUG
#define DBG(x) printk x
#else
#define DBG(x) do { } while(0)
#endif
#define CONF_BASE_TIMEOUT	(HZ*5)
#define CONF_RETRIES	 	10
#define CONF_TIMEOUT_RANDOM	(HZ)
#define CONF_TIMEOUT_MULT	*5/4
#define CONF_TIMEOUT_MAX	(HZ*30)
static char user_dev_name[IFNAMSIZ] __initdata = { 0, };
u32 ic_myaddr __initdata = INADDR_NONE;
u32 ic_servaddr __initdata = INADDR_NONE;
u32 ic_gateway __initdata = INADDR_NONE;
u32 ic_netmask __initdata = INADDR_NONE;
int ic_enable __initdata = 1;
int ic_host_name_set __initdata = 0;
int ic_set_manually __initdata = 0;
u32 root_server_addr __initdata = INADDR_NONE;
u8 root_server_path[256] __initdata = { 0, };
#if defined(CONFIG_IP_PNP_BOOTP) || defined(CONFIG_IP_PNP_RARP)
#define CONFIG_IP_PNP_DYNAMIC
static int ic_proto_enabled __initdata = 0
#ifdef CONFIG_IP_PNP_BOOTP
| IC_BOOTP
#endif
#ifdef CONFIG_IP_PNP_RARP
| IC_RARP
#endif
;
static int ic_got_reply __initdata = 0;
#else
static int ic_proto_enabled __initdata = 0;
#endif
static int ic_proto_have_if __initdata = 0;
struct ic_device {
struct ic_device *next;
struct device *dev;
unsigned short flags;
int able;
};
static struct ic_device *ic_first_dev __initdata = NULL;
static struct device *ic_dev __initdata = NULL;
static int __init ic_open_devs(void)
{
struct ic_device *d, **last;
struct device *dev;
unsigned short oflags;
last = &ic_first_dev;
for (dev = dev_base; dev; dev = dev->next)
if (user_dev_name[0] ? !strcmp(dev->name, user_dev_name) :
(!(dev->flags & IFF_LOOPBACK) &&
(dev->flags & (IFF_POINTOPOINT|IFF_BROADCAST)) &&
strncmp(dev->name, "dummy", 5))) {
int able = 0;
if (dev->mtu >= 364)
able |= IC_BOOTP;
else
printk(KERN_WARNING "BOOTP: Ignoring device %s, MTU %d too small", dev->name, dev->mtu);
if (!(dev->flags & IFF_NOARP))
able |= IC_RARP;
able &= ic_proto_enabled;
if (ic_proto_enabled && !able)
continue;
oflags = dev->flags;
if (dev_change_flags(dev, oflags | IFF_UP) < 0) {
printk(KERN_ERR "IP-Config: Failed to open %s\n", dev->name);
continue;
}
if (!(d = kmalloc(sizeof(struct ic_device), GFP_KERNEL)))
return -1;
d->dev = dev;
*last = d;
last = &d->next;
d->flags = oflags;
d->able = able;
ic_proto_have_if |= able;
DBG(("IP-Config: Opened %s (able=%d)\n", dev->name, able));
}
*last = NULL;
if (!ic_first_dev) {
if (user_dev_name[0])
printk(KERN_ERR "IP-Config: Device `%s' not found.\n", user_dev_name);
else
printk(KERN_ERR "IP-Config: No network devices available.\n");
return -1;
}
return 0;
}
static void __init ic_close_devs(void)
{
struct ic_device *d, *next;
struct device *dev;
next = ic_first_dev;
while ((d = next)) {
next = d->next;
dev = d->dev;
if (dev != ic_dev) {
DBG(("IP-Config: Downing %s\n", dev->name));
dev_change_flags(dev, d->flags);
}
kfree_s(d, sizeof(struct ic_device));
}
}
static inline void
set_sockaddr(struct sockaddr_in *sin, u32 addr, u16 port)
{
sin->sin_family = AF_INET;
sin->sin_addr.s_addr = addr;
sin->sin_port = port;
}
static int __init ic_dev_ioctl(unsigned int cmd, struct ifreq *arg)
{
int res;
mm_segment_t oldfs = get_fs();
set_fs(get_ds());
res = devinet_ioctl(cmd, arg);
set_fs(oldfs);
return res;
}
static int __init ic_route_ioctl(unsigned int cmd, struct rtentry *arg)
{
int res;
mm_segment_t oldfs = get_fs();
set_fs(get_ds());
res = ip_rt_ioctl(cmd, arg);
set_fs(oldfs);
return res;
}
static int __init ic_setup_if(void)
{
struct ifreq ir;
struct sockaddr_in *sin = (void *) &ir.ifr_ifru.ifru_addr;
int err;
memset(&ir, 0, sizeof(ir));
strcpy(ir.ifr_ifrn.ifrn_name, ic_dev->name);
set_sockaddr(sin, ic_myaddr, 0);
if ((err = ic_dev_ioctl(SIOCSIFADDR, &ir)) < 0) {
printk(KERN_ERR "IP-Config: Unable to set interface address (%d).\n", err);
return -1;
}
set_sockaddr(sin, ic_netmask, 0);
if ((err = ic_dev_ioctl(SIOCSIFNETMASK, &ir)) < 0) {
printk(KERN_ERR "IP-Config: Unable to set interface netmask (%d).\n", err);
return -1;
}
set_sockaddr(sin, ic_myaddr | ~ic_netmask, 0);
if ((err = ic_dev_ioctl(SIOCSIFBRDADDR, &ir)) < 0) {
printk(KERN_ERR "IP-Config: Unable to set interface broadcast address (%d).\n", err);
return -1;
}
return 0;
}
static int __init ic_setup_routes(void)
{
if (ic_gateway != INADDR_NONE) {
struct rtentry rm;
int err;
memset(&rm, 0, sizeof(rm));
if ((ic_gateway ^ ic_myaddr) & ic_netmask) {
printk(KERN_ERR "IP-Config: Gateway not on directly connected network.\n");
return -1;
}
set_sockaddr((struct sockaddr_in *) &rm.rt_dst, 0, 0);
set_sockaddr((struct sockaddr_in *) &rm.rt_genmask, 0, 0);
set_sockaddr((struct sockaddr_in *) &rm.rt_gateway, ic_gateway, 0);
rm.rt_flags = RTF_UP | RTF_GATEWAY;
if ((err = ic_route_ioctl(SIOCADDRT, &rm)) < 0) {
printk(KERN_ERR "IP-Config: Cannot add default route (%d).\n", err);
return -1;
}
}
return 0;
}
static int __init ic_defaults(void)
{
if (!ic_host_name_set)
strcpy(system_utsname.nodename, in_ntoa(ic_myaddr));
if (root_server_addr == INADDR_NONE)
root_server_addr = ic_servaddr;
if (ic_netmask == INADDR_NONE) {
if (IN_CLASSA(ntohl(ic_myaddr)))
ic_netmask = htonl(IN_CLASSA_NET);
else if (IN_CLASSB(ntohl(ic_myaddr)))
ic_netmask = htonl(IN_CLASSB_NET);
else if (IN_CLASSC(ntohl(ic_myaddr)))
ic_netmask = htonl(IN_CLASSC_NET);
else {
printk(KERN_ERR "IP-Config: Unable to guess netmask for address %08x\n", ic_myaddr);
return -1;
}
printk("IP-Config: Guessing netmask %s\n", in_ntoa(ic_netmask));
}
return 0;
}
#ifdef CONFIG_IP_PNP_RARP
static int ic_rarp_recv(struct sk_buff *skb, struct device *dev, struct packet_type *pt);
static struct packet_type rarp_packet_type __initdata = {
__constant_htons(ETH_P_RARP),
NULL,
ic_rarp_recv,
NULL,
NULL
};
static inline void ic_rarp_init(void)
{
dev_add_pack(&rarp_packet_type);
}
static inline void ic_rarp_cleanup(void)
{
dev_remove_pack(&rarp_packet_type);
}
static int __init
ic_rarp_recv(struct sk_buff *skb, struct device *dev, struct packet_type *pt)
{
struct arphdr *rarp = (struct arphdr *)skb->h.raw;
unsigned char *rarp_ptr = (unsigned char *) (rarp + 1);
unsigned long sip, tip;
unsigned char *sha, *tha;
if (ic_got_reply)
goto drop;
if (rarp->ar_hln != dev->addr_len || dev->type != ntohs(rarp->ar_hrd))
goto drop;
if (rarp->ar_op != htons(ARPOP_RREPLY))
goto drop;
if (rarp->ar_pro != htons(ETH_P_IP))
goto drop;
sha = rarp_ptr;
rarp_ptr += dev->addr_len;
memcpy(&sip, rarp_ptr, 4);
rarp_ptr += 4;
tha = rarp_ptr;
rarp_ptr += dev->addr_len;
memcpy(&tip, rarp_ptr, 4);
if (memcmp(tha, dev->dev_addr, dev->addr_len))
goto drop;
if (ic_servaddr != INADDR_NONE && ic_servaddr != sip)
goto drop;
if (!ic_got_reply) {
ic_got_reply = IC_RARP;
ic_dev = dev;
if (ic_myaddr == INADDR_NONE)
ic_myaddr = tip;
ic_servaddr = sip;
}
drop:
kfree_skb(skb);
return 0;
}
static void __init ic_rarp_send(void)
{
struct ic_device *d;
for (d=ic_first_dev; d; d=d->next)
if (d->able & IC_RARP) {
struct device *dev = d->dev;
arp_send(ARPOP_RREQUEST, ETH_P_RARP, 0, dev, 0, NULL,
dev->dev_addr, dev->dev_addr);
}
}
#endif
#ifdef CONFIG_IP_PNP_BOOTP
struct bootp_pkt {
struct iphdr iph;
struct udphdr udph;
u8 op;
u8 htype;
u8 hlen;
u8 hops;
u32 xid;
u16 secs;
u16 flags;
u32 client_ip;
u32 your_ip;
u32 server_ip;
u32 relay_ip;
u8 hw_addr[16];
u8 serv_name[64];
u8 boot_file[128];
u8 vendor_area[128];
};
#define BOOTP_REQUEST 1
#define BOOTP_REPLY 2
static u32 ic_bootp_xid;
static int ic_bootp_recv(struct sk_buff *skb, struct device *dev, struct packet_type *pt);
static struct packet_type bootp_packet_type __initdata = {
__constant_htons(ETH_P_IP),
NULL,
ic_bootp_recv,
NULL,
NULL
};
static void __init ic_bootp_init_ext(u8 *e)
{
*e++ = 99;
*e++ = 130;
*e++ = 83;
*e++ = 99;
*e++ = 1;
*e++ = 4;
e += 4;
*e++ = 3;
*e++ = 4;
e += 4;
*e++ = 12;
*e++ = 32;
e += 32;
*e++ = 40;
*e++ = 32;
e += 32;
*e++ = 17;
*e++ = 32;
e += 32;
*e = 255;
}
static inline void ic_bootp_init(void)
{
get_random_bytes(&ic_bootp_xid, sizeof(u32));
DBG(("BOOTP: XID=%08x\n", ic_bootp_xid));
dev_add_pack(&bootp_packet_type);
}
static inline void ic_bootp_cleanup(void)
{
dev_remove_pack(&bootp_packet_type);
}
static void __init ic_bootp_send_if(struct ic_device *d, u32 jiffies)
{
struct device *dev = d->dev;
struct sk_buff *skb;
struct bootp_pkt *b;
int hh_len = (dev->hard_header_len + 15) & ~15;
struct iphdr *h;
skb = alloc_skb(sizeof(struct bootp_pkt) + hh_len + 15, GFP_KERNEL);
if (!skb)
return;
skb_reserve(skb, hh_len);
b = (struct bootp_pkt *) skb_put(skb, sizeof(struct bootp_pkt));
memset(b, 0, sizeof(struct bootp_pkt));
skb->nh.iph = h = &b->iph;
h->version = 4;
h->ihl = 5;
h->tot_len = htons(sizeof(struct bootp_pkt));
h->frag_off = htons(IP_DF);
h->ttl = 64;
h->protocol = IPPROTO_UDP;
h->daddr = INADDR_BROADCAST;
h->check = ip_fast_csum((unsigned char *) h, h->ihl);
b->udph.source = htons(68);
b->udph.dest = htons(67);
b->udph.len = htons(sizeof(struct bootp_pkt) - sizeof(struct iphdr));
b->op = BOOTP_REQUEST;
b->htype = dev->type;
b->hlen = dev->addr_len;
memcpy(b->hw_addr, dev->dev_addr, dev->addr_len);
b->secs = htons(jiffies / HZ);
b->xid = ic_bootp_xid;
ic_bootp_init_ext(b->vendor_area);
skb->dev = dev;
skb->protocol = __constant_htons(ETH_P_IP);
if ((dev->hard_header &&
dev->hard_header(skb, dev, ntohs(skb->protocol), dev->broadcast, dev->dev_addr, skb->len) < 0) ||
dev_queue_xmit(skb) < 0)
printk("E");
}
static void __init ic_bootp_send(u32 jiffies)
{
struct ic_device *d;
for(d=ic_first_dev; d; d=d->next)
if (d->able & IC_BOOTP)
ic_bootp_send_if(d, jiffies);
}
static int __init ic_bootp_string(char *dest, char *src, int len, int max)
{
if (!len)
return 0;
if (len > max-1)
len = max-1;
strncpy(dest, src, len);
dest[len] = '\0';
return 1;
}
static void __init ic_do_bootp_ext(u8 *ext)
{
#ifdef IPCONFIG_DEBUG
u8 *c;
printk("BOOTP: Got extension %02x",*ext);
for(c=ext+2; c<ext+2+ext[1]; c++)
printk(" %02x", *c);
printk("\n");
#endif
switch (*ext++) {
case 1:
if (ic_netmask == INADDR_NONE)
memcpy(&ic_netmask, ext+1, 4);
break;
case 3:
if (ic_gateway == INADDR_NONE)
memcpy(&ic_gateway, ext+1, 4);
break;
case 12:
ic_bootp_string(system_utsname.nodename, ext+1, *ext, __NEW_UTS_LEN);
ic_host_name_set = 1;
break;
case 40:
ic_bootp_string(system_utsname.domainname, ext+1, *ext, __NEW_UTS_LEN);
break;
case 17:
if (!root_server_path[0])
ic_bootp_string(root_server_path, ext+1, *ext, sizeof(root_server_path));
break;
}
}
static int __init ic_bootp_recv(struct sk_buff *skb, struct device *dev, struct packet_type *pt)
{
struct bootp_pkt *b = (struct bootp_pkt *) skb->nh.iph;
struct iphdr *h = &b->iph;
int len;
if (ic_got_reply)
goto drop;
if (skb->pkt_type == PACKET_OTHERHOST ||
skb->len < sizeof(struct udphdr) + sizeof(struct iphdr) ||
h->ihl != 5 ||
h->version != 4 ||
ip_fast_csum((char *) h, h->ihl) != 0 ||
skb->len < ntohs(h->tot_len) ||
h->protocol != IPPROTO_UDP ||
b->udph.source != htons(67) ||
b->udph.dest != htons(68) ||
ntohs(h->tot_len) < ntohs(b->udph.len) + sizeof(struct iphdr))
goto drop;
if (h->frag_off & htons(IP_OFFSET|IP_MF)) {
printk(KERN_ERR "BOOTP: Ignoring fragmented reply.\n");
goto drop;
}
len = ntohs(b->udph.len) - sizeof(struct udphdr);
if (len < 300 ||
b->op != BOOTP_REPLY ||
b->xid != ic_bootp_xid) {
printk("?");
goto drop;
}
ic_myaddr = b->your_ip;
ic_servaddr = b->server_ip;
ic_got_reply = IC_BOOTP;
ic_dev = dev;
if (b->vendor_area[0] == 99 &&
b->vendor_area[1] == 130 &&
b->vendor_area[2] == 83 &&
b->vendor_area[3] == 99) {
u8 *ext = &b->vendor_area[4];
u8 *end = (u8 *) b + ntohs(b->iph.tot_len);
while (ext < end && *ext != 0xff) {
if (*ext == 0)
ext++;
else {
u8 *opt = ext;
ext += ext[1] + 2;
if (ext <= end)
ic_do_bootp_ext(opt);
}
}
}
if (ic_gateway == INADDR_NONE && b->relay_ip)
ic_gateway = b->relay_ip;
drop:
kfree_skb(skb);
return 0;
}
#endif
#ifdef CONFIG_IP_PNP_DYNAMIC
static int __init ic_dynamic(void)
{
int retries;
unsigned long timeout, jiff;
unsigned long start_jiffies;
int do_rarp = ic_proto_have_if & IC_RARP;
int do_bootp = ic_proto_have_if & IC_BOOTP;
if (!ic_proto_enabled) {
printk(KERN_ERR "IP-Config: Incomplete network configuration information.\n");
return -1;
}
#ifdef CONFIG_IP_PNP_BOOTP
if ((ic_proto_enabled ^ ic_proto_have_if) & IC_BOOTP)
printk(KERN_ERR "BOOTP: No suitable device found.\n");
#endif
#ifdef CONFIG_IP_PNP_RARP
if ((ic_proto_enabled ^ ic_proto_have_if) & IC_RARP)
printk(KERN_ERR "RARP: No suitable device found.\n");
#endif
if (!ic_proto_have_if)
return -1;
#ifdef CONFIG_IP_PNP_RARP
if (do_rarp)
ic_rarp_init();
#endif
#ifdef CONFIG_IP_PNP_BOOTP
if (do_bootp)
ic_bootp_init();
#endif
printk(KERN_NOTICE "Sending %s%s%s requests...",
do_bootp ? "BOOTP" : "",
do_bootp && do_rarp ? " and " : "",
do_rarp ? "RARP" : "");
start_jiffies = jiffies;
retries = CONF_RETRIES;
get_random_bytes(&timeout, sizeof(timeout));
timeout = CONF_BASE_TIMEOUT + (timeout % (unsigned) CONF_TIMEOUT_RANDOM);
for(;;) {
#ifdef CONFIG_IP_PNP_BOOTP
if (do_bootp)
ic_bootp_send(jiffies - start_jiffies);
#endif
#ifdef CONFIG_IP_PNP_RARP
if (do_rarp)
ic_rarp_send();
#endif
printk(".");
jiff = jiffies + timeout;
while (jiffies < jiff && !ic_got_reply)
;
if (ic_got_reply) {
printk(" OK\n");
break;
}
if (! --retries) {
printk(" timed out!\n");
break;
}
timeout = timeout CONF_TIMEOUT_MULT;
if (timeout > CONF_TIMEOUT_MAX)
timeout = CONF_TIMEOUT_MAX;
}
#ifdef CONFIG_IP_PNP_RARP
if (do_rarp)
ic_rarp_cleanup();
#endif
#ifdef CONFIG_IP_PNP_BOOTP
if (do_bootp)
ic_bootp_cleanup();
#endif
if (!ic_got_reply)
return -1;
printk("IP-Config: Got %s answer from %s, ",
(ic_got_reply & IC_BOOTP) ? "BOOTP" : "RARP",
in_ntoa(ic_servaddr));
printk("my address is %s\n", in_ntoa(ic_myaddr));
return 0;
}
#endif
int __init ip_auto_config(void)
{
if (!ic_enable)
return 0;
DBG(("IP-Config: Entered.\n"));
if (ic_open_devs() < 0)
return -1;
if (ic_myaddr == INADDR_NONE ||
#ifdef CONFIG_ROOT_NFS
(root_server_addr == INADDR_NONE && ic_servaddr == INADDR_NONE) ||
#endif
ic_first_dev->next) {
#ifdef CONFIG_IP_PNP_DYNAMIC
if (ic_dynamic() < 0) {
printk(KERN_ERR "IP-Config: Auto-configuration of network failed.\n");
ic_close_devs();
return -1;
}
#else
printk(KERN_ERR "IP-Config: Incomplete network configuration information.\n");
ic_close_devs();
return -1;
#endif
} else {
ic_dev = ic_first_dev->dev;
}
if (ic_defaults() < 0)
return -1;
ic_close_devs();
if (ic_setup_if() < 0 || ic_setup_routes() < 0)
return -1;
DBG(("IP-Config: device=%s, local=%08x, server=%08x, boot=%08x, gw=%08x, mask=%08x\n",
ic_dev->name, ic_myaddr, ic_servaddr, root_server_addr, ic_gateway, ic_netmask));
DBG(("IP-Config: host=%s, domain=%s, path=`%s'\n", system_utsname.nodename,
system_utsname.domainname, root_server_path));
return 0;
}
static int __init ic_proto_name(char *name)
{
if (!strcmp(name, "off")) {
ic_proto_enabled = 0;
return 1;
}
#ifdef CONFIG_IP_PNP_BOOTP
else if (!strcmp(name, "bootp")) {
ic_proto_enabled &= ~IC_RARP;
return 1;
}
#endif
#ifdef CONFIG_IP_PNP_RARP
else if (!strcmp(name, "rarp")) {
ic_proto_enabled &= ~IC_BOOTP;
return 1;
}
#endif
#ifdef CONFIG_IP_PNP_DYNAMIC
else if (!strcmp(name, "both")) {
return 1;
}
#endif
return 0;
}
void __init ip_auto_config_setup(char *addrs, int *ints)
{
char *cp, *ip, *dp;
int num = 0;
ic_set_manually = 1;
if (!strcmp(addrs, "off")) {
ic_enable = 0;
return;
}
if (ic_proto_name(addrs))
return;
ip = addrs;
while (ip && *ip) {
if ((cp = strchr(ip, ':')))
*cp++ = '\0';
if (strlen(ip) > 0) {
DBG(("IP-Config: Parameter #%d: `%s'\n", num, ip));
switch (num) {
case 0:
if ((ic_myaddr = in_aton(ip)) == INADDR_ANY)
ic_myaddr = INADDR_NONE;
break;
case 1:
if ((ic_servaddr = in_aton(ip)) == INADDR_ANY)
ic_servaddr = INADDR_NONE;
break;
case 2:
if ((ic_gateway = in_aton(ip)) == INADDR_ANY)
ic_gateway = INADDR_NONE;
break;
case 3:
if ((ic_netmask = in_aton(ip)) == INADDR_ANY)
ic_netmask = INADDR_NONE;
break;
case 4:
if ((dp = strchr(ip, '.'))) {
*dp++ = '\0';
strncpy(system_utsname.domainname, dp, __NEW_UTS_LEN);
system_utsname.domainname[__NEW_UTS_LEN] = '\0';
}
strncpy(system_utsname.nodename, ip, __NEW_UTS_LEN);
system_utsname.nodename[__NEW_UTS_LEN] = '\0';
ic_host_name_set = 1;
break;
case 5:
strncpy(user_dev_name, ip, IFNAMSIZ);
user_dev_name[IFNAMSIZ-1] = '\0';
break;
case 6:
ic_proto_name(ip);
break;
}
}
ip = cp;
num++;
}
}