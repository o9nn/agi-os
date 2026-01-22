#ifndef _LINUX_ARCDEVICE_H
#define _LINUX_ARCDEVICE_H
#include <linux/config.h>
#include <linux/if_arcnet.h>
#ifdef __KERNEL__
#define ARC_20020     1
#define ARC_RIM_I     2
#define ARC_90xx      3
#define ARC_90xx_IO   4
#define MAX_ARCNET_DEVS 8
#define DETECT_RECONFIGS
#undef SHOW_RECONFIGS
#define RECON_THRESHOLD 30
#define TX_TIMEOUT (20*HZ/100)
#undef ALPHA_WARNING
#define D_NORMAL	1
#define D_EXTRA		2
#define	D_INIT		4
#define D_INIT_REASONS	8
#define D_DURING	16
#define D_TX		32
#define D_RX		64
#define D_SKB		128
#ifndef ARCNET_DEBUG_MAX
#define ARCNET_DEBUG_MAX (~0)
#endif
#ifndef ARCNET_DEBUG
#define ARCNET_DEBUG (D_NORMAL|D_EXTRA)
#endif
extern int arcnet_debug;
#define BUGLVL(x) if ((ARCNET_DEBUG_MAX)&arcnet_debug&(x))
#define BUGMSG2(x,msg,args...) do { BUGLVL(x) printk(msg, ## args); } while (0)
#define BUGMSG(x,msg,args...) \
BUGMSG2(x,"%s%6s: " msg, \
x==D_NORMAL	? KERN_WARNING : \
x<=D_INIT_REASONS	? KERN_INFO    : KERN_DEBUG , \
dev->name , ## args)
#define SETMASK AINTMASK(lp->intmask)
#define RESETtime (HZ * 3 / 10)
#define MTU	253
#define MinTU	257
#define XMTU	508
#define TXFREEflag	0x01
#define TXACKflag       0x02
#define RECONflag       0x04
#define TESTflag        0x08
#define RESETflag       0x10
#define RES1flag        0x20
#define RES2flag        0x40
#define NORXflag        0x80
#define AUTOINCflag     0x40
#define IOMAPflag       0x02
#define ENABLE16flag    0x80
#define NOTXcmd         0x01
#define NORXcmd         0x02
#define TXcmd           0x03
#define RXcmd           0x04
#define CONFIGcmd       0x05
#define CFLAGScmd       0x06
#define TESTcmd         0x07
#define RESETclear      0x08
#define CONFIGclear     0x10
#define TESTload        0x08
#define TESTvalue       0321
#define RXbcasts        0x80
#define NORMALconf      0x00
#define EXTconf         0x08
#define EnableReceiver()	ACOMMAND(RXcmd|(recbuf<<3)|RXbcasts)
#define JIFFER(time) for (delayval=jiffies+time; time_before(jiffies,delayval);) ;
union ArcPacket
{
struct archdr hardheader;
u_char raw[512];
};
struct ClientData
{
u_char  saddr,
daddr;
u_char	protocol_id,
split_flag;
u_short	sequence;
};
#define EXTRA_CLIENTDATA (sizeof(struct ClientData)-4)
struct S_ClientData
{
u_char  saddr,
daddr,
junk;
u_char	protocol_id;
};
#define S_EXTRA_CLIENTDATA (sizeof(struct S_ClientData)-1)
struct Incoming
{
struct sk_buff *skb;
unsigned char lastpacket,
numpackets;
u_short sequence;
};
struct Outgoing
{
struct sk_buff *skb;
struct ClientData *hdr;
u_char *data;
short length,
dataleft,
segnum,
numsegs,
seglen;
};
struct arcnet_local {
struct net_device_stats stats;
u_short sequence;
u_short aborted_seq;
u_char stationid,
recbuf,
txbuf,
txready,
config,
timeout,
backplane,
setup,
intmask;
short intx,
in_txhandler,
sending,
lastload_dest,
lasttrans_dest;
#if defined(DETECT_RECONFIGS) && defined(RECON_THRESHOLD)
time_t first_recon,
last_recon;
int num_recons,
network_down;
#endif
struct timer_list timer;
struct Incoming incoming[256];
struct Outgoing outgoing;
int card_type;
char *card_type_str;
void (*inthandler) (struct device *dev);
int (*arcnet_reset) (struct device *dev, int reset_delay);
void (*asetmask) (struct device *dev, u_char mask);
void (*acommand) (struct device *dev, u_char command);
u_char (*astatus) (struct device *dev);
void (*en_dis_able_TX) (struct device *dev, int enable);
void (*prepare_tx)(struct device *dev,u_char *hdr,int hdrlen,
char *data,int length,int daddr,int exceptA, int offset);
void (*openclose_device)(int open);
struct device *adev;
#ifdef CONFIG_ARCNET_ETH
struct device *edev;
#endif
#ifdef CONFIG_ARCNET_1051
struct device *sdev;
#endif
};
#if ARCNET_DEBUG_MAX & D_SKB
extern void arcnet_dump_skb(struct device *dev,struct sk_buff *skb,
char *desc);
#else
#define arcnet_dump_skb(dev,skb,desc) ;
#endif
#if (ARCNET_DEBUG_MAX & D_RX) || (ARCNET_DEBUG_MAX & D_TX)
extern void arcnet_dump_packet(struct device *dev,u_char *buffer,int ext,
char *desc);
#else
#define arcnet_dump_packet(dev,buffer,ext,desc) ;
#endif
extern void arcnet_tx_done(struct device *dev, struct arcnet_local *lp);
extern void arcnet_makename(char *device);
extern void arcnet_interrupt(int irq,void *dev_id,struct pt_regs *regs);
extern void arcnet_setup(struct device *dev);
extern int arcnet_go_tx(struct device *dev,int enable_irq);
extern void arcnetA_continue_tx(struct device *dev);
extern void arcnet_rx(struct arcnet_local *lp, u_char *arcsoft, short length, int saddr, int daddr);
extern void arcnet_use_count(int open);
#endif
#endif