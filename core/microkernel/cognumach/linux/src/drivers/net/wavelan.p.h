#ifndef WAVELAN_P_H
#define WAVELAN_P_H
#include	<linux/module.h>
#include	<linux/kernel.h>
#include	<linux/sched.h>
#include	<linux/types.h>
#include	<linux/fcntl.h>
#include	<linux/interrupt.h>
#include	<linux/stat.h>
#include	<linux/ptrace.h>
#include	<linux/ioport.h>
#include	<linux/in.h>
#include	<linux/string.h>
#include	<linux/delay.h>
#include	<asm/system.h>
#include	<asm/bitops.h>
#include	<asm/io.h>
#include	<asm/dma.h>
#include	<linux/errno.h>
#include	<linux/netdevice.h>
#include	<linux/etherdevice.h>
#include	<linux/skbuff.h>
#include	<linux/malloc.h>
#include	<linux/timer.h>
#include <linux/wireless.h>
#include	"i82586.h"
#include	"wavelan.h"
#undef DEBUG_MODULE_TRACE
#undef DEBUG_CALLBACK_TRACE
#undef DEBUG_INTERRUPT_TRACE
#undef DEBUG_INTERRUPT_INFO
#define DEBUG_INTERRUPT_ERROR
#undef DEBUG_CONFIG_TRACE
#undef DEBUG_CONFIG_INFO
#define DEBUG_CONFIG_ERRORS
#undef DEBUG_TX_TRACE
#undef DEBUG_TX_INFO
#define DEBUG_TX_ERROR
#undef DEBUG_RX_TRACE
#undef DEBUG_RX_INFO
#define DEBUG_RX_ERROR
#undef DEBUG_PACKET_DUMP	16
#undef DEBUG_IOCTL_TRACE
#undef DEBUG_IOCTL_INFO
#define DEBUG_IOCTL_ERROR
#define DEBUG_BASIC_SHOW
#undef DEBUG_VERSION_SHOW
#undef DEBUG_PSA_SHOW
#undef DEBUG_MMC_SHOW
#undef DEBUG_SHOW_UNUSED
#undef DEBUG_I82586_SHOW
#undef DEBUG_DEVICE_SHOW
#define USE_PSA_CONFIG
#define IGNORE_NORMAL_XMIT_ERRS
#undef STRUCT_CHECK
#undef PSA_CRC
#undef OLDIES
#undef RECORD_SNR
#undef EEPROM_IS_PROTECTED
#define MULTICAST_AVOID
#ifdef WIRELESS_EXT
#define WIRELESS_SPY
#undef HISTOGRAM
#endif
#ifdef DEBUG_VERSION_SHOW
static const char	*version	= "wavelan.c : v16 (wireless extensions) 17/4/97\n";
#endif
#define	WATCHDOG_JIFFIES	32
#define	NELS(a)				(sizeof(a) / sizeof(a[0]))
#define SIOCSIPQTHR	SIOCDEVPRIVATE
#define SIOCGIPQTHR	SIOCDEVPRIVATE + 1
#define SIOCSIPLTHR	SIOCDEVPRIVATE + 2
#define SIOCGIPLTHR	SIOCDEVPRIVATE + 3
#define SIOCSIPHISTO	SIOCDEVPRIVATE + 6
#define SIOCGIPHISTO	SIOCDEVPRIVATE + 7
#ifndef copy_from_user
#define copy_from_user	memcpy_fromfs
#define copy_to_user	memcpy_tofs
#endif
typedef struct device		device;
typedef struct enet_statistics	en_stats;
typedef struct iw_statistics	iw_stats;
typedef struct iw_quality	iw_qual;
typedef struct iw_freq		iw_freq;
typedef struct net_local	net_local;
typedef struct timer_list	timer_list;
typedef u_char		mac_addr[WAVELAN_ADDR_SIZE];
struct net_local
{
net_local *	next;
device *	dev;
en_stats	stats;
int		nresets;
u_char	reconfig_82586;
u_char	promiscuous;
int		mc_count;
timer_list	watchdog;
u_short	hacr;
int		tx_n_in_use;
u_short	rx_head;
u_short	rx_last;
u_short	tx_first_free;
u_short	tx_first_in_use;
#ifdef WIRELESS_EXT
iw_stats	wstats;
#endif
#ifdef WIRELESS_SPY
int		spy_number;
mac_addr	spy_address[IW_MAX_SPY];
iw_qual	spy_stat[IW_MAX_SPY];
#endif
#ifdef HISTOGRAM
int		his_number;
u_char	his_range[16];
u_long	his_sum[16];
#endif
};
static inline unsigned long
wv_splhi(void);
static inline void
wv_splx(unsigned long);
static u_char
wv_irq_to_psa(int);
static int
wv_psa_to_irq(u_char);
static inline u_short
hasr_read(u_long);
static inline void
hacr_write(u_long,
u_short),
hacr_write_slow(u_long,
u_short),
set_chan_attn(u_long,
u_short),
wv_hacr_reset(u_long),
wv_16_off(u_long,
u_short),
wv_16_on(u_long,
u_short),
wv_ints_off(device *),
wv_ints_on(device *);
static void
psa_read(u_long,
u_short,
int,
u_char *,
int),
psa_write(u_long,
u_short,
int,
u_char *,
int);
static inline void
mmc_out(u_long,
u_short,
u_char),
mmc_write(u_long,
u_char,
u_char *,
int);
static inline u_char
mmc_in(u_long,
u_short);
static inline void
mmc_read(u_long,
u_char,
u_char *,
int),
fee_wait(u_long,
int,
int);
static void
fee_read(u_long,
u_short,
u_short *,
int);
static  void
obram_read(u_long,
u_short,
u_char *,
int);
static inline void
obram_write(u_long,
u_short,
u_char *,
int);
static void
wv_ack(device *);
static inline int
wv_synchronous_cmd(device *,
const char *),
wv_config_complete(device *,
u_long,
net_local *);
static int
wv_complete(device *,
u_long,
net_local *);
static inline void
wv_82586_reconfig(device *);
#ifdef DEBUG_I82586_SHOW
static void
wv_scb_show(unsigned short);
#endif
static inline void
wv_init_info(device *);
static en_stats	*
wavelan_get_stats(device *);
static void
wavelan_set_multicast_list(device *);
static inline void
wv_packet_read(device *,
u_short,
int),
wv_receive(device *);
static inline void
wv_packet_write(device *,
void *,
short);
static int
wavelan_packet_xmit(struct sk_buff *,
device *);
static inline int
wv_mmc_init(device *),
wv_ru_start(device *),
wv_cu_start(device *),
wv_82586_start(device *);
static void
wv_82586_config(device *);
static inline void
wv_82586_stop(device *);
static int
wv_hw_reset(device *),
wv_check_ioaddr(u_long,
u_char *);
static void
wavelan_interrupt(int,
void *,
struct pt_regs *);
static void
wavelan_watchdog(u_long);
static int
wavelan_open(device *),
wavelan_close(device *),
wavelan_config(device *);
extern int
wavelan_probe(device *);
static net_local *	wavelan_list	= (net_local *) NULL;
static u_char	irqvals[]	=
{
0,    0,    0, 0x01,
0x02, 0x04,    0, 0x08,
0,    0, 0x10, 0x20,
0x40,    0,    0, 0x80,
};
static unsigned short	iobase[]	=
{
#if	0
0x300, 0x390, 0x3E0, 0x3C0
#endif
0x390, 0x3E0
};
#ifdef	MODULE
static char	devname[4][IFNAMSIZ] = { "", "", "", "" };
static int	io[4]	= { 0, 0, 0, 0 };
static int	irq[4]	= { 0, 0, 0, 0 };
static char *	name[4] = { devname[0], devname[1], devname[2], devname[3] };
#endif
#endif