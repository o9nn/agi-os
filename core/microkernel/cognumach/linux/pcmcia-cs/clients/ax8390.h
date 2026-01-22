#ifndef _8390_h
#define _8390_h
#include <linux/config.h>
#include <linux/if_ether.h>
#include <linux/ioport.h>
#include <linux/skbuff.h>
#define TX_2X_PAGES 12
#define TX_1X_PAGES 6
#define TX_PAGES TX_2X_PAGES
#define ETHER_ADDR_LEN 6
struct e8390_pkt_hdr {
unsigned char status;
unsigned char next;
unsigned short count;
};
#ifdef notdef
extern int ei_debug;
#else
#define ei_debug 1
#endif
#ifndef HAVE_AUTOIRQ
extern void autoirq_setup(int waittime);
extern unsigned long autoirq_report(int waittime);
#endif
struct ei_device {
const char *name;
void (*reset_8390)(struct net_device *);
void (*get_8390_hdr)(struct net_device *, struct e8390_pkt_hdr *, int);
void (*block_output)(struct net_device *, int, const unsigned char *, int);
void (*block_input)(struct net_device *, int, struct sk_buff *, int);
unsigned char mcfilter[8];
unsigned open:1;
unsigned word16:1;
unsigned txing:1;
unsigned irqlock:1;
unsigned dmaing:1;
unsigned char tx_start_page, rx_start_page, stop_page;
unsigned char current_page;
unsigned char interface_num;
unsigned char txqueue;
short tx1, tx2;
short lasttx;
unsigned char saved_irq;
struct net_device_stats stat;
spinlock_t page_lock;
unsigned long priv;
};
#define MAX_SERVICE 12
#define TX_TIMEOUT (20*HZ/100)
#define ei_status (*(struct ei_device *)(dev->priv))
#define E8390_TX_IRQ_MASK	0xa
#define E8390_RX_IRQ_MASK	0x5
#define E8390_RXCONFIG		0x4
#define E8390_RXOFF		0x20
#define E8390_TXCONFIG		0x00
#define E8390_TXOFF		0x02
#define E8390_STOP	0x01
#define E8390_START	0x02
#define E8390_TRANS	0x04
#define E8390_RREAD	0x08
#define E8390_RWRITE	0x10
#define E8390_NODMA	0x20
#define E8390_PAGE0	0x00
#define E8390_PAGE1	0x40
#define E8390_PAGE2	0x80
#define E8390_CMD	0x00
#define EN0_CLDALO	0x01
#define EN0_STARTPG	0x01
#define EN0_CLDAHI	0x02
#define EN0_STOPPG	0x02
#define EN0_BOUNDARY	0x03
#define EN0_TSR		0x04
#define EN0_TPSR	0x04
#define EN0_NCR		0x05
#define EN0_TCNTLO	0x05
#define EN0_FIFO	0x06
#define EN0_TCNTHI	0x06
#define EN0_ISR		0x07
#define EN0_CRDALO	0x08
#define EN0_RSARLO	0x08
#define EN0_CRDAHI	0x09
#define EN0_RSARHI	0x09
#define EN0_RCNTLO	0x0a
#define EN0_RCNTHI	0x0b
#define EN0_RSR		0x0c
#define EN0_RXCR	0x0c
#define EN0_TXCR	0x0d
#define EN0_COUNTER0	0x0d
#define EN0_DCFG	0x0e
#define EN0_COUNTER1	0x0e
#define EN0_IMR		0x0f
#define EN0_COUNTER2	0x0f
#define ENISR_RX	0x01
#define ENISR_TX	0x02
#define ENISR_RX_ERR	0x04
#define ENISR_TX_ERR	0x08
#define ENISR_OVER	0x10
#define ENISR_COUNTERS	0x20
#define ENISR_RDC	0x40
#define ENISR_RESET	0x80
#define ENISR_ALL	0x3f
#define ENDCFG_WTS	0x01
#define EN1_PHYS   	0x01
#define EN1_PHYS_SHIFT(i)  (i+1)
#define EN1_CURPAG 	0x07
#define EN1_MULT   	0x08
#define EN1_MULT_SHIFT(i)  (8+i)
#define ENRSR_RXOK	0x01
#define ENRSR_CRC	0x02
#define ENRSR_FAE	0x04
#define ENRSR_FO	0x08
#define ENRSR_MPA	0x10
#define ENRSR_PHY	0x20
#define ENRSR_DIS	0x40
#define ENRSR_DEF	0x80
#define ENTSR_PTX 0x01
#define ENTSR_ND  0x02
#define ENTSR_COL 0x04
#define ENTSR_ABT 0x08
#define ENTSR_CRS 0x10
#define ENTSR_FU  0x20
#define ENTSR_CDH 0x40
#define ENTSR_OWC 0x80
#endif