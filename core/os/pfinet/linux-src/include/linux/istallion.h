#ifndef _ISTALLION_H
#define _ISTALLION_H
#define STL_MAXBRDS 4
#define STL_MAXPANELS 4
#define STL_MAXPORTS 64
#define STL_MAXCHANS (STL_MAXPORTS + 1)
#define STL_MAXDEVS (STL_MAXBRDS * STL_MAXPORTS)
typedef struct {
unsigned long magic;
int portnr;
int panelnr;
int brdnr;
unsigned long state;
int devnr;
int flags;
int baud_base;
int custom_divisor;
int close_delay;
int closing_wait;
int refcount;
int openwaitcnt;
int rc;
int argsize;
void *argp;
long session;
long pgrp;
unsigned int rxmarkmsk;
struct tty_struct *tty;
struct wait_queue *open_wait;
struct wait_queue *close_wait;
struct wait_queue *raw_wait;
struct tq_struct tqhangup;
struct termios normaltermios;
struct termios callouttermios;
asysigs_t asig;
unsigned long addr;
unsigned long rxoffset;
unsigned long txoffset;
unsigned long sigs;
unsigned long pflag;
unsigned int rxsize;
unsigned int txsize;
unsigned char reqbit;
unsigned char portidx;
unsigned char portbit;
} stliport_t;
typedef struct stlibrd {
unsigned long magic;
int brdnr;
int brdtype;
int state;
int nrpanels;
int nrports;
int nrdevs;
unsigned int iobase;
int iosize;
unsigned long memaddr;
void *membase;
int memsize;
int pagesize;
int hostoffset;
int slaveoffset;
int bitsize;
int enabval;
int panels[STL_MAXPANELS];
int panelids[STL_MAXPANELS];
void (*init)(struct stlibrd *brdp);
void (*enable)(struct stlibrd *brdp);
void (*reenable)(struct stlibrd *brdp);
void (*disable)(struct stlibrd *brdp);
char *(*getmemptr)(struct stlibrd *brdp, unsigned long offset, int line);
void (*intr)(struct stlibrd *brdp);
void (*reset)(struct stlibrd *brdp);
stliport_t *ports[STL_MAXPORTS];
} stlibrd_t;
#define STLI_PORTMAGIC 0xe671c7a1
#define STLI_BOARDMAGIC 0x4bc6c825
#endif