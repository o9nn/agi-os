#include <linux/config.h>
#ifndef _PPA_H
#define _PPA_H
#define   PPA_VERSION   "1.42"
#if 0
#ifndef CONFIG_SCSI_PPA_HAVE_PEDANTIC
#define CONFIG_SCSI_PPA_HAVE_PEDANTIC	3
#endif
#endif
#ifdef PPA_CODE
#include  <linux/stddef.h>
#include  <linux/module.h>
#include  <linux/kernel.h>
#include  <linux/tqueue.h>
#include  <linux/ioport.h>
#include  <linux/delay.h>
#include  <linux/proc_fs.h>
#include  <linux/stat.h>
#include  <linux/blk.h>
#include  <linux/sched.h>
#include  <linux/interrupt.h>
#include  <asm/io.h>
#include  "sd.h"
#include  "hosts.h"
#define   PPA_AUTODETECT        0
#define   PPA_NIBBLE            1
#define   PPA_PS2               2
#define   PPA_EPP_8             3
#define   PPA_EPP_16            4
#define   PPA_EPP_32            5
#define   PPA_UNKNOWN           6
static char *PPA_MODE_STRING[] =
{
"Autodetect",
"SPP",
"PS/2",
"EPP 8 bit",
"EPP 16 bit",
"EPP 32 bit",
"Unknown"};
int ppa_sg = SG_ALL;
#define PPA_CAN_QUEUE   1
#define PPA_BURST_SIZE	512
#define PPA_SELECT_TMO  5000
#define PPA_SPIN_TMO    50000
#define PPA_DEBUG	0
#define IN_EPP_MODE(x) (x == PPA_EPP_8 || x == PPA_EPP_16 || x == PPA_EPP_32)
#define CONNECT_EPP_MAYBE 1
#define CONNECT_NORMAL  0
#define r_dtr(x)        (unsigned char)inb((x))
#define r_str(x)        (unsigned char)inb((x)+1)
#define r_ctr(x)        (unsigned char)inb((x)+2)
#define r_epp(x)        (unsigned char)inb((x)+4)
#define r_fifo(x)       (unsigned char)inb((x)+0x400)
#define r_ecr(x)        (unsigned char)inb((x)+0x402)
#define w_dtr(x,y)      outb(y, (x))
#define w_str(x,y)      outb(y, (x)+1)
#define w_ctr(x,y)      outb(y, (x)+2)
#define w_epp(x,y)      outb(y, (x)+4)
#define w_fifo(x,y)     outb(y, (x)+0x400)
#define w_ecr(x,y)      outb(y, (x)+0x402)
static int ppa_engine(ppa_struct *, Scsi_Cmnd *);
static int ppa_in(int, char *, int);
static int ppa_init(int);
static void ppa_interrupt(void *);
static int ppa_out(int, char *, int);
struct proc_dir_entry proc_scsi_ppa =
{PROC_SCSI_PPA, 3, "ppa", S_IFDIR | S_IRUGO | S_IXUGO, 2};
#else
extern struct proc_dir_entry proc_scsi_ppa;
#endif
int ppa_detect(Scsi_Host_Template *);
const char *ppa_info(struct Scsi_Host *);
int ppa_queuecommand(Scsi_Cmnd *, void (*done) (Scsi_Cmnd *));
int ppa_abort(Scsi_Cmnd *);
int ppa_reset(Scsi_Cmnd *, unsigned int);
int ppa_proc_info(char *, char **, off_t, int, int, int);
int ppa_biosparam(Disk *, kdev_t, int *);
#define PPA {	proc_dir:		&proc_scsi_ppa,			\
proc_info:		ppa_proc_info,			\
name:			"Iomega parport ZIP drive",	\
detect:			ppa_detect,			\
queuecommand:		ppa_queuecommand,		\
abort:			ppa_abort,			\
reset:			ppa_reset,			\
bios_param:		ppa_biosparam,			\
this_id:		-1,				\
sg_tablesize:		SG_ALL,				\
cmd_per_lun:		1,				\
use_clustering:		ENABLE_CLUSTERING		\
}
#endif