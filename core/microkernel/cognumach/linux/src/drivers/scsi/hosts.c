#define _SCSI_SYMS_VER_
#define __NO_VERSION__
#include <linux/module.h>
#include <linux/config.h>
#include <linux/blk.h>
#include <linux/kernel.h>
#include <linux/string.h>
#include <linux/mm.h>
#include <linux/proc_fs.h>
#include "scsi.h"
#ifndef NULL
#define NULL 0L
#endif
#define HOSTS_C
#include "hosts.h"
#ifdef CONFIG_A3000_SCSI
#include "a3000.h"
#endif
#ifdef CONFIG_A2091_SCSI
#include "a2091.h"
#endif
#ifdef CONFIG_GVP11_SCSI
#include "gvp11.h"
#endif
#ifdef CONFIG_ATARI_SCSI
#include "atari_scsi.h"
#endif
#ifdef CONFIG_SCSI_ADVANSYS
#include "advansys.h"
#endif
#ifdef CONFIG_SCSI_AHA152X
#include "aha152x.h"
#endif
#ifdef CONFIG_SCSI_AHA1542
#include "aha1542.h"
#endif
#ifdef CONFIG_SCSI_AHA1740
#include "aha1740.h"
#endif
#ifdef CONFIG_SCSI_AIC7XXX
#include "aic7xxx.h"
#endif
#ifdef CONFIG_SCSI_BUSLOGIC
#include "BusLogic.h"
#endif
#ifdef CONFIG_SCSI_EATA_DMA
#include "eata_dma.h"
#endif
#ifdef CONFIG_SCSI_EATA_PIO
#include "eata_pio.h"
#endif
#ifdef CONFIG_SCSI_U14_34F
#include "u14-34f.h"
#endif
#ifdef CONFIG_SCSI_FUTURE_DOMAIN
#include "fdomain.h"
#endif
#ifdef CONFIG_SCSI_GENERIC_NCR5380
#include "g_NCR5380.h"
#endif
#ifdef CONFIG_SCSI_IN2000
#include "in2000.h"
#endif
#ifdef CONFIG_SCSI_PAS16
#include "pas16.h"
#endif
#ifdef CONFIG_SCSI_QLOGIC_FAS
#include "qlogicfas.h"
#endif
#ifdef CONFIG_SCSI_QLOGIC_ISP
#include "qlogicisp.h"
#endif
#ifdef CONFIG_SCSI_SEAGATE
#include "seagate.h"
#endif
#ifdef CONFIG_SCSI_T128
#include "t128.h"
#endif
#ifdef CONFIG_SCSI_DTC3280
#include "dtc.h"
#endif
#ifdef CONFIG_SCSI_NCR53C7xx
#include "53c7,8xx.h"
#endif
#ifdef CONFIG_SCSI_SYM53C8XX
#include "sym53c8xx.h"
#endif
#ifdef CONFIG_SCSI_NCR53C8XX
#include "ncr53c8xx.h"
#endif
#ifdef CONFIG_SCSI_ULTRASTOR
#include "ultrastor.h"
#endif
#ifdef CONFIG_SCSI_7000FASST
#include "wd7000.h"
#endif
#ifdef CONFIG_SCSI_EATA
#include "eata.h"
#endif
#ifdef CONFIG_SCSI_NCR53C406A
#include "NCR53c406a.h"
#endif
#ifdef CONFIG_SCSI_DC390T
#include "dc390.h"
#endif
#ifdef CONFIG_SCSI_AM53C974
#include "AM53C974.h"
#endif
#ifdef CONFIG_SCSI_MEGARAID
#include "megaraid.h"
#endif
#ifdef CONFIG_SCSI_PPA
#include "ppa.h"
#endif
#ifdef CONFIG_SCSI_SUNESP
#include "esp.h"
#endif
#ifdef CONFIG_BLK_DEV_IDESCSI
#include "ide-scsi.h"
#endif
#ifdef CONFIG_SCSI_GDTH
#include "gdth.h"
#endif
#ifdef CONFIG_SCSI_DEBUG
#include "scsi_debug.h"
#endif
#define NO_CONTROLLER {NULL, NULL, NULL, NULL, NULL, NULL, NULL, \
NULL, NULL, 0, 0, 0, 0, 0, 0}
Scsi_Host_Template * scsi_hosts = NULL;
static Scsi_Host_Template builtin_scsi_hosts[] =
{
#ifdef CONFIG_AMIGA
#ifdef CONFIG_A3000_SCSI
A3000_SCSI,
#endif
#ifdef CONFIG_A2091_SCSI
A2091_SCSI,
#endif
#ifdef CONFIG_GVP11_SCSI
GVP11_SCSI,
#endif
#endif
#ifdef CONFIG_ATARI
#ifdef CONFIG_ATARI_SCSI
ATARI_SCSI,
#endif
#endif
#ifdef CONFIG_SCSI_ADVANSYS
ADVANSYS,
#endif
#ifdef CONFIG_SCSI_BUSLOGIC
BUSLOGIC,
#endif
#ifdef CONFIG_SCSI_U14_34F
ULTRASTOR_14_34F,
#endif
#ifdef CONFIG_SCSI_ULTRASTOR
ULTRASTOR_14F,
#endif
#ifdef CONFIG_SCSI_AHA152X
AHA152X,
#endif
#ifdef CONFIG_SCSI_AHA1542
AHA1542,
#endif
#ifdef CONFIG_SCSI_AHA1740
AHA1740,
#endif
#ifdef CONFIG_SCSI_AIC7XXX
AIC7XXX,
#endif
#ifdef CONFIG_SCSI_FUTURE_DOMAIN
FDOMAIN_16X0,
#endif
#ifdef CONFIG_SCSI_IN2000
IN2000,
#endif
#ifdef CONFIG_SCSI_GENERIC_NCR5380
GENERIC_NCR5380,
#endif
#ifdef CONFIG_SCSI_NCR53C406A
NCR53c406a,
#endif
#ifdef CONFIG_SCSI_QLOGIC_FAS
QLOGICFAS,
#endif
#ifdef CONFIG_SCSI_QLOGIC_ISP
QLOGICISP,
#endif
#ifdef CONFIG_SCSI_PAS16
MV_PAS16,
#endif
#ifdef CONFIG_SCSI_SEAGATE
SEAGATE_ST0X,
#endif
#ifdef CONFIG_SCSI_T128
TRANTOR_T128,
#endif
#ifdef CONFIG_SCSI_DTC3280
DTC3x80,
#endif
#ifdef CONFIG_SCSI_DC390T
DC390_T,
#endif
#ifdef CONFIG_SCSI_NCR53C7xx
NCR53c7xx,
#endif
#ifdef CONFIG_SCSI_SYM53C8XX
SYM53C8XX,
#endif
#ifdef CONFIG_SCSI_NCR53C8XX
NCR53C8XX,
#endif
#ifdef CONFIG_SCSI_EATA_DMA
EATA_DMA,
#endif
#ifdef CONFIG_SCSI_EATA_PIO
EATA_PIO,
#endif
#ifdef CONFIG_SCSI_7000FASST
WD7000,
#endif
#ifdef CONFIG_SCSI_EATA
EATA,
#endif
#ifdef CONFIG_SCSI_AM53C974
AM53C974,
#endif
#ifdef CONFIG_SCSI_MEGARAID
MEGARAID,
#endif
#ifdef CONFIG_SCSI_PPA
PPA,
#endif
#ifdef CONFIG_SCSI_SUNESP
SCSI_SPARC_ESP,
#endif
#ifdef CONFIG_SCSI_GDTH
GDTH,
#endif
#ifdef CONFIG_BLK_DEV_IDESCSI
IDESCSI,
#endif
#ifdef CONFIG_SCSI_DEBUG
SCSI_DEBUG,
#endif
};
#define MAX_SCSI_HOSTS (sizeof(builtin_scsi_hosts) / sizeof(Scsi_Host_Template))
struct Scsi_Host * scsi_hostlist = NULL;
struct Scsi_Device_Template * scsi_devicelist = NULL;
int max_scsi_hosts = 0;
int next_scsi_host = 0;
void
scsi_unregister(struct Scsi_Host * sh){
struct Scsi_Host * shpnt;
if(scsi_hostlist == sh)
scsi_hostlist = sh->next;
else {
shpnt = scsi_hostlist;
while(shpnt->next != sh) shpnt = shpnt->next;
shpnt->next = shpnt->next->next;
}
if(sh->host_no == max_scsi_hosts - 1) {
while(--max_scsi_hosts >= next_scsi_host) {
shpnt = scsi_hostlist;
while(shpnt && shpnt->host_no != max_scsi_hosts - 1)
shpnt = shpnt->next;
if(shpnt)
break;
}
}
next_scsi_host--;
scsi_init_free((char *) sh, sizeof(struct Scsi_Host) + sh->extra_bytes);
}
struct Scsi_Host * scsi_register(Scsi_Host_Template * tpnt, int j){
struct Scsi_Host * retval, *shpnt;
retval = (struct Scsi_Host *)scsi_init_malloc(sizeof(struct Scsi_Host) + j,
(tpnt->unchecked_isa_dma && j ? GFP_DMA : 0) | GFP_ATOMIC);
retval->host_busy = 0;
retval->block = NULL;
retval->wish_block = 0;
if(j > 0xffff) panic("Too many extra bytes requested\n");
retval->extra_bytes = j;
retval->loaded_as_module = scsi_loadable_module_flag;
retval->host_no = max_scsi_hosts++;
next_scsi_host++;
retval->host_queue = NULL;
retval->host_wait = NULL;
retval->last_reset = 0;
retval->irq = 0;
retval->dma_channel = 0xff;
retval->max_channel = 0;
retval->max_id = 8;
retval->max_lun = 8;
retval->unique_id = 0;
retval->io_port = 0;
retval->hostt = tpnt;
retval->next = NULL;
#ifdef DEBUG
printk("Register %x %x: %d\n", (int)retval, (int)retval->hostt, j);
#endif
retval->this_id = tpnt->this_id;
retval->can_queue = tpnt->can_queue;
retval->sg_tablesize = tpnt->sg_tablesize;
retval->cmd_per_lun = tpnt->cmd_per_lun;
retval->unchecked_isa_dma = tpnt->unchecked_isa_dma;
retval->use_clustering = tpnt->use_clustering;
retval->select_queue_depths = NULL;
if(!scsi_hostlist)
scsi_hostlist = retval;
else
{
shpnt = scsi_hostlist;
while(shpnt->next) shpnt = shpnt->next;
shpnt->next = retval;
}
return retval;
}
int
scsi_register_device(struct Scsi_Device_Template * sdpnt)
{
if(sdpnt->next) panic("Device already registered");
sdpnt->next = scsi_devicelist;
scsi_devicelist = sdpnt;
return 0;
}
unsigned int scsi_init()
{
static int called = 0;
int i, pcount;
Scsi_Host_Template * tpnt;
struct Scsi_Host * shpnt;
const char * name;
if(called) return 0;
called = 1;
for (tpnt = &builtin_scsi_hosts[0], i = 0; i < MAX_SCSI_HOSTS; ++i, tpnt++)
{
printk("\rprobing scsi %d/%d: %s \e[K", tpnt-builtin_scsi_hosts, MAX_SCSI_HOSTS, tpnt->name);
pcount = next_scsi_host;
if ((tpnt->detect) &&
(tpnt->present =
tpnt->detect(tpnt)))
{
if(pcount == next_scsi_host) {
if(tpnt->present > 1)
panic("Failure to register low-level scsi driver");
scsi_register(tpnt,0);
}
tpnt->next = scsi_hosts;
scsi_hosts = tpnt;
#if CONFIG_PROC_FS
build_proc_dir_entries(tpnt);
#endif
}
}
printk("\ndone\n");
for(shpnt=scsi_hostlist; shpnt; shpnt = shpnt->next)
{
if(shpnt->hostt->info)
name = shpnt->hostt->info(shpnt);
else
name = shpnt->hostt->name;
printk ("scsi%d : %s\n",
shpnt->host_no, name);
}
printk ("scsi : %d host%s.\n", next_scsi_host,
(next_scsi_host == 1) ? "" : "s");
scsi_make_blocked_list();
#ifdef CONFIG_BLK_DEV_SD
scsi_register_device(&sd_template);
#endif
#ifdef CONFIG_BLK_DEV_SR
scsi_register_device(&sr_template);
#endif
#ifdef CONFIG_CHR_DEV_ST
scsi_register_device(&st_template);
#endif
#ifdef CONFIG_CHR_DEV_SG
scsi_register_device(&sg_template);
#endif
#if 0
max_scsi_hosts = next_scsi_host;
#endif
return 0;
}