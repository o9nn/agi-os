#define _SCSI_SYMS_VER_
#include <linux/config.h>
#include <linux/module.h>
#include <linux/sched.h>
#include <linux/timer.h>
#include <linux/string.h>
#include <linux/malloc.h>
#include <linux/ioport.h>
#include <linux/kernel.h>
#include <linux/stat.h>
#include <linux/blk.h>
#include <linux/interrupt.h>
#include <linux/delay.h>
#include <asm/system.h>
#include <asm/irq.h>
#include <asm/dma.h>
#include "scsi.h"
#include "hosts.h"
#include "constants.h"
#ifdef CONFIG_KERNELD
#include <linux/kerneld.h>
#endif
#undef USE_STATIC_SCSI_MEMORY
const unsigned char scsi_command_size[8] = { 6, 10, 10, 12, 12, 12, 10, 10 };
#define INTERNAL_ERROR (panic ("Internal error in file %s, line %d.\n", __FILE__, __LINE__))
#define SECTOR_SIZE 512
#define SECTORS_PER_PAGE (PAGE_SIZE/SECTOR_SIZE)
#if SECTORS_PER_PAGE <= 8
typedef unsigned char FreeSectorBitmap;
#elif SECTORS_PER_PAGE <= 32
typedef unsigned int FreeSectorBitmap;
#else
# error You lose.
#endif
static void scsi_done (Scsi_Cmnd *SCpnt);
static int update_timeout (Scsi_Cmnd *, int);
static void print_inquiry(unsigned char *data);
static void scsi_times_out (Scsi_Cmnd * SCpnt);
static int scan_scsis_single (int channel,int dev,int lun,int * max_scsi_dev ,
int * sparse_lun, Scsi_Device ** SDpnt, Scsi_Cmnd * SCpnt,
struct Scsi_Host *shpnt, char * scsi_result);
void scsi_build_commandblocks(Scsi_Device * SDpnt);
#ifdef CONFIG_MODULES
extern struct symbol_table scsi_symbol_table;
#endif
static FreeSectorBitmap * dma_malloc_freelist = NULL;
static int scsi_need_isa_bounce_buffers;
static unsigned int dma_sectors = 0;
unsigned int dma_free_sectors = 0;
unsigned int need_isa_buffer = 0;
static unsigned char ** dma_malloc_pages = NULL;
static int time_start;
static int time_elapsed;
static volatile struct Scsi_Host * host_active = NULL;
#define SCSI_BLOCK(HOST) ((HOST->block && host_active && HOST != host_active) \
|| (HOST->can_queue && HOST->host_busy >= HOST->can_queue))
const char *const scsi_device_types[MAX_SCSI_DEVICE_CODE] =
{
"Direct-Access    ",
"Sequential-Access",
"Printer          ",
"Processor        ",
"WORM             ",
"CD-ROM           ",
"Scanner          ",
"Optical Device   ",
"Medium Changer   ",
"Communications   "
};
Scsi_Device * scsi_devices = NULL;
unsigned long scsi_pid = 0;
static unsigned long serial_number = 0;
static unsigned char generic_sense[6] = {REQUEST_SENSE, 0,0,0, 255, 0};
static void resize_dma_pool(void);
Scsi_Cmnd * last_cmnd = NULL;
#if CONFIG_PROC_FS
extern int (* dispatch_scsi_info_ptr)(int ino, char *buffer, char **start,
off_t offset, int length, int inout);
extern int dispatch_scsi_info(int ino, char *buffer, char **start,
off_t offset, int length, int inout);
struct proc_dir_entry proc_scsi_scsi = {
PROC_SCSI_SCSI, 4, "scsi",
S_IFREG | S_IRUGO | S_IWUSR, 1, 0, 0, 0,
NULL,
NULL, NULL,
NULL, NULL, NULL
};
#endif
#ifdef DEBUG_TIMEOUT
static void scsi_dump_status(void);
#endif
#ifdef DEBUG
#define SCSI_TIMEOUT (5*HZ)
#else
#define SCSI_TIMEOUT (2*HZ)
#endif
#ifdef DEBUG
#define SENSE_TIMEOUT SCSI_TIMEOUT
#define ABORT_TIMEOUT SCSI_TIMEOUT
#define RESET_TIMEOUT SCSI_TIMEOUT
#else
#define SENSE_TIMEOUT (5*HZ/10)
#define RESET_TIMEOUT (5*HZ/10)
#define ABORT_TIMEOUT (5*HZ/10)
#endif
#define MIN_RESET_DELAY (2*HZ)
#define MIN_RESET_PERIOD (15*HZ)
#define BLIST_NOLUN 0x01
#define BLIST_FORCELUN 0x02
#define BLIST_BORKEN 0x04
#define BLIST_KEY 0x08
#define BLIST_SINGLELUN 0x10
#define BLIST_NOTQ 0x20
#define BLIST_SPARSELUN 0x40
#define BLIST_MAX5LUN 0x80
struct dev_info{
const char * vendor;
const char * model;
const char * revision;
unsigned flags;
};
static struct dev_info device_list[] =
{
{"TEAC","CD-R55S","1.0H", BLIST_NOLUN},
{"CHINON","CD-ROM CDS-431","H42", BLIST_NOLUN},
{"CHINON","CD-ROM CDS-535","Q14", BLIST_NOLUN},
{"DENON","DRD-25X","V", BLIST_NOLUN},
{"HITACHI","DK312C","CM81", BLIST_NOLUN},
{"HITACHI","DK314C","CR21" , BLIST_NOLUN},
{"IMS", "CDD521/10","2.06", BLIST_NOLUN},
{"MAXTOR","XT-3280","PR02", BLIST_NOLUN},
{"MAXTOR","XT-4380S","B3C", BLIST_NOLUN},
{"MAXTOR","MXT-1240S","I1.2", BLIST_NOLUN},
{"MAXTOR","XT-4170S","B5A", BLIST_NOLUN},
{"MAXTOR","XT-8760S","B7B", BLIST_NOLUN},
{"MEDIAVIS","RENO CD-ROMX2A","2.03",BLIST_NOLUN},
{"MICROP", "4110", "*", BLIST_NOTQ},
{"NEC","CD-ROM DRIVE:841","1.0", BLIST_NOLUN},
{"RODIME","RO3000S","2.33", BLIST_NOLUN},
{"SANYO", "CRD-250S", "1.20", BLIST_NOLUN},
{"SEAGATE", "ST157N", "\004|j", BLIST_NOLUN},
{"SEAGATE", "ST296","921", BLIST_NOLUN},
{"SEAGATE","ST1581","6538",BLIST_NOLUN},
{"SONY","CD-ROM CDU-541","4.3d", BLIST_NOLUN},
{"SONY","CD-ROM CDU-55S","1.0i", BLIST_NOLUN},
{"SONY","CD-ROM CDU-561","1.7x", BLIST_NOLUN},
{"TANDBERG","TDC 3600","U07", BLIST_NOLUN},
{"TEAC","CD-ROM","1.06", BLIST_NOLUN},
{"TEXEL","CD-ROM","1.06", BLIST_NOLUN},
{"QUANTUM","LPS525S","3110", BLIST_NOLUN},
{"QUANTUM","PD1225S","3110", BLIST_NOLUN},
{"MEDIAVIS","CDR-H93MV","1.31", BLIST_NOLUN},
{"SANKYO", "CP525","6.64", BLIST_NOLUN},
{"HP", "C1750A", "3226", BLIST_NOLUN},
{"HP", "C1790A", "", BLIST_NOLUN},
{"HP", "C2500A", "", BLIST_NOLUN},
{"SONY","CD-ROM CDU-8001","*", BLIST_BORKEN},
{"TEXEL","CD-ROM","1.06", BLIST_BORKEN},
{"IOMEGA","Io20S         *F","*", BLIST_KEY},
{"INSITE","Floptical   F*8I","*", BLIST_KEY},
{"INSITE","I325VM","*", BLIST_KEY},
{"NRC","MBR-7","*", BLIST_FORCELUN | BLIST_SINGLELUN},
{"NRC","MBR-7.4","*", BLIST_FORCELUN | BLIST_SINGLELUN},
{"REGAL","CDC-4X","*", BLIST_MAX5LUN | BLIST_SINGLELUN},
{"NAKAMICH","MJ-4.8S","*", BLIST_FORCELUN | BLIST_SINGLELUN},
{"NAKAMICH","MJ-5.16S","*", BLIST_FORCELUN | BLIST_SINGLELUN},
{"PIONEER","CD-ROM DRM-600","*", BLIST_FORCELUN | BLIST_SINGLELUN},
{"PIONEER","CD-ROM DRM-602X","*", BLIST_FORCELUN | BLIST_SINGLELUN},
{"PIONEER","CD-ROM DRM-604X","*", BLIST_FORCELUN | BLIST_SINGLELUN},
{"EMULEX","MD21/S2     ESDI","*", BLIST_SINGLELUN},
{"CANON","IPUBJD","*", BLIST_SPARSELUN},
{"MATSHITA","PD","*", BLIST_FORCELUN | BLIST_SINGLELUN},
{"YAMAHA","CDR100","1.00", BLIST_NOLUN},
{"YAMAHA","CDR102","1.00", BLIST_NOLUN},
{"nCipher","Fastness Crypto","*", BLIST_FORCELUN},
{NULL, NULL, NULL}
};
static int get_device_flags(unsigned char * response_data){
int i = 0;
unsigned char * pnt;
for(i=0; 1; i++){
if(device_list[i].vendor == NULL) return 0;
pnt = &response_data[8];
while(*pnt && *pnt == ' ') pnt++;
if(memcmp(device_list[i].vendor, pnt,
strlen(device_list[i].vendor))) continue;
pnt = &response_data[16];
while(*pnt && *pnt == ' ') pnt++;
if(memcmp(device_list[i].model, pnt,
strlen(device_list[i].model))) continue;
return device_list[i].flags;
}
return 0;
}
void scsi_make_blocked_list(void) {
int block_count = 0, index;
unsigned long flags;
struct Scsi_Host * sh[128], * shpnt;
save_flags(flags);
cli();
host_active = NULL;
for(shpnt=scsi_hostlist; shpnt; shpnt = shpnt->next) {
#if 0
if (shpnt->unchecked_isa_dma) shpnt->wish_block = 1;
#endif
if (shpnt->wish_block) sh[block_count++] = shpnt;
}
if (block_count == 1) sh[0]->block = NULL;
else if (block_count > 1) {
for(index = 0; index < block_count - 1; index++) {
sh[index]->block = sh[index + 1];
printk("scsi%d : added to blocked host list.\n",
sh[index]->host_no);
}
sh[block_count - 1]->block = sh[0];
printk("scsi%d : added to blocked host list.\n",
sh[index]->host_no);
}
restore_flags(flags);
}
static void scan_scsis_done (Scsi_Cmnd * SCpnt)
{
#ifdef DEBUG
printk ("scan_scsis_done(%p, %06x)\n", SCpnt->host, SCpnt->result);
#endif
SCpnt->request.rq_status = RQ_SCSI_DONE;
if (SCpnt->request.sem != NULL)
up(SCpnt->request.sem);
}
#ifdef CONFIG_SCSI_MULTI_LUN
static int max_scsi_luns = 8;
#else
static int max_scsi_luns = 1;
#endif
void scsi_luns_setup(char *str, int *ints) {
if (ints[0] != 1)
printk("scsi_luns_setup : usage max_scsi_luns=n (n should be between 1 and 8)\n");
else
max_scsi_luns = ints[1];
}
static void scan_scsis (struct Scsi_Host *shpnt, unchar hardcoded,
unchar hchannel, unchar hid, unchar hlun)
{
int dev, lun, channel;
unsigned char scsi_result0[256];
unsigned char *scsi_result;
Scsi_Device *SDpnt;
int max_dev_lun, sparse_lun;
Scsi_Cmnd *SCpnt;
SCpnt = (Scsi_Cmnd *) scsi_init_malloc (sizeof (Scsi_Cmnd), GFP_ATOMIC | GFP_DMA);
SDpnt = (Scsi_Device *) scsi_init_malloc (sizeof (Scsi_Device), GFP_ATOMIC);
memset (SCpnt, 0, sizeof (Scsi_Cmnd));
scsi_result = ( ( !shpnt->unchecked_isa_dma )
? &scsi_result0[0] : scsi_init_malloc (512, GFP_DMA));
if (scsi_result == NULL) {
printk ("Unable to obtain scsi_result buffer\n");
goto leave;
}
if(shpnt->host_queue)
shpnt->host_queue->prev = SCpnt;
SCpnt->next = shpnt->host_queue;
SCpnt->prev = NULL;
shpnt->host_queue = SCpnt;
if (hardcoded == 1) {
Scsi_Device *oldSDpnt=SDpnt;
struct Scsi_Device_Template * sdtpnt;
channel = hchannel;
if(channel > shpnt->max_channel) goto leave;
dev = hid;
if(dev >= shpnt->max_id) goto leave;
lun = hlun;
if(lun >= shpnt->max_lun) goto leave;
scan_scsis_single (channel, dev, lun, &max_dev_lun, &sparse_lun,
&SDpnt, SCpnt, shpnt, scsi_result);
if(SDpnt!=oldSDpnt) {
for(sdtpnt = scsi_devicelist; sdtpnt; sdtpnt = sdtpnt->next)
if(sdtpnt->init && sdtpnt->dev_noticed) (*sdtpnt->init)();
oldSDpnt->scsi_request_fn = NULL;
for(sdtpnt = scsi_devicelist; sdtpnt; sdtpnt = sdtpnt->next)
if(sdtpnt->attach) {
(*sdtpnt->attach)(oldSDpnt);
if(oldSDpnt->attached) scsi_build_commandblocks(oldSDpnt);}
resize_dma_pool();
for(sdtpnt = scsi_devicelist; sdtpnt; sdtpnt = sdtpnt->next) {
if(sdtpnt->finish && sdtpnt->nr_dev)
{(*sdtpnt->finish)();}
}
}
}
else {
for (channel = 0; channel <= shpnt->max_channel; channel++) {
for (dev = 0; dev < shpnt->max_id; ++dev) {
if (shpnt->this_id != dev) {
max_dev_lun = (max_scsi_luns < shpnt->max_lun ?
max_scsi_luns : shpnt->max_lun);
sparse_lun = 0;
for (lun = 0; lun < max_dev_lun; ++lun) {
if (!scan_scsis_single (channel, dev, lun, &max_dev_lun,
&sparse_lun, &SDpnt, SCpnt, shpnt,
scsi_result)
&& !sparse_lun)
break;
}
}
}
}
}
leave:
{
Scsi_Cmnd *prev, *next, *hqptr;
for(hqptr = shpnt->host_queue; hqptr != SCpnt; hqptr = hqptr->next) ;
if(hqptr) {
prev = hqptr->prev;
next = hqptr->next;
if(prev)
prev->next = next;
else
shpnt->host_queue = next;
if(next) next->prev = prev;
}
}
if (SDpnt != NULL)
scsi_init_free ((char *) SDpnt, sizeof (Scsi_Device));
if (SCpnt != NULL)
scsi_init_free ((char *) SCpnt, sizeof (Scsi_Cmnd));
if (scsi_result != &scsi_result0[0] && scsi_result != NULL)
scsi_init_free (scsi_result, 512);
}
int scan_scsis_single (int channel, int dev, int lun, int *max_dev_lun,
int *sparse_lun, Scsi_Device **SDpnt2, Scsi_Cmnd * SCpnt,
struct Scsi_Host * shpnt, char *scsi_result)
{
unsigned char scsi_cmd[12];
struct Scsi_Device_Template *sdtpnt;
Scsi_Device * SDtail, *SDpnt=*SDpnt2;
int bflags, type=-1;
SDtail = scsi_devices;
if (scsi_devices)
while (SDtail->next)
SDtail = SDtail->next;
memset (SDpnt, 0, sizeof (Scsi_Device));
SDpnt->host = shpnt;
SDpnt->id = dev;
SDpnt->lun = lun;
SDpnt->channel = channel;
SDpnt->type = -1;
SDpnt->borken = 1;
SDpnt->was_reset = 0;
SDpnt->expecting_cc_ua = 0;
scsi_cmd[0] = TEST_UNIT_READY;
scsi_cmd[1] = lun << 5;
scsi_cmd[2] = scsi_cmd[3] = scsi_cmd[4] = scsi_cmd[5] = 0;
SCpnt->host = SDpnt->host;
SCpnt->device = SDpnt;
SCpnt->target = SDpnt->id;
SCpnt->lun = SDpnt->lun;
SCpnt->channel = SDpnt->channel;
{
struct semaphore sem = MUTEX_LOCKED;
SCpnt->request.sem = &sem;
SCpnt->request.rq_status = RQ_SCSI_BUSY;
scsi_do_cmd (SCpnt, (void *) scsi_cmd,
(void *) scsi_result,
256, scan_scsis_done, SCSI_TIMEOUT + 4 * HZ, 5);
down (&sem);
}
#if defined(DEBUG) || defined(DEBUG_INIT)
printk ("scsi: scan_scsis_single id %d lun %d. Return code 0x%08x\n",
dev, lun, SCpnt->result);
print_driverbyte(SCpnt->result); print_hostbyte(SCpnt->result);
printk("\n");
#endif
if (SCpnt->result) {
if (((driver_byte (SCpnt->result) & DRIVER_SENSE) ||
(status_byte (SCpnt->result) & CHECK_CONDITION)) &&
((SCpnt->sense_buffer[0] & 0x70) >> 4) == 7) {
if (((SCpnt->sense_buffer[2] & 0xf) != NOT_READY) &&
((SCpnt->sense_buffer[2] & 0xf) != UNIT_ATTENTION) &&
((SCpnt->sense_buffer[2] & 0xf) != ILLEGAL_REQUEST || lun > 0))
return 1;
}
else
return 0;
}
#if defined (DEBUG) || defined(DEBUG_INIT)
printk ("scsi: performing INQUIRY\n");
#endif
scsi_cmd[0] = INQUIRY;
scsi_cmd[1] = (lun << 5) & 0xe0;
scsi_cmd[2] = 0;
scsi_cmd[3] = 0;
scsi_cmd[4] = 255;
scsi_cmd[5] = 0;
SCpnt->cmd_len = 0;
{
struct semaphore sem = MUTEX_LOCKED;
SCpnt->request.sem = &sem;
SCpnt->request.rq_status = RQ_SCSI_BUSY;
scsi_do_cmd (SCpnt, (void *) scsi_cmd,
(void *) scsi_result,
256, scan_scsis_done, SCSI_TIMEOUT, 3);
down (&sem);
}
#if defined(DEBUG) || defined(DEBUG_INIT)
printk ("scsi: INQUIRY %s with code 0x%x\n",
SCpnt->result ? "failed" : "successful", SCpnt->result);
#endif
if (SCpnt->result)
return 0;
if( (scsi_result[0] >> 5) == 3 )
{
return 0;
}
if (!strncmp (scsi_result + 8, "TOSHIBA", 7) &&
!strncmp (scsi_result + 16, "CD-ROM", 6) &&
scsi_result[0] == TYPE_DISK) {
scsi_result[0] = TYPE_ROM;
scsi_result[1] |= 0x80;
}
if (!strncmp (scsi_result + 8, "NEC", 3)) {
if (!strncmp (scsi_result + 16, "CD-ROM DRIVE:84 ", 16) ||
!strncmp (scsi_result + 16, "CD-ROM DRIVE:25", 15))
SDpnt->manufacturer = SCSI_MAN_NEC_OLDCDR;
else
SDpnt->manufacturer = SCSI_MAN_NEC;
}
else if (!strncmp (scsi_result + 8, "TOSHIBA", 7))
SDpnt->manufacturer = SCSI_MAN_TOSHIBA;
else if (!strncmp (scsi_result + 8, "SONY", 4))
SDpnt->manufacturer = SCSI_MAN_SONY;
else if (!strncmp (scsi_result + 8, "PIONEER", 7))
SDpnt->manufacturer = SCSI_MAN_PIONEER;
else
SDpnt->manufacturer = SCSI_MAN_UNKNOWN;
memcpy (SDpnt->vendor, scsi_result + 8, 8);
memcpy (SDpnt->model, scsi_result + 16, 16);
memcpy (SDpnt->rev, scsi_result + 32, 4);
SDpnt->removable = (0x80 & scsi_result[1]) >> 7;
SDpnt->lockable = SDpnt->removable;
SDpnt->changed = 0;
SDpnt->access_count = 0;
SDpnt->busy = 0;
SDpnt->has_cmdblocks = 0;
switch (type = (scsi_result[0] & 0x1f)) {
case TYPE_TAPE:
case TYPE_DISK:
case TYPE_MOD:
case TYPE_PROCESSOR:
case TYPE_SCANNER:
case TYPE_MEDIUM_CHANGER:
SDpnt->writeable = 1;
break;
case TYPE_WORM:
case TYPE_ROM:
SDpnt->writeable = 0;
break;
default:
printk ("scsi: unknown type %d\n", type);
}
SDpnt->single_lun = 0;
SDpnt->soft_reset =
(scsi_result[7] & 1) && ((scsi_result[3] & 7) == 2);
SDpnt->random = (type == TYPE_TAPE) ? 0 : 1;
SDpnt->type = (type & 0x1f);
print_inquiry (scsi_result);
for (sdtpnt = scsi_devicelist; sdtpnt;
sdtpnt = sdtpnt->next)
if (sdtpnt->detect)
SDpnt->attached +=
(*sdtpnt->detect) (SDpnt);
SDpnt->scsi_level = scsi_result[2] & 0x07;
if (SDpnt->scsi_level >= 2 ||
(SDpnt->scsi_level == 1 &&
(scsi_result[3] & 0x0f) == 1))
SDpnt->scsi_level++;
SDpnt->disconnect = 0;
bflags = get_device_flags (scsi_result);
SDpnt->tagged_queue = 0;
if ((SDpnt->scsi_level >= SCSI_2) &&
(scsi_result[7] & 2) &&
!(bflags & BLIST_NOTQ)) {
SDpnt->tagged_supported = 1;
SDpnt->current_tag = 0;
}
if ((bflags & BLIST_BORKEN) == 0)
SDpnt->borken = 0;
if (bflags & BLIST_SINGLELUN)
SDpnt->single_lun = 1;
if ((bflags & BLIST_KEY) != 0) {
printk ("Unlocked floptical drive.\n");
SDpnt->lockable = 0;
scsi_cmd[0] = MODE_SENSE;
scsi_cmd[1] = (lun << 5) & 0xe0;
scsi_cmd[2] = 0x2e;
scsi_cmd[3] = 0;
scsi_cmd[4] = 0x2a;
scsi_cmd[5] = 0;
SCpnt->cmd_len = 0;
{
struct semaphore sem = MUTEX_LOCKED;
SCpnt->request.rq_status = RQ_SCSI_BUSY;
SCpnt->request.sem = &sem;
scsi_do_cmd (SCpnt, (void *) scsi_cmd,
(void *) scsi_result, 0x2a,
scan_scsis_done, SCSI_TIMEOUT, 3);
down (&sem);
}
}
if (SDtail)
SDtail->next = SDpnt;
else
scsi_devices = SDpnt;
SDtail = SDpnt;
SDpnt = (Scsi_Device *) scsi_init_malloc (sizeof (Scsi_Device), GFP_ATOMIC);
*SDpnt2=SDpnt;
if (!SDpnt)
printk ("scsi: scan_scsis_single: Cannot malloc\n");
if (bflags & BLIST_NOLUN)
return 0;
if (bflags & BLIST_SPARSELUN) {
*max_dev_lun = 8;
*sparse_lun = 1;
return 1;
}
if (bflags & BLIST_FORCELUN) {
*max_dev_lun = 8;
return 1;
}
if (bflags & BLIST_MAX5LUN) {
*max_dev_lun = 5;
return 1;
}
if (((scsi_result[2] & 0x07) == 0)
||
((scsi_result[2] & 0x07) == 1 &&
(scsi_result[3] & 0x0f) == 0))
return 0;
return 1;
}
#define NORMAL_TIMEOUT 0
#define IN_ABORT 1
#define IN_RESET 2
#define IN_RESET2 4
#define IN_RESET3 8
static void scsi_times_out (Scsi_Cmnd * SCpnt)
{
switch (SCpnt->internal_timeout & (IN_ABORT | IN_RESET | IN_RESET2 | IN_RESET3))
{
case NORMAL_TIMEOUT:
{
#ifdef DEBUG_TIMEOUT
scsi_dump_status();
#endif
}
if (!scsi_abort (SCpnt, DID_TIME_OUT))
return;
case IN_ABORT:
printk("SCSI host %d abort (pid %ld) timed out - resetting\n",
SCpnt->host->host_no, SCpnt->pid);
if (!scsi_reset (SCpnt, SCSI_RESET_ASYNCHRONOUS))
return;
case IN_RESET:
case (IN_ABORT | IN_RESET):
printk("SCSI host %d channel %d reset (pid %ld) timed out - "
"trying harder\n",
SCpnt->host->host_no, SCpnt->channel, SCpnt->pid);
SCpnt->internal_timeout &= ~IN_RESET;
SCpnt->internal_timeout |= IN_RESET2;
scsi_reset (SCpnt,
SCSI_RESET_ASYNCHRONOUS | SCSI_RESET_SUGGEST_BUS_RESET);
return;
case IN_RESET2:
case (IN_ABORT | IN_RESET2):
printk("SCSI host %d reset (pid %ld) timed out - trying to shake it loose\n",
SCpnt->host->host_no, SCpnt->pid);
SCpnt->internal_timeout &= ~(IN_RESET | IN_RESET2);
SCpnt->internal_timeout |= IN_RESET3;
scsi_reset (SCpnt,
SCSI_RESET_ASYNCHRONOUS | SCSI_RESET_SUGGEST_HOST_RESET);
return;
default:
printk("SCSI host %d reset (pid %ld) timed out again -\n",
SCpnt->host->host_no, SCpnt->pid);
printk("probably an unrecoverable SCSI bus or device hang.\n");
return;
}
}
Scsi_Cmnd * request_queueable (struct request * req, Scsi_Device * device)
{
Scsi_Cmnd * SCpnt = NULL;
int tablesize;
Scsi_Cmnd * found = NULL;
struct buffer_head * bh, *bhp;
if (!device)
panic ("No device passed to request_queueable().\n");
if (req && req->rq_status == RQ_INACTIVE)
panic("Inactive in request_queueable");
if (!device->single_lun) {
SCpnt = device->device_queue;
while(SCpnt){
if(SCpnt->request.rq_status == RQ_INACTIVE) break;
SCpnt = SCpnt->device_next;
}
} else {
SCpnt = device->host->host_queue;
while(SCpnt){
if(SCpnt->channel == device->channel
&& SCpnt->target == device->id) {
if (SCpnt->lun == device->lun) {
if(found == NULL
&& SCpnt->request.rq_status == RQ_INACTIVE)
{
found=SCpnt;
}
}
if(SCpnt->request.rq_status != RQ_INACTIVE) {
return NULL;
}
}
SCpnt = SCpnt->next;
}
SCpnt = found;
}
if (!SCpnt) return NULL;
if (SCSI_BLOCK(device->host)) return NULL;
if (req) {
memcpy(&SCpnt->request, req, sizeof(struct request));
tablesize = device->host->sg_tablesize;
bhp = bh = req->bh;
if(!tablesize) bh = NULL;
while(req->nr_sectors && bh){
bhp = bhp->b_reqnext;
if(!bhp || !CONTIGUOUS_BUFFERS(bh,bhp)) tablesize--;
req->nr_sectors -= bh->b_size >> 9;
req->sector += bh->b_size >> 9;
if(!tablesize) break;
bh = bhp;
}
if(req->nr_sectors && bh && bh->b_reqnext){
SCpnt->request.bhtail = bh;
req->bh = bh->b_reqnext;
bh->b_reqnext = NULL;
bh = req->bh;
SCpnt->request.nr_sectors -= req->nr_sectors;
req->current_nr_sectors = bh->b_size >> 9;
req->buffer = bh->b_data;
SCpnt->request.sem = NULL;
} else {
req->rq_status = RQ_INACTIVE;
wake_up(&wait_for_request);
}
} else {
SCpnt->request.rq_status = RQ_SCSI_BUSY;
SCpnt->request.sem = NULL;
}
SCpnt->use_sg = 0;
SCpnt->old_use_sg = 0;
SCpnt->transfersize = 0;
SCpnt->underflow = 0;
SCpnt->cmd_len = 0;
SCpnt->channel = device->channel;
SCpnt->lun = device->lun;
SCpnt->target = device->id;
return SCpnt;
}
Scsi_Cmnd * allocate_device (struct request ** reqp, Scsi_Device * device,
int wait)
{
kdev_t dev;
struct request * req = NULL;
int tablesize;
unsigned long flags;
struct buffer_head * bh, *bhp;
struct Scsi_Host * host;
Scsi_Cmnd * SCpnt = NULL;
Scsi_Cmnd * SCwait = NULL;
Scsi_Cmnd * found = NULL;
if (!device)
panic ("No device passed to allocate_device().\n");
if (reqp) req = *reqp;
if (req) {
if(req->rq_status == RQ_INACTIVE) return NULL;
dev = req->rq_dev;
} else
dev = 0;
host = device->host;
if (intr_count && SCSI_BLOCK(host)) return NULL;
while (1==1){
if (!device->single_lun) {
SCpnt = device->device_queue;
while(SCpnt){
SCwait = SCpnt;
if(SCpnt->request.rq_status == RQ_INACTIVE) break;
SCpnt = SCpnt->device_next;
}
} else {
SCpnt = device->host->host_queue;
while(SCpnt){
if(SCpnt->channel == device->channel
&& SCpnt->target == device->id) {
if (SCpnt->lun == device->lun) {
SCwait = SCpnt;
if(found == NULL
&& SCpnt->request.rq_status == RQ_INACTIVE)
{
found=SCpnt;
}
}
if(SCpnt->request.rq_status != RQ_INACTIVE) {
found = NULL;
break;
}
}
SCpnt = SCpnt->next;
}
SCpnt = found;
}
save_flags(flags);
cli();
if (req && (req->rq_status == RQ_INACTIVE || req->rq_dev != dev)) {
restore_flags(flags);
return NULL;
}
if (!SCpnt || SCpnt->request.rq_status != RQ_INACTIVE)
{
#if 1
if (wait && SCwait && SCwait->request.rq_status != RQ_INACTIVE){
sleep_on(&device->device_wait);
restore_flags(flags);
} else {
restore_flags(flags);
if (!wait) return NULL;
if (!SCwait) {
printk("Attempt to allocate device channel %d,"
" target %d, lun %d\n", device->channel,
device->id, device->lun);
panic("No device found in allocate_device\n");
}
}
#else
restore_flags(flags);
if(!wait) return NULL;
if (!SCwait) {
printk("Attempt to allocate device channel %d, target"
" %d, lun %d\n", device->channel, device->id,
device->lun);
panic("No device found in allocate_device\n");
}
SCSI_SLEEP(&device->device_wait,
(SCwait->request.rq_status != RQ_INACTIVE));
#endif
} else {
if (req) {
memcpy(&SCpnt->request, req, sizeof(struct request));
tablesize = device->host->sg_tablesize;
bhp = bh = req->bh;
if(!tablesize) bh = NULL;
while(req->nr_sectors && bh){
bhp = bhp->b_reqnext;
if(!bhp || !CONTIGUOUS_BUFFERS(bh,bhp)) tablesize--;
req->nr_sectors -= bh->b_size >> 9;
req->sector += bh->b_size >> 9;
if(!tablesize) break;
bh = bhp;
}
if(req->nr_sectors && bh && bh->b_reqnext){
SCpnt->request.bhtail = bh;
req->bh = bh->b_reqnext;
bh->b_reqnext = NULL;
bh = req->bh;
SCpnt->request.nr_sectors -= req->nr_sectors;
req->current_nr_sectors = bh->b_size >> 9;
req->buffer = bh->b_data;
SCpnt->request.sem = NULL;
}
else
{
req->rq_status = RQ_INACTIVE;
*reqp = req->next;
wake_up(&wait_for_request);
}
} else {
SCpnt->request.rq_status = RQ_SCSI_BUSY;
SCpnt->request.sem = NULL;
}
restore_flags(flags);
break;
}
}
SCpnt->use_sg = 0;
SCpnt->old_use_sg = 0;
SCpnt->transfersize = 0;
SCpnt->cmd_len = 0;
SCpnt->underflow = 0;
SCpnt->channel = device->channel;
SCpnt->lun = device->lun;
SCpnt->target = device->id;
return SCpnt;
}
inline void internal_cmnd (Scsi_Cmnd * SCpnt)
{
unsigned long flags, timeout;
struct Scsi_Host * host;
#ifdef DEBUG_DELAY
unsigned long clock;
#endif
#if DEBUG
unsigned long *ret = 0;
#ifdef __mips__
__asm__ __volatile__ ("move\t%0,$31":"=r"(ret));
#else
ret = __builtin_return_address(0);
#endif
#endif
host = SCpnt->host;
save_flags(flags);
cli();
if (++serial_number == 0) serial_number = 1;
SCpnt->serial_number = serial_number;
timeout = host->last_reset + MIN_RESET_DELAY;
if (jiffies < timeout) {
int ticks_remaining = timeout - jiffies;
sti();
while (--ticks_remaining >= 0) udelay(1000000/HZ);
host->last_reset = jiffies - MIN_RESET_DELAY;
}
restore_flags(flags);
update_timeout(SCpnt, SCpnt->timeout_per_command);
#ifdef DEBUG
printk("internal_cmnd (host = %d, channel = %d, target = %d, "
"command = %p, buffer = %p, \nbufflen = %d, done = %p)\n",
SCpnt->host->host_no, SCpnt->channel, SCpnt->target, SCpnt->cmnd,
SCpnt->buffer, SCpnt->bufflen, SCpnt->done);
#endif
if (host->can_queue)
{
#ifdef DEBUG
printk("queuecommand : routine at %p\n",
host->hostt->queuecommand);
#endif
if(!intr_count && SCpnt->host->irq)
disable_irq(SCpnt->host->irq);
host->hostt->queuecommand (SCpnt, scsi_done);
if(!intr_count && SCpnt->host->irq)
enable_irq(SCpnt->host->irq);
}
else
{
int temp;
#ifdef DEBUG
printk("command() :  routine at %p\n", host->hostt->command);
#endif
temp = host->hostt->command (SCpnt);
SCpnt->result = temp;
#ifdef DEBUG_DELAY
clock = jiffies + 4 * HZ;
while (jiffies < clock) barrier();
printk("done(host = %d, result = %04x) : routine at %p\n",
host->host_no, temp, host->hostt->command);
#endif
scsi_done(SCpnt);
}
#ifdef DEBUG
printk("leaving internal_cmnd()\n");
#endif
}
static void scsi_request_sense (Scsi_Cmnd * SCpnt)
{
unsigned long flags;
save_flags(flags);
cli();
SCpnt->flags |= WAS_SENSE | ASKED_FOR_SENSE;
update_timeout(SCpnt, SENSE_TIMEOUT);
restore_flags(flags);
memcpy ((void *) SCpnt->cmnd , (void *) generic_sense,
sizeof(generic_sense));
SCpnt->cmnd[1] = SCpnt->lun << 5;
SCpnt->cmnd[4] = sizeof(SCpnt->sense_buffer);
SCpnt->request_buffer = &SCpnt->sense_buffer;
SCpnt->request_bufflen = sizeof(SCpnt->sense_buffer);
SCpnt->use_sg = 0;
SCpnt->cmd_len = COMMAND_SIZE(SCpnt->cmnd[0]);
internal_cmnd (SCpnt);
}
void scsi_do_cmd (Scsi_Cmnd * SCpnt, const void *cmnd ,
void *buffer, unsigned bufflen, void (*done)(Scsi_Cmnd *),
int timeout, int retries)
{
unsigned long flags;
struct Scsi_Host * host = SCpnt->host;
#ifdef DEBUG
{
int i;
int target = SCpnt->target;
printk ("scsi_do_cmd (host = %d, channel = %d target = %d, "
"buffer =%p, bufflen = %d, done = %p, timeout = %d, "
"retries = %d)\n"
"command : " , host->host_no, SCpnt->channel, target, buffer,
bufflen, done, timeout, retries);
for (i = 0; i < 10; ++i)
printk ("%02x  ", ((unsigned char *) cmnd)[i]);
printk("\n");
}
#endif
if (!host)
{
panic ("Invalid or not present host.\n");
}
save_flags(flags);
cli();
SCpnt->pid = scsi_pid++;
while (SCSI_BLOCK(host)) {
restore_flags(flags);
SCSI_SLEEP(&host->host_wait, SCSI_BLOCK(host));
cli();
}
if (host->block) host_active = host;
host->host_busy++;
restore_flags(flags);
memcpy ((void *) SCpnt->data_cmnd , (const void *) cmnd, 12);
#if 0
SCpnt->host = host;
SCpnt->channel = channel;
SCpnt->target = target;
SCpnt->lun = (SCpnt->data_cmnd[1] >> 5);
#endif
SCpnt->reset_chain = NULL;
SCpnt->serial_number = 0;
SCpnt->bufflen = bufflen;
SCpnt->buffer = buffer;
SCpnt->flags = 0;
SCpnt->retries = 0;
SCpnt->allowed = retries;
SCpnt->done = done;
SCpnt->timeout_per_command = timeout;
memcpy ((void *) SCpnt->cmnd , (const void *) cmnd, 12);
memset ((void *) SCpnt->sense_buffer, 0, sizeof SCpnt->sense_buffer);
SCpnt->request_buffer = buffer;
SCpnt->request_bufflen = bufflen;
SCpnt->old_use_sg = SCpnt->use_sg;
if (SCpnt->cmd_len == 0)
SCpnt->cmd_len = COMMAND_SIZE(SCpnt->cmnd[0]);
SCpnt->old_cmd_len = SCpnt->cmd_len;
SCpnt->internal_timeout = NORMAL_TIMEOUT;
SCpnt->abort_reason = 0;
internal_cmnd (SCpnt);
#ifdef DEBUG
printk ("Leaving scsi_do_cmd()\n");
#endif
}
static int check_sense (Scsi_Cmnd * SCpnt)
{
if (((SCpnt->sense_buffer[0] & 0x70) >> 4) != 7) {
if(!(SCpnt->flags & ASKED_FOR_SENSE))
return SUGGEST_SENSE;
else
return SUGGEST_RETRY;
}
SCpnt->flags &= ~ASKED_FOR_SENSE;
#ifdef DEBUG_INIT
printk("scsi%d, channel%d : ", SCpnt->host->host_no, SCpnt->channel);
print_sense("", SCpnt);
printk("\n");
#endif
if (SCpnt->sense_buffer[2] & 0xe0)
return SUGGEST_ABORT;
switch (SCpnt->sense_buffer[2] & 0xf)
{
case NO_SENSE:
return 0;
case RECOVERED_ERROR:
return SUGGEST_IS_OK;
case ABORTED_COMMAND:
return SUGGEST_RETRY;
case NOT_READY:
case UNIT_ATTENTION:
if( SCpnt->device->expecting_cc_ua )
{
SCpnt->device->expecting_cc_ua = 0;
return SUGGEST_RETRY;
}
return SUGGEST_ABORT;
case COPY_ABORTED:
case VOLUME_OVERFLOW:
case MISCOMPARE:
case MEDIUM_ERROR:
return SUGGEST_REMAP;
case BLANK_CHECK:
case DATA_PROTECT:
case HARDWARE_ERROR:
case ILLEGAL_REQUEST:
default:
return SUGGEST_ABORT;
}
}
static void scsi_done (Scsi_Cmnd * SCpnt)
{
int status=0;
int exit=0;
int checked;
int oldto;
struct Scsi_Host * host = SCpnt->host;
int result = SCpnt->result;
SCpnt->serial_number = 0;
oldto = update_timeout(SCpnt, 0);
#ifdef DEBUG_TIMEOUT
if(result) printk("Non-zero result in scsi_done %x %d:%d\n",
result, SCpnt->target, SCpnt->lun);
#endif
if(host_byte(result) == DID_ABORT && SCpnt->abort_reason)
SCpnt->result = result = (result & 0xff00ffff) |
(SCpnt->abort_reason << 16);
#define FINISHED 0
#define MAYREDO 1
#define REDO 3
#define PENDING 4
#ifdef DEBUG
printk("In scsi_done(host = %d, result = %06x)\n", host->host_no, result);
#endif
if(SCpnt->flags & WAS_SENSE)
{
SCpnt->use_sg = SCpnt->old_use_sg;
SCpnt->cmd_len = SCpnt->old_cmd_len;
}
switch (host_byte(result))
{
case DID_OK:
if (status_byte(result) && (SCpnt->flags & WAS_SENSE))
{
SCpnt->flags &= ~WAS_SENSE;
#if 0
SCpnt->internal_timeout &= ~SENSE_TIMEOUT;
#endif
if (!(SCpnt->flags & WAS_RESET))
{
printk("scsi%d : channel %d target %d lun %d request sense"
" failed, performing reset.\n",
SCpnt->host->host_no, SCpnt->channel, SCpnt->target,
SCpnt->lun);
scsi_reset(SCpnt, SCSI_RESET_SYNCHRONOUS);
return;
}
else
{
exit = (DRIVER_HARD | SUGGEST_ABORT);
status = FINISHED;
}
}
else switch(msg_byte(result))
{
case COMMAND_COMPLETE:
switch (status_byte(result))
{
case GOOD:
if (SCpnt->flags & WAS_SENSE)
{
#ifdef DEBUG
printk ("In scsi_done, GOOD status, COMMAND COMPLETE, "
"parsing sense information.\n");
#endif
SCpnt->flags &= ~WAS_SENSE;
#if 0
SCpnt->internal_timeout &= ~SENSE_TIMEOUT;
#endif
switch (checked = check_sense(SCpnt))
{
case SUGGEST_SENSE:
case 0:
#ifdef DEBUG
printk("NO SENSE.  status = REDO\n");
#endif
update_timeout(SCpnt, oldto);
status = REDO;
break;
case SUGGEST_IS_OK:
break;
case SUGGEST_REMAP:
#ifdef DEBUG
printk("SENSE SUGGEST REMAP - status = FINISHED\n");
#endif
status = FINISHED;
exit = DRIVER_SENSE | SUGGEST_ABORT;
break;
case SUGGEST_RETRY:
#ifdef DEBUG
printk("SENSE SUGGEST RETRY - status = MAYREDO\n");
#endif
status = MAYREDO;
exit = DRIVER_SENSE | SUGGEST_RETRY;
break;
case SUGGEST_ABORT:
#ifdef DEBUG
printk("SENSE SUGGEST ABORT - status = FINISHED");
#endif
status = FINISHED;
exit = DRIVER_SENSE | SUGGEST_ABORT;
break;
default:
printk ("Internal error %s %d \n", __FILE__,
__LINE__);
}
}
else
{
#ifdef DEBUG
printk("COMMAND COMPLETE message returned, "
"status = FINISHED. \n");
#endif
exit = DRIVER_OK;
status = FINISHED;
}
break;
case CHECK_CONDITION:
case COMMAND_TERMINATED:
switch (check_sense(SCpnt))
{
case 0:
update_timeout(SCpnt, oldto);
status = REDO;
break;
case SUGGEST_REMAP:
status = FINISHED;
exit = DRIVER_SENSE | SUGGEST_ABORT;
break;
case SUGGEST_RETRY:
status = MAYREDO;
exit = DRIVER_SENSE | SUGGEST_RETRY;
break;
case SUGGEST_ABORT:
status = FINISHED;
exit = DRIVER_SENSE | SUGGEST_ABORT;
break;
case SUGGEST_SENSE:
scsi_request_sense (SCpnt);
status = PENDING;
break;
}
break;
case CONDITION_GOOD:
case INTERMEDIATE_GOOD:
case INTERMEDIATE_C_GOOD:
break;
case BUSY:
case QUEUE_FULL:
update_timeout(SCpnt, oldto);
status = REDO;
break;
case RESERVATION_CONFLICT:
printk("scsi%d, channel %d : RESERVATION CONFLICT performing"
" reset.\n", SCpnt->host->host_no, SCpnt->channel);
scsi_reset(SCpnt, SCSI_RESET_SYNCHRONOUS);
return;
#if 0
exit = DRIVER_SOFT | SUGGEST_ABORT;
status = MAYREDO;
break;
#endif
default:
printk ("Internal error %s %d \n"
"status byte = %d \n", __FILE__,
__LINE__, status_byte(result));
}
break;
default:
panic("scsi: unsupported message byte %d received\n",
msg_byte(result));
}
break;
case DID_TIME_OUT:
#ifdef DEBUG
printk("Host returned DID_TIME_OUT - ");
#endif
if (SCpnt->flags & WAS_TIMEDOUT)
{
#ifdef DEBUG
printk("Aborting\n");
#endif
if (SCpnt->cmnd[0] != TEST_UNIT_READY &&
SCpnt->cmnd[0] != INQUIRY)
status = MAYREDO;
exit = (DRIVER_TIMEOUT | SUGGEST_ABORT);
}
else
{
#ifdef DEBUG
printk ("Retrying.\n");
#endif
SCpnt->flags |= WAS_TIMEDOUT;
SCpnt->internal_timeout &= ~IN_ABORT;
status = REDO;
}
break;
case DID_BUS_BUSY:
case DID_PARITY:
status = REDO;
break;
case DID_NO_CONNECT:
#ifdef DEBUG
printk("Couldn't connect.\n");
#endif
exit = (DRIVER_HARD | SUGGEST_ABORT);
break;
case DID_ERROR:
status = MAYREDO;
exit = (DRIVER_HARD | SUGGEST_ABORT);
break;
case DID_BAD_TARGET:
case DID_ABORT:
exit = (DRIVER_INVALID | SUGGEST_ABORT);
break;
case DID_RESET:
if (SCpnt->flags & IS_RESETTING)
{
SCpnt->flags &= ~IS_RESETTING;
status = REDO;
break;
}
if(msg_byte(result) == GOOD &&
status_byte(result) == CHECK_CONDITION) {
switch (check_sense(SCpnt)) {
case 0:
update_timeout(SCpnt, oldto);
status = REDO;
break;
case SUGGEST_REMAP:
case SUGGEST_RETRY:
status = MAYREDO;
exit = DRIVER_SENSE | SUGGEST_RETRY;
break;
case SUGGEST_ABORT:
status = FINISHED;
exit = DRIVER_SENSE | SUGGEST_ABORT;
break;
case SUGGEST_SENSE:
scsi_request_sense (SCpnt);
status = PENDING;
break;
}
} else {
status=REDO;
exit = SUGGEST_RETRY;
}
break;
default :
exit = (DRIVER_ERROR | SUGGEST_DIE);
}
switch (status)
{
case FINISHED:
case PENDING:
break;
case MAYREDO:
#ifdef DEBUG
printk("In MAYREDO, allowing %d retries, have %d\n",
SCpnt->allowed, SCpnt->retries);
#endif
if ((++SCpnt->retries) < SCpnt->allowed)
{
if ((SCpnt->retries >= (SCpnt->allowed >> 1))
&& !(SCpnt->host->last_reset > 0 &&
jiffies < SCpnt->host->last_reset + MIN_RESET_PERIOD)
&& !(SCpnt->flags & WAS_RESET))
{
printk("scsi%d channel %d : resetting for second half of retries.\n",
SCpnt->host->host_no, SCpnt->channel);
scsi_reset(SCpnt, SCSI_RESET_SYNCHRONOUS);
break;
}
}
else
{
status = FINISHED;
break;
}
case REDO:
if (SCpnt->flags & WAS_SENSE)
scsi_request_sense(SCpnt);
else
{
memcpy ((void *) SCpnt->cmnd,
(void*) SCpnt->data_cmnd,
sizeof(SCpnt->data_cmnd));
SCpnt->request_buffer = SCpnt->buffer;
SCpnt->request_bufflen = SCpnt->bufflen;
SCpnt->use_sg = SCpnt->old_use_sg;
SCpnt->cmd_len = SCpnt->old_cmd_len;
internal_cmnd (SCpnt);
}
break;
default:
INTERNAL_ERROR;
}
if (status == FINISHED) {
#ifdef DEBUG
printk("Calling done function - at address %p\n", SCpnt->done);
#endif
host->host_busy--;
if (host->block && host->host_busy == 0) {
host_active = NULL;
if (MAJOR(SCpnt->request.rq_dev) != SCSI_DISK_MAJOR &&
MAJOR(SCpnt->request.rq_dev) != SCSI_CDROM_MAJOR) {
struct Scsi_Host * next;
for (next = host->block; next != host; next = next->block)
wake_up(&next->host_wait);
}
}
wake_up(&host->host_wait);
SCpnt->result = result | ((exit & 0xff) << 24);
SCpnt->use_sg = SCpnt->old_use_sg;
SCpnt->cmd_len = SCpnt->old_cmd_len;
SCpnt->done (SCpnt);
}
#undef FINISHED
#undef REDO
#undef MAYREDO
#undef PENDING
}
int scsi_abort (Scsi_Cmnd * SCpnt, int why)
{
int oldto;
unsigned long flags;
struct Scsi_Host * host = SCpnt->host;
while(1)
{
save_flags(flags);
cli();
if (SCpnt->serial_number != SCpnt->serial_number_at_timeout) {
restore_flags(flags);
return 0;
}
if (SCpnt->internal_timeout & IN_ABORT)
{
restore_flags(flags);
while (SCpnt->internal_timeout & IN_ABORT)
barrier();
}
else
{
SCpnt->internal_timeout |= IN_ABORT;
oldto = update_timeout(SCpnt, ABORT_TIMEOUT);
if ((SCpnt->flags & IS_RESETTING) && SCpnt->device->soft_reset) {
printk("Stale command on %d %d:%d appears to have died when"
" the bus was reset\n",
SCpnt->channel, SCpnt->target, SCpnt->lun);
}
restore_flags(flags);
if (!host->host_busy) {
SCpnt->internal_timeout &= ~IN_ABORT;
update_timeout(SCpnt, oldto);
return 0;
}
printk("scsi : aborting command due to timeout : pid %lu, scsi%d,"
" channel %d, id %d, lun %d ",
SCpnt->pid, SCpnt->host->host_no, (int) SCpnt->channel,
(int) SCpnt->target, (int) SCpnt->lun);
print_command (SCpnt->cmnd);
if (SCpnt->serial_number != SCpnt->serial_number_at_timeout)
return 0;
SCpnt->abort_reason = why;
switch(host->hostt->abort(SCpnt)) {
case SCSI_ABORT_BUSY:
case SCSI_ABORT_SNOOZE:
if(why == DID_TIME_OUT) {
save_flags(flags);
cli();
SCpnt->internal_timeout &= ~IN_ABORT;
if(SCpnt->flags & WAS_TIMEDOUT) {
restore_flags(flags);
return 1;
} else {
SCpnt->flags |= WAS_TIMEDOUT;
oldto = SCpnt->timeout_per_command;
update_timeout(SCpnt, oldto);
}
restore_flags(flags);
}
return 0;
case SCSI_ABORT_PENDING:
if(why != DID_TIME_OUT) {
save_flags(flags);
cli();
update_timeout(SCpnt, oldto);
restore_flags(flags);
}
return 0;
case SCSI_ABORT_SUCCESS:
SCpnt->internal_timeout &= ~IN_ABORT;
return 0;
case SCSI_ABORT_NOT_RUNNING:
SCpnt->internal_timeout &= ~IN_ABORT;
update_timeout(SCpnt, 0);
return 0;
case SCSI_ABORT_ERROR:
default:
SCpnt->internal_timeout &= ~IN_ABORT;
return 1;
}
}
}
}
static inline void scsi_mark_device_reset(Scsi_Device *Device)
{
Device->was_reset = 1;
Device->expecting_cc_ua = 1;
}
void scsi_mark_host_reset(struct Scsi_Host *Host)
{
Scsi_Cmnd *SCpnt;
for (SCpnt = Host->host_queue; SCpnt; SCpnt = SCpnt->next)
scsi_mark_device_reset(SCpnt->device);
}
void scsi_mark_bus_reset(struct Scsi_Host *Host, int channel)
{
Scsi_Cmnd *SCpnt;
for (SCpnt = Host->host_queue; SCpnt; SCpnt = SCpnt->next)
if (SCpnt->channel == channel)
scsi_mark_device_reset(SCpnt->device);
}
int scsi_reset (Scsi_Cmnd * SCpnt, unsigned int reset_flags)
{
int temp;
unsigned long flags;
Scsi_Cmnd * SCpnt1;
struct Scsi_Host * host = SCpnt->host;
printk("SCSI bus is being reset for host %d channel %d.\n",
host->host_no, SCpnt->channel);
#if 0
SCpnt->host->suggest_bus_reset = FALSE;
SCpnt1 = host->host_queue;
while(SCpnt1) {
if( SCpnt1->request.rq_status != RQ_INACTIVE
&& (SCpnt1->flags & (WAS_RESET | IS_RESETTING)) == 0 )
break;
SCpnt1 = SCpnt1->next;
}
if( SCpnt1 == NULL ) {
reset_flags |= SCSI_RESET_SUGGEST_BUS_RESET;
}
if( reset_flags & SCSI_RESET_SUGGEST_BUS_RESET ) {
SCpnt->host->suggest_bus_reset = TRUE;
}
#endif
while (1) {
save_flags(flags);
cli();
if (reset_flags & SCSI_RESET_ASYNCHRONOUS)
if (SCpnt->serial_number != SCpnt->serial_number_at_timeout) {
restore_flags(flags);
return 0;
}
if (SCpnt->internal_timeout & IN_RESET)
{
restore_flags(flags);
while (SCpnt->internal_timeout & IN_RESET)
barrier();
}
else
{
SCpnt->internal_timeout |= IN_RESET;
update_timeout(SCpnt, RESET_TIMEOUT);
if (host->host_busy)
{
restore_flags(flags);
SCpnt1 = host->host_queue;
while(SCpnt1) {
if (SCpnt1->request.rq_status != RQ_INACTIVE) {
#if 0
if (!(SCpnt1->flags & IS_RESETTING) &&
!(SCpnt1->internal_timeout & IN_ABORT))
scsi_abort(SCpnt1, DID_RESET);
#endif
SCpnt1->flags |= (WAS_RESET | IS_RESETTING);
}
SCpnt1 = SCpnt1->next;
}
host->last_reset = jiffies;
temp = host->hostt->reset(SCpnt, reset_flags);
if ((host->last_reset < jiffies) ||
(host->last_reset > (jiffies + 20 * HZ)))
host->last_reset = jiffies;
}
else
{
if (!host->block) host->host_busy++;
restore_flags(flags);
host->last_reset = jiffies;
SCpnt->flags |= (WAS_RESET | IS_RESETTING);
temp = host->hostt->reset(SCpnt, reset_flags);
if ((host->last_reset < jiffies) ||
(host->last_reset > (jiffies + 20 * HZ)))
host->last_reset = jiffies;
if (!host->block) host->host_busy--;
}
#ifdef DEBUG
printk("scsi reset function returned %d\n", temp);
#endif
switch(temp & SCSI_RESET_ACTION) {
case SCSI_RESET_SUCCESS:
if (temp & SCSI_RESET_HOST_RESET)
scsi_mark_host_reset(host);
else if (temp & SCSI_RESET_BUS_RESET)
scsi_mark_bus_reset(host, SCpnt->channel);
else scsi_mark_device_reset(SCpnt->device);
save_flags(flags);
cli();
SCpnt->internal_timeout &= ~(IN_RESET|IN_RESET2|IN_RESET3);
restore_flags(flags);
return 0;
case SCSI_RESET_PENDING:
if (temp & SCSI_RESET_HOST_RESET)
scsi_mark_host_reset(host);
else if (temp & SCSI_RESET_BUS_RESET)
scsi_mark_bus_reset(host, SCpnt->channel);
else scsi_mark_device_reset(SCpnt->device);
case SCSI_RESET_NOT_RUNNING:
return 0;
case SCSI_RESET_PUNT:
SCpnt->internal_timeout &= ~(IN_RESET|IN_RESET2|IN_RESET3);
scsi_request_sense (SCpnt);
return 0;
case SCSI_RESET_WAKEUP:
if (temp & SCSI_RESET_HOST_RESET)
scsi_mark_host_reset(host);
else if (temp & SCSI_RESET_BUS_RESET)
scsi_mark_bus_reset(host, SCpnt->channel);
else scsi_mark_device_reset(SCpnt->device);
SCpnt->internal_timeout &= ~(IN_RESET|IN_RESET2|IN_RESET3);
scsi_request_sense (SCpnt);
if( temp & SCSI_RESET_HOST_RESET )
{
SCpnt1 = host->host_queue;
while(SCpnt1) {
if (SCpnt1->request.rq_status != RQ_INACTIVE
&& SCpnt1 != SCpnt)
scsi_request_sense (SCpnt1);
SCpnt1 = SCpnt1->next;
}
} else if( temp & SCSI_RESET_BUS_RESET ) {
SCpnt1 = host->host_queue;
while(SCpnt1) {
if(SCpnt1->request.rq_status != RQ_INACTIVE
&& SCpnt1 != SCpnt
&& SCpnt1->channel == SCpnt->channel)
scsi_request_sense (SCpnt);
SCpnt1 = SCpnt1->next;
}
}
return 0;
case SCSI_RESET_SNOOZE:
save_flags(flags);
cli();
SCpnt->internal_timeout &= ~(IN_RESET|IN_RESET2|IN_RESET3);
update_timeout(SCpnt, 0);
restore_flags(flags);
case SCSI_RESET_ERROR:
default:
return 1;
}
return temp;
}
}
}
static void scsi_main_timeout(void)
{
int timed_out;
unsigned long flags;
struct Scsi_Host * host;
Scsi_Cmnd * SCpnt = NULL;
save_flags(flags);
cli();
update_timeout(NULL, 0);
timed_out = 0;
for (host = scsi_hostlist; host; host = host->next) {
for (SCpnt = host->host_queue; SCpnt; SCpnt = SCpnt->next)
if (SCpnt->timeout == -1)
{
SCpnt->timeout = 0;
SCpnt->serial_number_at_timeout = SCpnt->serial_number;
++timed_out;
}
}
if (timed_out > 0) {
for (host = scsi_hostlist; host; host = host->next) {
for (SCpnt = host->host_queue; SCpnt; SCpnt = SCpnt->next)
if (SCpnt->serial_number_at_timeout > 0 &&
SCpnt->serial_number_at_timeout == SCpnt->serial_number)
{
restore_flags(flags);
scsi_times_out(SCpnt);
SCpnt->serial_number_at_timeout = 0;
cli();
}
}
}
restore_flags(flags);
}
static int update_timeout(Scsi_Cmnd * SCset, int timeout)
{
unsigned int least, used;
unsigned int oldto;
unsigned long flags;
struct Scsi_Host * host;
Scsi_Cmnd * SCpnt = NULL;
save_flags(flags);
cli();
oldto = 0;
if (jiffies == time_start && timer_table[SCSI_TIMER].expires > 0) {
if(SCset){
oldto = SCset->timeout;
SCset->timeout = timeout;
if (timeout > 0 &&
jiffies + timeout < timer_table[SCSI_TIMER].expires)
timer_table[SCSI_TIMER].expires = jiffies + timeout;
}
restore_flags(flags);
return oldto;
}
used = (time_start) ? (jiffies - time_start) : 0;
oldto = 0;
if(SCset){
oldto = SCset->timeout - used;
SCset->timeout = timeout;
}
least = 0xffffffff;
for(host = scsi_hostlist; host; host = host->next)
for(SCpnt = host->host_queue; SCpnt; SCpnt = SCpnt->next)
if (SCpnt->timeout > 0) {
if (SCpnt != SCset)
SCpnt->timeout -= used;
if(SCpnt->timeout <= 0) SCpnt->timeout = -1;
if(SCpnt->timeout > 0 && SCpnt->timeout < least)
least = SCpnt->timeout;
}
if (least != 0xffffffff)
{
time_start = jiffies;
timer_table[SCSI_TIMER].expires = (time_elapsed = least) + jiffies;
timer_active |= 1 << SCSI_TIMER;
}
else
{
timer_table[SCSI_TIMER].expires = time_start = time_elapsed = 0;
timer_active &= ~(1 << SCSI_TIMER);
}
restore_flags(flags);
return oldto;
}
#ifdef CONFIG_MODULES
static int scsi_register_host(Scsi_Host_Template *);
static void scsi_unregister_host(Scsi_Host_Template *);
#endif
void *scsi_malloc(unsigned int len)
{
unsigned int nbits, mask;
unsigned long flags;
int i, j;
if(len % SECTOR_SIZE != 0 || len > PAGE_SIZE)
return NULL;
save_flags(flags);
cli();
nbits = len >> 9;
mask = (1 << nbits) - 1;
for(i=0;i < dma_sectors / SECTORS_PER_PAGE; i++)
for(j=0; j<=SECTORS_PER_PAGE - nbits; j++){
if ((dma_malloc_freelist[i] & (mask << j)) == 0){
dma_malloc_freelist[i] |= (mask << j);
restore_flags(flags);
dma_free_sectors -= nbits;
#ifdef DEBUG
printk("SMalloc: %d %p\n",len, dma_malloc_pages[i] + (j << 9));
#endif
return (void *) ((unsigned long) dma_malloc_pages[i] + (j << 9));
}
}
restore_flags(flags);
return NULL;
}
int scsi_free(void *obj, unsigned int len)
{
unsigned int page, sector, nbits, mask;
unsigned long flags;
#ifdef DEBUG
unsigned long ret = 0;
#ifdef __mips__
__asm__ __volatile__ ("move\t%0,$31":"=r"(ret));
#else
ret = __builtin_return_address(0);
#endif
printk("scsi_free %p %d\n",obj, len);
#endif
for (page = 0; page < dma_sectors / SECTORS_PER_PAGE; page++) {
unsigned long page_addr = (unsigned long) dma_malloc_pages[page];
if ((unsigned long) obj >= page_addr &&
(unsigned long) obj < page_addr + PAGE_SIZE)
{
sector = (((unsigned long) obj) - page_addr) >> 9;
nbits = len >> 9;
mask = (1 << nbits) - 1;
if ((mask << sector) >= (1 << SECTORS_PER_PAGE))
panic ("scsi_free:Bad memory alignment");
save_flags(flags);
cli();
if((dma_malloc_freelist[page] &
(mask << sector)) != (mask<<sector)){
#ifdef DEBUG
printk("scsi_free(obj=%p, len=%d) called from %08lx\n",
obj, len, ret);
#endif
panic("scsi_free:Trying to free unused memory");
}
dma_free_sectors += nbits;
dma_malloc_freelist[page] &= ~(mask << sector);
restore_flags(flags);
return 0;
}
}
panic("scsi_free:Bad offset");
}
int scsi_loadable_module_flag;
void * scsi_init_malloc(unsigned int size, int priority)
{
void * retval;
if ((size % PAGE_SIZE) == 0) {
int order, a_size;
for (order = 0, a_size = PAGE_SIZE;
a_size < size; order++, a_size <<= 1)
;
retval = (void *) __get_dma_pages(priority & GFP_LEVEL_MASK,
order);
} else
retval = kmalloc(size, priority);
if (retval)
memset(retval, 0, size);
return retval;
}
void scsi_init_free(char * ptr, unsigned int size)
{
if ((size % PAGE_SIZE) == 0) {
int order, a_size;
for (order = 0, a_size = PAGE_SIZE;
a_size < size; order++, a_size <<= 1)
;
free_pages((unsigned long)ptr, order);
} else
kfree(ptr);
}
void scsi_build_commandblocks(Scsi_Device * SDpnt)
{
struct Scsi_Host *host = SDpnt->host;
int j;
Scsi_Cmnd * SCpnt;
if (SDpnt->queue_depth == 0)
SDpnt->queue_depth = host->cmd_per_lun;
SDpnt->device_queue = NULL;
for(j=0;j<SDpnt->queue_depth;j++){
SCpnt = (Scsi_Cmnd *)
scsi_init_malloc(sizeof(Scsi_Cmnd),
GFP_ATOMIC |
(host->unchecked_isa_dma ? GFP_DMA : 0));
SCpnt->host = host;
SCpnt->device = SDpnt;
SCpnt->target = SDpnt->id;
SCpnt->lun = SDpnt->lun;
SCpnt->channel = SDpnt->channel;
SCpnt->request.rq_status = RQ_INACTIVE;
SCpnt->use_sg = 0;
SCpnt->old_use_sg = 0;
SCpnt->old_cmd_len = 0;
SCpnt->timeout = 0;
SCpnt->underflow = 0;
SCpnt->transfersize = 0;
SCpnt->serial_number = 0;
SCpnt->serial_number_at_timeout = 0;
SCpnt->host_scribble = NULL;
if(host->host_queue)
host->host_queue->prev = SCpnt;
SCpnt->next = host->host_queue;
SCpnt->prev = NULL;
host->host_queue = SCpnt;
SCpnt->device_next = SDpnt->device_queue;
SDpnt->device_queue = SCpnt;
}
SDpnt->has_cmdblocks = 1;
}
int scsi_dev_init(void)
{
Scsi_Device * SDpnt;
struct Scsi_Host * shpnt;
struct Scsi_Device_Template * sdtpnt;
#ifdef FOO_ON_YOU
return;
#endif
#if CONFIG_PROC_FS
dispatch_scsi_info_ptr = dispatch_scsi_info;
#endif
scsi_loadable_module_flag = 0;
timer_table[SCSI_TIMER].fn = scsi_main_timeout;
timer_table[SCSI_TIMER].expires = 0;
#ifdef CONFIG_MODULES
register_symtab(&scsi_symbol_table);
#endif
#if CONFIG_PROC_FS
proc_scsi_register(0, &proc_scsi_scsi);
#endif
scsi_init();
scsi_devices = (Scsi_Device *) NULL;
for (shpnt = scsi_hostlist; shpnt; shpnt = shpnt->next) {
scan_scsis(shpnt,0,0,0,0);
if (shpnt->select_queue_depths != NULL)
(shpnt->select_queue_depths)(shpnt, scsi_devices);
}
printk("scsi : detected ");
for (sdtpnt = scsi_devicelist; sdtpnt; sdtpnt = sdtpnt->next)
if (sdtpnt->dev_noticed && sdtpnt->name)
printk("%d SCSI %s%s ", sdtpnt->dev_noticed, sdtpnt->name,
(sdtpnt->dev_noticed != 1) ? "s" : "");
printk("total.\n");
for(sdtpnt = scsi_devicelist; sdtpnt; sdtpnt = sdtpnt->next)
if(sdtpnt->init && sdtpnt->dev_noticed) (*sdtpnt->init)();
for (SDpnt=scsi_devices; SDpnt; SDpnt = SDpnt->next) {
SDpnt->scsi_request_fn = NULL;
for(sdtpnt = scsi_devicelist; sdtpnt; sdtpnt = sdtpnt->next)
if(sdtpnt->attach) (*sdtpnt->attach)(SDpnt);
if(SDpnt->attached) scsi_build_commandblocks(SDpnt);
}
resize_dma_pool();
for(sdtpnt = scsi_devicelist; sdtpnt; sdtpnt = sdtpnt->next)
if(sdtpnt->finish && sdtpnt->nr_dev)
(*sdtpnt->finish)();
scsi_loadable_module_flag = 1;
return 0;
}
static void print_inquiry(unsigned char *data)
{
int i;
printk("  Vendor: ");
for (i = 8; i < 16; i++)
{
if (data[i] >= 0x20 && i < data[4] + 5)
printk("%c", data[i]);
else
printk(" ");
}
printk("  Model: ");
for (i = 16; i < 32; i++)
{
if (data[i] >= 0x20 && i < data[4] + 5)
printk("%c", data[i]);
else
printk(" ");
}
printk("  Rev: ");
for (i = 32; i < 36; i++)
{
if (data[i] >= 0x20 && i < data[4] + 5)
printk("%c", data[i]);
else
printk(" ");
}
printk("\n");
i = data[0] & 0x1f;
printk("  Type:   %s ",
i < MAX_SCSI_DEVICE_CODE ? scsi_device_types[i] : "Unknown          " );
printk("                 ANSI SCSI revision: %02x", data[2] & 0x07);
if ((data[2] & 0x07) == 1 && (data[3] & 0x0f) == 1)
printk(" CCS\n");
else
printk("\n");
}
#ifdef CONFIG_PROC_FS
int scsi_proc_info(char *buffer, char **start, off_t offset, int length,
int hostno, int inout)
{
Scsi_Cmnd *SCpnt;
struct Scsi_Device_Template *SDTpnt;
Scsi_Device *scd, *scd_h = NULL;
struct Scsi_Host *HBA_ptr;
char *p;
int host, channel, id, lun;
int size, len = 0;
off_t begin = 0;
off_t pos = 0;
scd = scsi_devices;
HBA_ptr = scsi_hostlist;
if(inout == 0) {
size = sprintf(buffer+len,"Attached devices: %s\n", (scd)?"":"none");
len += size;
pos = begin + len;
while (HBA_ptr) {
#if 0
size += sprintf(buffer+len,"scsi%2d: %s\n", (int) HBA_ptr->host_no,
HBA_ptr->hostt->procname);
len += size;
pos = begin + len;
#endif
scd = scsi_devices;
while (scd) {
if (scd->host == HBA_ptr) {
proc_print_scsidevice(scd, buffer, &size, len);
len += size;
pos = begin + len;
if (pos < offset) {
len = 0;
begin = pos;
}
if (pos > offset + length)
goto stop_output;
}
scd = scd->next;
}
HBA_ptr = HBA_ptr->next;
}
stop_output:
*start=buffer+(offset-begin);
len-=(offset-begin);
if(len>length)
len = length;
return (len);
}
if(!buffer || length < 25 || strncmp("scsi", buffer, 4))
return(-EINVAL);
if(!strncmp("add-single-device", buffer + 5, 17)) {
p = buffer + 23;
host = simple_strtoul(p, &p, 0);
channel = simple_strtoul(p+1, &p, 0);
id = simple_strtoul(p+1, &p, 0);
lun = simple_strtoul(p+1, &p, 0);
printk("scsi singledevice %d %d %d %d\n", host, channel,
id, lun);
while(scd && (scd->host->host_no != host
|| scd->channel != channel
|| scd->id != id
|| scd->lun != lun)) {
scd = scd->next;
}
if(scd)
return(-ENOSYS);
while(HBA_ptr && HBA_ptr->host_no != host)
HBA_ptr = HBA_ptr->next;
if(!HBA_ptr)
return(-ENXIO);
scan_scsis (HBA_ptr, 1, channel, id, lun);
return(length);
}
else if(!strncmp("remove-single-device", buffer + 5, 20)) {
p = buffer + 26;
host = simple_strtoul(p, &p, 0);
channel = simple_strtoul(p+1, &p, 0);
id = simple_strtoul(p+1, &p, 0);
lun = simple_strtoul(p+1, &p, 0);
while(scd != NULL) {
if(scd->host->host_no == host
&& scd->channel == channel
&& scd->id == id
&& scd->lun == lun){
break;
}
scd_h = scd;
scd = scd->next;
}
if(scd == NULL)
return(-ENODEV);
if(scd->access_count)
return(-EBUSY);
SDTpnt = scsi_devicelist;
while(SDTpnt != NULL) {
if(SDTpnt->detach) (*SDTpnt->detach)(scd);
SDTpnt = SDTpnt->next;
}
if(scd->attached == 0) {
for(SCpnt=scd->host->host_queue; SCpnt; SCpnt = SCpnt->next){
if(SCpnt->device == scd) {
if(SCpnt->prev != NULL)
SCpnt->prev->next = SCpnt->next;
if(SCpnt->next != NULL)
SCpnt->next->prev = SCpnt->prev;
if(SCpnt == scd->host->host_queue)
scd->host->host_queue = SCpnt->next;
scsi_init_free((char *) SCpnt, sizeof(*SCpnt));
}
}
if(scd_h != NULL) {
scd_h->next = scd->next;
} else if (scsi_devices == scd) {
scsi_devices = scd->next;
}
scsi_init_free((char *) scd, sizeof(Scsi_Device));
} else {
return(-EBUSY);
}
return(0);
}
return(-EINVAL);
}
#endif
static void resize_dma_pool(void)
{
int i;
unsigned long size;
struct Scsi_Host * shpnt;
struct Scsi_Host * host = NULL;
Scsi_Device * SDpnt;
unsigned long flags;
FreeSectorBitmap * new_dma_malloc_freelist = NULL;
unsigned int new_dma_sectors = 0;
unsigned int new_need_isa_buffer = 0;
unsigned char ** new_dma_malloc_pages = NULL;
if( !scsi_devices )
{
if( dma_free_sectors != dma_sectors )
panic("SCSI DMA pool memory leak %d %d\n",dma_free_sectors,dma_sectors);
for(i=0; i < dma_sectors / SECTORS_PER_PAGE; i++)
scsi_init_free(dma_malloc_pages[i], PAGE_SIZE);
if (dma_malloc_pages)
scsi_init_free((char *) dma_malloc_pages,
(dma_sectors / SECTORS_PER_PAGE)*sizeof(*dma_malloc_pages));
dma_malloc_pages = NULL;
if (dma_malloc_freelist)
scsi_init_free((char *) dma_malloc_freelist,
(dma_sectors / SECTORS_PER_PAGE)*sizeof(*dma_malloc_freelist));
dma_malloc_freelist = NULL;
dma_sectors = 0;
dma_free_sectors = 0;
return;
}
new_dma_sectors = 2*SECTORS_PER_PAGE;
if (high_memory-1 > ISA_DMA_THRESHOLD)
scsi_need_isa_bounce_buffers = 1;
else
scsi_need_isa_bounce_buffers = 0;
if (scsi_devicelist)
for(shpnt=scsi_hostlist; shpnt; shpnt = shpnt->next)
new_dma_sectors += SECTORS_PER_PAGE;
for (SDpnt=scsi_devices; SDpnt; SDpnt = SDpnt->next) {
host = SDpnt->host;
if (SDpnt->type == TYPE_WORM || SDpnt->type == TYPE_ROM ||
SDpnt->type == TYPE_DISK || SDpnt->type == TYPE_MOD) {
new_dma_sectors += ((host->sg_tablesize *
sizeof(struct scatterlist) + 511) >> 9) *
SDpnt->queue_depth;
if (SDpnt->type == TYPE_WORM || SDpnt->type == TYPE_ROM)
new_dma_sectors += (2048 >> 9) * SDpnt->queue_depth;
}
else if (SDpnt->type == TYPE_SCANNER ||
SDpnt->type == TYPE_PROCESSOR ||
SDpnt->type == TYPE_MEDIUM_CHANGER) {
new_dma_sectors += (4096 >> 9) * SDpnt->queue_depth;
}
else {
if (SDpnt->type != TYPE_TAPE) {
printk("resize_dma_pool: unknown device type %d\n", SDpnt->type);
new_dma_sectors += (4096 >> 9) * SDpnt->queue_depth;
}
}
if(host->unchecked_isa_dma &&
scsi_need_isa_bounce_buffers &&
SDpnt->type != TYPE_TAPE) {
new_dma_sectors += (PAGE_SIZE >> 9) * host->sg_tablesize *
SDpnt->queue_depth;
new_need_isa_buffer++;
}
}
#ifdef DEBUG_INIT
printk("resize_dma_pool: needed dma sectors = %d\n", new_dma_sectors);
#endif
new_dma_sectors = (new_dma_sectors + 15) & 0xfff0;
if( new_dma_sectors < dma_sectors )
new_dma_sectors = dma_sectors;
if (new_dma_sectors)
{
size = (new_dma_sectors / SECTORS_PER_PAGE)*sizeof(FreeSectorBitmap);
new_dma_malloc_freelist = (FreeSectorBitmap *) scsi_init_malloc(size, GFP_ATOMIC);
memset(new_dma_malloc_freelist, 0, size);
size = (new_dma_sectors / SECTORS_PER_PAGE)*sizeof(*new_dma_malloc_pages);
new_dma_malloc_pages = (unsigned char **) scsi_init_malloc(size, GFP_ATOMIC);
memset(new_dma_malloc_pages, 0, size);
}
if( new_dma_sectors > dma_sectors ) {
for(i=dma_sectors / SECTORS_PER_PAGE; i< new_dma_sectors / SECTORS_PER_PAGE; i++)
new_dma_malloc_pages[i] = (unsigned char *)
scsi_init_malloc(PAGE_SIZE, GFP_ATOMIC | GFP_DMA);
}
save_flags(flags);
cli();
if (dma_malloc_freelist)
{
size = (dma_sectors / SECTORS_PER_PAGE)*sizeof(FreeSectorBitmap);
memcpy(new_dma_malloc_freelist, dma_malloc_freelist, size);
scsi_init_free((char *) dma_malloc_freelist, size);
}
dma_malloc_freelist = new_dma_malloc_freelist;
if (dma_malloc_pages)
{
size = (dma_sectors / SECTORS_PER_PAGE)*sizeof(*dma_malloc_pages);
memcpy(new_dma_malloc_pages, dma_malloc_pages, size);
scsi_init_free((char *) dma_malloc_pages, size);
}
dma_free_sectors += new_dma_sectors - dma_sectors;
dma_malloc_pages = new_dma_malloc_pages;
dma_sectors = new_dma_sectors;
need_isa_buffer = new_need_isa_buffer;
restore_flags(flags);
#ifdef DEBUG_INIT
printk("resize_dma_pool: dma free sectors   = %d\n", dma_free_sectors);
printk("resize_dma_pool: dma sectors        = %d\n", dma_sectors);
printk("resize_dma_pool: need isa buffers   = %d\n", need_isa_buffer);
#endif
}
#ifdef CONFIG_MODULES
static int scsi_register_host(Scsi_Host_Template * tpnt)
{
int pcount;
struct Scsi_Host * shpnt;
Scsi_Device * SDpnt;
struct Scsi_Device_Template * sdtpnt;
const char * name;
if (tpnt->next || !tpnt->detect) return 1;
pcount = next_scsi_host;
if ((tpnt->present = tpnt->detect(tpnt)))
{
if(pcount == next_scsi_host) {
if(tpnt->present > 1) {
printk("Failure to register low-level scsi driver");
scsi_unregister_host(tpnt);
return 1;
}
scsi_register(tpnt,0);
}
tpnt->next = scsi_hosts;
scsi_hosts = tpnt;
#if CONFIG_PROC_FS
build_proc_dir_entries(tpnt);
#endif
for(shpnt=scsi_hostlist; shpnt; shpnt = shpnt->next)
if(shpnt->hostt == tpnt)
{
if(tpnt->info)
name = tpnt->info(shpnt);
else
name = tpnt->name;
printk ("scsi%d : %s\n",
shpnt->host_no, name);
}
printk ("scsi : %d host%s.\n", next_scsi_host,
(next_scsi_host == 1) ? "" : "s");
scsi_make_blocked_list();
for(shpnt=scsi_hostlist; shpnt; shpnt = shpnt->next)
if(shpnt->hostt == tpnt) {
scan_scsis(shpnt,0,0,0,0);
if (shpnt->select_queue_depths != NULL)
(shpnt->select_queue_depths)(shpnt, scsi_devices);
}
for(sdtpnt = scsi_devicelist; sdtpnt; sdtpnt = sdtpnt->next)
if(sdtpnt->init && sdtpnt->dev_noticed) (*sdtpnt->init)();
for(SDpnt = scsi_devices; SDpnt; SDpnt = SDpnt->next)
if(SDpnt->host->hostt == tpnt)
{
for(sdtpnt = scsi_devicelist; sdtpnt; sdtpnt = sdtpnt->next)
if(sdtpnt->attach) (*sdtpnt->attach)(SDpnt);
if(SDpnt->attached) scsi_build_commandblocks(SDpnt);
}
resize_dma_pool();
for(sdtpnt = scsi_devicelist; sdtpnt; sdtpnt = sdtpnt->next)
if(sdtpnt->finish && sdtpnt->nr_dev)
(*sdtpnt->finish)();
}
#if defined(USE_STATIC_SCSI_MEMORY)
printk ("SCSI memory: total %ldKb, used %ldKb, free %ldKb.\n",
(scsi_memory_upper_value - scsi_memory_lower_value) / 1024,
(scsi_init_memory_start - scsi_memory_lower_value) / 1024,
(scsi_memory_upper_value - scsi_init_memory_start) / 1024);
#endif
MOD_INC_USE_COUNT;
return 0;
}
static void scsi_unregister_host(Scsi_Host_Template * tpnt)
{
Scsi_Host_Template * SHT, *SHTp;
Scsi_Device *sdpnt, * sdppnt, * sdpnt1;
Scsi_Cmnd * SCpnt;
unsigned long flags;
struct Scsi_Device_Template * sdtpnt;
struct Scsi_Host * shpnt, *sh1;
int pcount;
for(sdpnt = scsi_devices; sdpnt; sdpnt = sdpnt->next)
if(sdpnt->host->hostt == tpnt && sdpnt->host->hostt->usage_count
&& *sdpnt->host->hostt->usage_count) return;
for(shpnt = scsi_hostlist; shpnt; shpnt = shpnt->next)
{
if (shpnt->hostt != tpnt) continue;
for(SCpnt = shpnt->host_queue; SCpnt; SCpnt = SCpnt->next)
{
save_flags(flags);
cli();
if(SCpnt->request.rq_status != RQ_INACTIVE) {
restore_flags(flags);
for(SCpnt = shpnt->host_queue; SCpnt; SCpnt = SCpnt->next)
if(SCpnt->request.rq_status == RQ_SCSI_DISCONNECTING)
SCpnt->request.rq_status = RQ_INACTIVE;
printk("Device busy???\n");
return;
}
SCpnt->request.rq_status = RQ_SCSI_DISCONNECTING;
restore_flags(flags);
}
}
for(sdpnt = scsi_devices; sdpnt; sdpnt = sdpnt->next)
if(sdpnt->host->hostt == tpnt)
{
for(sdtpnt = scsi_devicelist; sdtpnt; sdtpnt = sdtpnt->next)
if(sdtpnt->detach) (*sdtpnt->detach)(sdpnt);
if (sdpnt->attached) {
printk("Attached usage count = %d\n", sdpnt->attached);
return;
}
}
for(sdpnt = scsi_devices; sdpnt; sdpnt = sdpnt->next)
if(sdpnt->host->hostt == tpnt)
while (sdpnt->host->host_queue) {
SCpnt = sdpnt->host->host_queue->next;
scsi_init_free((char *) sdpnt->host->host_queue, sizeof(Scsi_Cmnd));
sdpnt->host->host_queue = SCpnt;
if (SCpnt) SCpnt->prev = NULL;
sdpnt->has_cmdblocks = 0;
}
sdppnt = NULL;
for(sdpnt = scsi_devices; sdpnt; sdpnt = sdpnt1)
{
sdpnt1 = sdpnt->next;
if (sdpnt->host->hostt == tpnt) {
if (sdppnt)
sdppnt->next = sdpnt->next;
else
scsi_devices = sdpnt->next;
scsi_init_free((char *) sdpnt, sizeof (Scsi_Device));
} else
sdppnt = sdpnt;
}
shpnt = scsi_hostlist;
while(shpnt) {
sh1 = shpnt->next;
if(shpnt->hostt == tpnt) {
if(shpnt->loaded_as_module) {
pcount = next_scsi_host;
#if CONFIG_PROC_FS
proc_scsi_unregister(tpnt->proc_dir,
shpnt->host_no + PROC_SCSI_FILE);
#endif
if(tpnt->release)
(*tpnt->release)(shpnt);
else {
if (shpnt->irq) free_irq(shpnt->irq, NULL);
if (shpnt->dma_channel != 0xff) free_dma(shpnt->dma_channel);
if (shpnt->io_port && shpnt->n_io_port)
release_region(shpnt->io_port, shpnt->n_io_port);
}
if(pcount == next_scsi_host) scsi_unregister(shpnt);
tpnt->present--;
}
}
shpnt = sh1;
}
if( !scsi_devices )
resize_dma_pool();
printk ("scsi : %d host%s.\n", next_scsi_host,
(next_scsi_host == 1) ? "" : "s");
#if defined(USE_STATIC_SCSI_MEMORY)
printk ("SCSI memory: total %ldKb, used %ldKb, free %ldKb.\n",
(scsi_memory_upper_value - scsi_memory_lower_value) / 1024,
(scsi_init_memory_start - scsi_memory_lower_value) / 1024,
(scsi_memory_upper_value - scsi_init_memory_start) / 1024);
#endif
scsi_make_blocked_list();
if (tpnt->present) return;
for(SHTp=NULL, SHT=scsi_hosts; SHT; SHTp=SHT, SHT=SHT->next)
if(SHT == tpnt) {
if(SHTp)
SHTp->next = SHT->next;
else
scsi_hosts = SHT->next;
SHT->next = NULL;
break;
}
#if CONFIG_PROC_FS
proc_scsi_unregister(tpnt->proc_dir, tpnt->proc_dir->low_ino);
#endif
MOD_DEC_USE_COUNT;
}
static int scsi_register_device_module(struct Scsi_Device_Template * tpnt)
{
Scsi_Device * SDpnt;
if (tpnt->next) return 1;
scsi_register_device(tpnt);
for(SDpnt = scsi_devices; SDpnt; SDpnt = SDpnt->next)
if(tpnt->detect) SDpnt->attached += (*tpnt->detect)(SDpnt);
if(tpnt->init && tpnt->dev_noticed)
if ((*tpnt->init)()) return 1;
for(SDpnt = scsi_devices; SDpnt; SDpnt = SDpnt->next)
{
if(tpnt->attach) (*tpnt->attach)(SDpnt);
if(SDpnt->attached && SDpnt->has_cmdblocks == 0)
scsi_build_commandblocks(SDpnt);
}
if(tpnt->finish && tpnt->nr_dev) (*tpnt->finish)();
MOD_INC_USE_COUNT;
return 0;
}
static int scsi_unregister_device(struct Scsi_Device_Template * tpnt)
{
Scsi_Device * SDpnt;
Scsi_Cmnd * SCpnt;
struct Scsi_Device_Template * spnt;
struct Scsi_Device_Template * prev_spnt;
if( *tpnt->usage_count != 0) return 0;
for(SDpnt = scsi_devices; SDpnt; SDpnt = SDpnt->next)
{
if(tpnt->detach) (*tpnt->detach)(SDpnt);
if(SDpnt->attached == 0)
{
for(SCpnt = SDpnt->host->host_queue; SCpnt; SCpnt = SCpnt->next)
{
if(SCpnt->device == SDpnt)
{
if(SCpnt->prev != NULL)
SCpnt->prev->next = SCpnt->next;
if(SCpnt->next != NULL)
SCpnt->next->prev = SCpnt->prev;
if(SCpnt == SDpnt->host->host_queue)
SDpnt->host->host_queue = SCpnt->next;
scsi_init_free((char *) SCpnt, sizeof(*SCpnt));
}
}
SDpnt->has_cmdblocks = 0;
}
}
spnt = scsi_devicelist;
prev_spnt = NULL;
while(spnt != tpnt)
{
prev_spnt = spnt;
spnt = spnt->next;
}
if(prev_spnt == NULL)
scsi_devicelist = tpnt->next;
else
prev_spnt->next = spnt->next;
MOD_DEC_USE_COUNT;
return 0;
}
int scsi_register_module(int module_type, void * ptr)
{
switch(module_type){
case MODULE_SCSI_HA:
return scsi_register_host((Scsi_Host_Template *) ptr);
case MODULE_SCSI_DEV:
#ifdef CONFIG_KERNELD
if (scsi_hosts == NULL)
request_module("scsi_hostadapter");
#endif
return scsi_register_device_module((struct Scsi_Device_Template *) ptr);
case MODULE_SCSI_CONST:
case MODULE_SCSI_IOCTL:
default:
return 1;
}
}
void scsi_unregister_module(int module_type, void * ptr)
{
switch(module_type) {
case MODULE_SCSI_HA:
scsi_unregister_host((Scsi_Host_Template *) ptr);
break;
case MODULE_SCSI_DEV:
scsi_unregister_device((struct Scsi_Device_Template *) ptr);
break;
case MODULE_SCSI_CONST:
case MODULE_SCSI_IOCTL:
break;
default:
}
return;
}
#endif
#ifdef DEBUG_TIMEOUT
static void
scsi_dump_status(void)
{
int i;
struct Scsi_Host * shpnt;
Scsi_Cmnd * SCpnt;
printk("Dump of scsi parameters:\n");
i = 0;
for(shpnt = scsi_hostlist; shpnt; shpnt = shpnt->next)
for(SCpnt=shpnt->host_queue; SCpnt; SCpnt = SCpnt->next)
{
printk("(%d) %d:%d:%d:%d (%s %ld %ld %ld %d) (%d %d %x) (%d %d %d) %x %x %x\n",
i++, SCpnt->host->host_no,
SCpnt->channel,
SCpnt->target,
SCpnt->lun,
kdevname(SCpnt->request.rq_dev),
SCpnt->request.sector,
SCpnt->request.nr_sectors,
SCpnt->request.current_nr_sectors,
SCpnt->use_sg,
SCpnt->retries,
SCpnt->allowed,
SCpnt->flags,
SCpnt->timeout_per_command,
SCpnt->timeout,
SCpnt->internal_timeout,
SCpnt->cmnd[0],
SCpnt->sense_buffer[2],
SCpnt->result);
}
printk("wait_for_request = %p\n", wait_for_request);
printk("Dump of pending block device requests\n");
for(i=0; i<MAX_BLKDEV; i++)
if(blk_dev[i].current_request)
{
struct request * req;
printk("%d: ", i);
req = blk_dev[i].current_request;
while(req) {
printk("(%s %d %ld %ld %ld) ",
kdevname(req->rq_dev),
req->cmd,
req->sector,
req->nr_sectors,
req->current_nr_sectors);
req = req->next;
}
printk("\n");
}
}
#endif
#ifdef MODULE
int init_module(void) {
unsigned long size;
#if CONFIG_PROC_FS
dispatch_scsi_info_ptr = dispatch_scsi_info;
#endif
timer_table[SCSI_TIMER].fn = scsi_main_timeout;
timer_table[SCSI_TIMER].expires = 0;
register_symtab(&scsi_symbol_table);
scsi_loadable_module_flag = 1;
#if CONFIG_PROC_FS
proc_scsi_register(0, &proc_scsi_scsi);
#endif
dma_sectors = PAGE_SIZE / SECTOR_SIZE;
dma_free_sectors= dma_sectors;
size = (dma_sectors / SECTORS_PER_PAGE)*sizeof(FreeSectorBitmap);
dma_malloc_freelist = (unsigned char *) scsi_init_malloc(size, GFP_ATOMIC);
memset(dma_malloc_freelist, 0, size);
dma_malloc_pages = (unsigned char **)
scsi_init_malloc((dma_sectors / SECTORS_PER_PAGE)*sizeof(*dma_malloc_pages), GFP_ATOMIC);
dma_malloc_pages[0] = (unsigned char *)
scsi_init_malloc(PAGE_SIZE, GFP_ATOMIC | GFP_DMA);
return 0;
}
void cleanup_module( void)
{
#if CONFIG_PROC_FS
proc_scsi_unregister(0, PROC_SCSI_SCSI);
dispatch_scsi_info_ptr = 0L;
#endif
resize_dma_pool();
timer_table[SCSI_TIMER].fn = NULL;
timer_table[SCSI_TIMER].expires = 0;
}
#endif