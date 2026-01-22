#ifndef _SCSI_H
#define _SCSI_H
#include <scsi/scsi.h>
#include <linux/random.h>
#ifndef TRUE
# define TRUE 1
#endif
#ifndef FALSE
# define FALSE 0
#endif
extern void scsi_make_blocked_list(void);
extern volatile int in_scan_scsis;
extern const unsigned char scsi_command_size[8];
#define COMMAND_SIZE(opcode) scsi_command_size[((opcode) >> 5) & 7]
#define IDENTIFY_BASE       0x80
#define IDENTIFY(can_disconnect, lun)   (IDENTIFY_BASE |\
((can_disconnect) ?  0x40 : 0) |\
((lun) & 0x07))
#define MAX_SCSI_DEVICE_CODE 10
extern const char *const scsi_device_types[MAX_SCSI_DEVICE_CODE];
#define DID_OK          0x00
#define DID_NO_CONNECT  0x01
#define DID_BUS_BUSY    0x02
#define DID_TIME_OUT    0x03
#define DID_BAD_TARGET  0x04
#define DID_ABORT       0x05
#define DID_PARITY      0x06
#define DID_ERROR       0x07
#define DID_RESET       0x08
#define DID_BAD_INTR    0x09
#define DRIVER_OK       0x00
#define DRIVER_BUSY         0x01
#define DRIVER_SOFT         0x02
#define DRIVER_MEDIA        0x03
#define DRIVER_ERROR        0x04
#define DRIVER_INVALID      0x05
#define DRIVER_TIMEOUT      0x06
#define DRIVER_HARD         0x07
#define DRIVER_SENSE	    0x08
#define SUGGEST_RETRY       0x10
#define SUGGEST_ABORT       0x20
#define SUGGEST_REMAP       0x30
#define SUGGEST_DIE         0x40
#define SUGGEST_SENSE       0x80
#define SUGGEST_IS_OK       0xff
#define DRIVER_MASK         0x0f
#define SUGGEST_MASK        0xf0
#define MAX_COMMAND_SIZE    12
#define SCSI_UNKNOWN    0
#define SCSI_1          1
#define SCSI_1_CCS      2
#define SCSI_2          3
#define SCSI_MAN_UNKNOWN     0
#define SCSI_MAN_NEC         1
#define SCSI_MAN_TOSHIBA     2
#define SCSI_MAN_NEC_OLDCDR  3
#define SCSI_MAN_SONY        4
#define SCSI_MAN_PIONEER     5
#define WAS_RESET       0x01
#define WAS_TIMEDOUT    0x02
#define WAS_SENSE       0x04
#define IS_RESETTING    0x08
#define IS_ABORTING     0x10
#define ASKED_FOR_SENSE 0x20
typedef struct scsi_device {
struct scsi_device * next;
unsigned char id, lun, channel;
unsigned int manufacturer;
int attached;
int access_count;
struct wait_queue * device_wait;
struct Scsi_Host * host;
void (*scsi_request_fn)(void);
struct scsi_cmnd *device_queue;
void *hostdata;
char type;
char scsi_level;
char vendor[8], model[16], rev[4];
unsigned char current_tag;
unsigned char sync_min_period;
unsigned char sync_max_offset;
unsigned char queue_depth;
unsigned writeable:1;
unsigned removable:1;
unsigned random:1;
unsigned has_cmdblocks:1;
unsigned changed:1;
unsigned busy:1;
unsigned lockable:1;
unsigned borken:1;
unsigned tagged_supported:1;
unsigned tagged_queue:1;
unsigned disconnect:1;
unsigned soft_reset:1;
unsigned sync:1;
unsigned single_lun:1;
unsigned was_reset:1;
unsigned expecting_cc_ua:1;
} Scsi_Device;
#define status_byte(result) (((result) >> 1) & 0x1f)
#define msg_byte(result)    (((result) >> 8) & 0xff)
#define host_byte(result)   (((result) >> 16) & 0xff)
#define driver_byte(result) (((result) >> 24) & 0xff)
#define suggestion(result)  (driver_byte(result) & SUGGEST_MASK)
#define sense_class(sense)  (((sense) >> 4) & 0x7)
#define sense_error(sense)  ((sense) & 0xf)
#define sense_valid(sense)  ((sense) & 0x80);
extern Scsi_Device * scsi_devices;
extern struct hd_struct * sd;
#if defined(MAJOR_NR) && (MAJOR_NR == SCSI_DISK_MAJOR)
extern struct hd_struct * sd;
#endif
extern int scsi_dev_init (void);
struct scatterlist {
char *  address;
char * alt_address;
unsigned int length;
};
#ifdef __alpha__
# define ISA_DMA_THRESHOLD (~0UL)
#else
# define ISA_DMA_THRESHOLD (0x00ffffff)
#endif
#define CONTIGUOUS_BUFFERS(X,Y) ((X->b_data+X->b_size) == Y->b_data)
#define SCSI_ABORT_SNOOZE 0
#define SCSI_ABORT_SUCCESS 1
#define SCSI_ABORT_PENDING 2
#define SCSI_ABORT_BUSY 3
#define SCSI_ABORT_NOT_RUNNING 4
#define SCSI_ABORT_ERROR 5
#define SCSI_RESET_SNOOZE 0
#define SCSI_RESET_PUNT 1
#define SCSI_RESET_SUCCESS 2
#define SCSI_RESET_PENDING 3
#define SCSI_RESET_WAKEUP 4
#define SCSI_RESET_NOT_RUNNING 5
#define SCSI_RESET_ERROR 6
#define SCSI_RESET_SYNCHRONOUS		0x01
#define SCSI_RESET_ASYNCHRONOUS		0x02
#define SCSI_RESET_SUGGEST_BUS_RESET	0x04
#define SCSI_RESET_SUGGEST_HOST_RESET	0x08
#define SCSI_RESET_BUS_RESET 0x100
#define SCSI_RESET_HOST_RESET 0x200
#define SCSI_RESET_ACTION   0xff
void *   scsi_malloc(unsigned int);
int      scsi_free(void *, unsigned int);
extern unsigned int dma_free_sectors;
extern unsigned int need_isa_buffer;
typedef struct scsi_pointer {
char * ptr;
int this_residual;
struct scatterlist *buffer;
int buffers_residual;
volatile int Status;
volatile int Message;
volatile int have_data_in;
volatile int sent_command;
volatile int phase;
} Scsi_Pointer;
typedef struct scsi_cmnd {
struct Scsi_Host * host;
Scsi_Device * device;
unsigned char target, lun, channel;
unsigned char cmd_len;
unsigned char old_cmd_len;
struct scsi_cmnd *next, *prev, *device_next, *reset_chain;
unsigned char cmnd[12];
unsigned request_bufflen;
void * request_buffer;
unsigned char data_cmnd[12];
unsigned short old_use_sg;
unsigned short use_sg;
unsigned short sglist_len;
unsigned short abort_reason;
unsigned bufflen;
void *buffer;
unsigned underflow;
unsigned transfersize;
struct request request;
unsigned char sense_buffer[16];
unsigned long serial_number;
unsigned long serial_number_at_timeout;
int retries;
int allowed;
int timeout_per_command, timeout_total, timeout;
unsigned volatile char internal_timeout;
unsigned flags;
int this_count;
void (*scsi_done)(struct scsi_cmnd *);
void (*done)(struct scsi_cmnd *);
Scsi_Pointer SCp;
unsigned char * host_scribble;
int result;
unsigned char tag;
unsigned long pid;
} Scsi_Cmnd;
extern int scsi_abort (Scsi_Cmnd *, int code);
extern void scsi_do_cmd (Scsi_Cmnd *, const void *cmnd ,
void *buffer, unsigned bufflen,
void (*done)(struct scsi_cmnd *),
int timeout, int retries);
extern Scsi_Cmnd * allocate_device(struct request **, Scsi_Device *, int);
extern Scsi_Cmnd * request_queueable(struct request *, Scsi_Device *);
extern int scsi_reset (Scsi_Cmnd *, unsigned int);
extern int max_scsi_hosts;
extern void proc_print_scsidevice(Scsi_Device *, char *, int *, int);
extern void print_command(unsigned char *);
extern void print_sense(const char *, Scsi_Cmnd *);
extern void print_driverbyte(int scsiresult);
extern void print_hostbyte(int scsiresult);
extern void scsi_mark_host_reset(struct Scsi_Host *Host);
extern void scsi_mark_bus_reset(struct Scsi_Host *Host, int channel);
#if defined(MAJOR_NR) && (MAJOR_NR != SCSI_TAPE_MAJOR)
#include "hosts.h"
static Scsi_Cmnd * end_scsi_request(Scsi_Cmnd * SCpnt, int uptodate, int sectors)
{
struct request * req;
struct buffer_head * bh;
req = &SCpnt->request;
req->errors = 0;
if (!uptodate) {
#if defined(MAJOR_NR) && (MAJOR_NR == SCSI_DISK_MAJOR)
printk(DEVICE_NAME " I/O error: dev %s, sector %lu, absolute sector %lu\n",
kdevname(req->rq_dev), req->sector,
req->sector + sd[MINOR(SCpnt->request.rq_dev)].start_sect);
#else
printk(DEVICE_NAME " I/O error: dev %s, sector %lu\n",
kdevname(req->rq_dev), req->sector);
#endif
}
do {
if ((bh = req->bh) != NULL) {
req->bh = bh->b_reqnext;
req->nr_sectors -= bh->b_size >> 9;
req->sector += bh->b_size >> 9;
bh->b_reqnext = NULL;
if (test_bit(BH_MD, &bh->b_state)) {
struct md_personality * pers=(struct md_personality *)bh->personality;
pers->end_request(bh,uptodate);
}
else {
mark_buffer_uptodate(bh, uptodate);
unlock_buffer(bh);
}
sectors -= bh->b_size >> 9;
if ((bh = req->bh) != NULL) {
req->current_nr_sectors = bh->b_size >> 9;
if (req->nr_sectors < req->current_nr_sectors) {
req->nr_sectors = req->current_nr_sectors;
printk("end_scsi_request: buffer-list destroyed\n");
}
}
}
} while(sectors && bh);
if (req->bh){
req->buffer = bh->b_data;
return SCpnt;
}
DEVICE_OFF(req->rq_dev);
if (req->sem != NULL) {
up(req->sem);
}
add_blkdev_randomness(MAJOR(req->rq_dev));
if (SCpnt->host->block) {
struct Scsi_Host * next;
for (next = SCpnt->host->block; next != SCpnt->host;
next = next->block)
wake_up(&next->host_wait);
}
req->rq_status = RQ_INACTIVE;
wake_up(&wait_for_request);
wake_up(&SCpnt->device->device_wait);
return NULL;
}
#define INIT_SCSI_REQUEST       \
if (!CURRENT) {             \
CLEAR_INTR;             \
restore_flags(flags);	\
return;                 \
}                           \
if (MAJOR(CURRENT->rq_dev) != MAJOR_NR)           \
panic(DEVICE_NAME ": request list destroyed");\
if (CURRENT->bh) {                                \
if (!buffer_locked(CURRENT->bh))              \
panic(DEVICE_NAME ": block not locked");  \
}
#endif
#ifdef MACH
#define SCSI_SLEEP(QUEUE, CONDITION) {		    \
if (CONDITION) {			            \
struct wait_queue wait = { NULL, NULL};     \
add_wait_queue(QUEUE, &wait);		    \
for(;;) {			            \
if (CONDITION) {		            \
if (intr_count)	                    \
panic("scsi: trying to call schedule() in interrupt" \
", file %s, line %d.\n", __FILE__, __LINE__);  \
schedule();			\
}				\
else			        \
break;      		\
}			        \
remove_wait_queue(QUEUE, &wait);\
}; }
#else
#define SCSI_SLEEP(QUEUE, CONDITION) {		    \
if (CONDITION) {			            \
struct wait_queue wait = { current, NULL};  \
add_wait_queue(QUEUE, &wait);		    \
for(;;) {			            \
current->state = TASK_UNINTERRUPTIBLE;	    \
if (CONDITION) {		            \
if (intr_count)	                    \
panic("scsi: trying to call schedule() in interrupt" \
", file %s, line %d.\n", __FILE__, __LINE__);  \
schedule();			\
}				\
else			        \
break;      		\
}			        \
remove_wait_queue(QUEUE, &wait);\
current->state = TASK_RUNNING;	\
}; }
#endif
#endif