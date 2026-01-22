#ifndef _HOSTS_H
#define _HOSTS_H
#include <linux/proc_fs.h>
#define SG_NONE 0
#define SG_ALL 0xff
#define DISABLE_CLUSTERING 0
#define ENABLE_CLUSTERING 1
typedef struct scsi_disk Disk;
typedef struct SHT
{
struct SHT * next;
long * usage_count;
struct proc_dir_entry *proc_dir;
int (*proc_info)(char *, char **, off_t, int, int, int);
const char *name;
int (* detect)(struct SHT *);
int (*release)(struct Scsi_Host *);
const char *(* info)(struct Scsi_Host *);
int (* command)(Scsi_Cmnd *);
int (* queuecommand)(Scsi_Cmnd *, void (*done)(Scsi_Cmnd *));
int (* abort)(Scsi_Cmnd *);
int (* reset)(Scsi_Cmnd *, unsigned int);
int (* slave_attach)(int, int);
int (* bios_param)(Disk *, kdev_t, int []);
int can_queue;
int this_id;
short unsigned int sg_tablesize;
short cmd_per_lun;
unsigned char present;
unsigned unchecked_isa_dma:1;
unsigned use_clustering:1;
} Scsi_Host_Template;
struct Scsi_Host
{
struct Scsi_Host * next;
unsigned short extra_bytes;
volatile unsigned char host_busy;
char host_no;
unsigned long last_reset;
struct wait_queue *host_wait;
Scsi_Cmnd *host_queue;
Scsi_Host_Template * hostt;
unsigned int max_id;
unsigned int max_lun;
unsigned int max_channel;
struct Scsi_Host * block;
unsigned wish_block:1;
unsigned char *base;
unsigned int io_port;
unsigned char n_io_port;
unsigned char irq;
unsigned char dma_channel;
unsigned int unique_id;
int this_id;
int can_queue;
short cmd_per_lun;
short unsigned int sg_tablesize;
unsigned unchecked_isa_dma:1;
unsigned use_clustering:1;
unsigned loaded_as_module:1;
void (*select_queue_depths)(struct Scsi_Host *, Scsi_Device *);
unsigned long hostdata[0];
};
extern struct Scsi_Host * scsi_hostlist;
extern struct Scsi_Device_Template * scsi_devicelist;
extern Scsi_Host_Template * scsi_hosts;
extern void build_proc_dir_entries(Scsi_Host_Template *);
extern void * scsi_init_malloc(unsigned int size, int priority);
extern void scsi_init_free(char * ptr, unsigned int size);
extern int next_scsi_host;
extern int scsi_loadable_module_flag;
unsigned int scsi_init(void);
extern struct Scsi_Host * scsi_register(Scsi_Host_Template *, int j);
extern void scsi_unregister(struct Scsi_Host * i);
#define BLANK_HOST {"", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
struct Scsi_Device_Template
{
struct Scsi_Device_Template * next;
const char * name;
const char * tag;
long * usage_count;
unsigned char scsi_type;
unsigned char major;
unsigned char nr_dev;
unsigned char dev_noticed;
unsigned char dev_max;
unsigned blk:1;
int (*detect)(Scsi_Device *);
int (*init)(void);
void (*finish)(void);
int (*attach)(Scsi_Device *);
void (*detach)(Scsi_Device *);
};
extern struct Scsi_Device_Template sd_template;
extern struct Scsi_Device_Template st_template;
extern struct Scsi_Device_Template sr_template;
extern struct Scsi_Device_Template sg_template;
int scsi_register_device(struct Scsi_Device_Template * sdpnt);
extern int scsi_register_module(int, void *);
extern void scsi_unregister_module(int, void *);
#define MODULE_SCSI_HA 1
#define MODULE_SCSI_CONST 2
#define MODULE_SCSI_IOCTL 3
#define MODULE_SCSI_DEV 4
#define SD_EXTRA_DEVS 2
#define ST_EXTRA_DEVS 2
#define SR_EXTRA_DEVS 2
#define SG_EXTRA_DEVS (SD_EXTRA_DEVS + SR_EXTRA_DEVS + ST_EXTRA_DEVS)
#endif