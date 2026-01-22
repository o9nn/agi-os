#include <linux/config.h>
#undef REALLY_FAST_IO
#define INITIAL_MULT_COUNT	16
#ifndef SUPPORT_SLOW_DATA_PORTS
#define SUPPORT_SLOW_DATA_PORTS	1
#endif
#ifndef SUPPORT_VLB_SYNC
#define SUPPORT_VLB_SYNC	1
#endif
#ifndef DISK_RECOVERY_TIME
#define DISK_RECOVERY_TIME	0
#endif
#ifndef OK_TO_RESET_CONTROLLER
#define OK_TO_RESET_CONTROLLER	1
#endif
#ifndef FAKE_FDISK_FOR_EZDRIVE
#define FAKE_FDISK_FOR_EZDRIVE 	1
#endif
#ifndef FANCY_STATUS_DUMPS
#define FANCY_STATUS_DUMPS	1
#endif
#ifdef CONFIG_BLK_DEV_CMD640
#if 0
void cmd640_dump_regs (void);
#define CMD640_DUMP_REGS cmd640_dump_regs()
#endif
#endif
#if	defined(CONFIG_BLK_DEV_IDECD) || defined(CONFIG_BLK_DEV_IDETAPE) || \
defined(CONFIG_BLK_DEV_IDEFLOPPY) || defined(CONFIG_BLK_DEV_IDESCSI)
#define CONFIG_BLK_DEV_IDEATAPI 1
#endif
#define IDE_DRIVE_CMD		99
#if defined(CONFIG_BLK_DEV_IDESCSI) && !defined(CONFIG_SCSI)
#error "SCSI must also be selected"
#endif
typedef unsigned char	byte;
#define ERROR_MAX	8
#define ERROR_RESET	3
#define ERROR_RECAL	1
#ifdef REALLY_SLOW_IO
#undef REALLY_FAST_IO
#endif
#define HWIF(drive)		((ide_hwif_t *)((drive)->hwif))
#define HWGROUP(drive)		((ide_hwgroup_t *)(HWIF(drive)->hwgroup))
#define IDE_DATA_OFFSET		(0)
#define IDE_ERROR_OFFSET	(1)
#define IDE_NSECTOR_OFFSET	(2)
#define IDE_SECTOR_OFFSET	(3)
#define IDE_LCYL_OFFSET		(4)
#define IDE_HCYL_OFFSET		(5)
#define IDE_SELECT_OFFSET	(6)
#define IDE_STATUS_OFFSET	(7)
#define IDE_FEATURE_OFFSET	IDE_ERROR_OFFSET
#define IDE_COMMAND_OFFSET	IDE_STATUS_OFFSET
#define IDE_DATA_REG		(HWIF(drive)->io_base+IDE_DATA_OFFSET)
#define IDE_ERROR_REG		(HWIF(drive)->io_base+IDE_ERROR_OFFSET)
#define IDE_NSECTOR_REG		(HWIF(drive)->io_base+IDE_NSECTOR_OFFSET)
#define IDE_SECTOR_REG		(HWIF(drive)->io_base+IDE_SECTOR_OFFSET)
#define IDE_LCYL_REG		(HWIF(drive)->io_base+IDE_LCYL_OFFSET)
#define IDE_HCYL_REG		(HWIF(drive)->io_base+IDE_HCYL_OFFSET)
#define IDE_SELECT_REG		(HWIF(drive)->io_base+IDE_SELECT_OFFSET)
#define IDE_STATUS_REG		(HWIF(drive)->io_base+IDE_STATUS_OFFSET)
#define IDE_CONTROL_REG		(HWIF(drive)->ctl_port)
#define IDE_FEATURE_REG		IDE_ERROR_REG
#define IDE_COMMAND_REG		IDE_STATUS_REG
#define IDE_ALTSTATUS_REG	IDE_CONTROL_REG
#define IDE_IREASON_REG		IDE_NSECTOR_REG
#define IDE_BCOUNTL_REG		IDE_LCYL_REG
#define IDE_BCOUNTH_REG		IDE_HCYL_REG
#ifdef REALLY_FAST_IO
#define OUT_BYTE(b,p)		outb((b),(p))
#define IN_BYTE(p)		(byte)inb(p)
#else
#define OUT_BYTE(b,p)		outb_p((b),(p))
#define IN_BYTE(p)		(byte)inb_p(p)
#endif
#define GET_ERR()		IN_BYTE(IDE_ERROR_REG)
#define GET_STAT()		IN_BYTE(IDE_STATUS_REG)
#define OK_STAT(stat,good,bad)	(((stat)&((good)|(bad)))==(good))
#define BAD_R_STAT		(BUSY_STAT   | ERR_STAT)
#define BAD_W_STAT		(BAD_R_STAT  | WRERR_STAT)
#define BAD_STAT		(BAD_R_STAT  | DRQ_STAT)
#define DRIVE_READY		(READY_STAT  | SEEK_STAT)
#define DATA_READY		(DRQ_STAT)
#define IDE_MAJOR_NAME	"ide"
#define MAJOR_NAME	IDE_MAJOR_NAME
#define PARTN_BITS	6
#define PARTN_MASK	((1<<PARTN_BITS)-1)
#define MAX_DRIVES	2
#ifndef MAX_HWIFS
#define MAX_HWIFS	4
#endif
#define SECTOR_WORDS	(512 / 4)
#define WAIT_DRQ	(1*HZ)
#ifdef CONFIG_APM
#define WAIT_READY	(5*HZ)
#else
#define WAIT_READY	(3*HZ/100)
#endif
#define WAIT_PIDENTIFY	(1*HZ)
#define WAIT_WORSTCASE	(30*HZ)
#define WAIT_CMD	(10*HZ)
#if defined(CONFIG_BLK_DEV_HT6560B) || defined(CONFIG_BLK_DEV_PROMISE)
#define SELECT_DRIVE(hwif,drive)				\
{								\
if (hwif->selectproc)					\
hwif->selectproc(drive);			\
else							\
OUT_BYTE((drive)->select.all, hwif->io_base+IDE_SELECT_OFFSET); \
}
#else
#define SELECT_DRIVE(hwif,drive)  OUT_BYTE((drive)->select.all, hwif->io_base+IDE_SELECT_OFFSET);
#endif
#ifdef CONFIG_BLK_DEV_IDETAPE
#include "ide-tape.h"
#endif
#ifdef CONFIG_BLK_DEV_IDECD
struct atapi_request_sense {
unsigned char error_code : 7;
unsigned char valid      : 1;
byte reserved1;
unsigned char sense_key  : 4;
unsigned char reserved2  : 1;
unsigned char ili        : 1;
unsigned char reserved3  : 2;
byte info[4];
byte sense_len;
byte command_info[4];
byte asc;
byte ascq;
byte fru;
byte sense_key_specific[3];
};
struct packet_command {
unsigned char *buffer;
int buflen;
int stat;
struct atapi_request_sense *sense_data;
unsigned char c[12];
};
struct atapi_msf {
byte reserved;
byte minute;
byte second;
byte frame;
};
#define MAX_TRACKS 99
struct atapi_toc_header {
unsigned short toc_length;
byte first_track;
byte last_track;
};
struct atapi_toc_entry {
byte reserved1;
unsigned control : 4;
unsigned adr     : 4;
byte track;
byte reserved2;
union {
unsigned lba;
struct atapi_msf msf;
} addr;
};
struct atapi_toc {
int    last_session_lba;
int    xa_flag;
unsigned capacity;
struct atapi_toc_header hdr;
struct atapi_toc_entry  ent[MAX_TRACKS+1];
};
struct atapi_cdrom_subchnl
{
u_char  acdsc_reserved;
u_char  acdsc_audiostatus;
u_short acdsc_length;
u_char  acdsc_format;
u_char  acdsc_adr:	4;
u_char  acdsc_ctrl:	4;
u_char  acdsc_trk;
u_char  acdsc_ind;
union {
struct atapi_msf msf;
int	lba;
} acdsc_absaddr;
union {
struct atapi_msf msf;
int	lba;
} acdsc_reladdr;
};
struct cdrom_info {
struct atapi_toc *toc;
unsigned long sector_buffered;
unsigned long nsectors_buffered;
char *sector_buffer;
struct atapi_request_sense sense_data;
int max_sectors;
};
#endif
typedef enum {ide_disk, ide_cdrom, ide_tape, ide_floppy, ide_scsi} ide_media_t;
typedef union {
unsigned all			: 8;
struct {
unsigned set_geometry	: 1;
unsigned recalibrate	: 1;
unsigned set_multmode	: 1;
unsigned set_tune	: 1;
unsigned mc		: 1;
unsigned reserved	: 3;
} b;
} special_t;
typedef union {
unsigned all			: 8;
struct {
unsigned head		: 4;
unsigned unit		: 1;
unsigned bit5		: 1;
unsigned lba		: 1;
unsigned bit7		: 1;
} b;
} select_t;
typedef struct ide_drive_s {
special_t	special;
unsigned present	: 1;
unsigned noprobe 	: 1;
unsigned keep_settings  : 1;
unsigned busy		: 1;
unsigned removable	: 1;
unsigned using_dma	: 1;
unsigned forced_geom	: 1;
unsigned unmask		: 1;
unsigned no_unmask	: 1;
unsigned no_io_32bit	: 1;
unsigned nobios		: 1;
unsigned slow		: 1;
unsigned autotune	: 2;
unsigned nodma		: 1;
#if FAKE_FDISK_FOR_EZDRIVE
unsigned remap_0_to_1	: 1;
#endif
unsigned no_geom	: 1;
ide_media_t	media;
select_t	select;
byte		ctl;
byte		ready_stat;
byte		mult_count;
byte 		mult_req;
byte 		tune_req;
byte		io_32bit;
byte		bad_wstat;
byte		sect0;
byte 		usage;
byte 		head;
byte		sect;
byte		bios_head;
byte		bios_sect;
unsigned short	bios_cyl;
unsigned short	cyl;
void		  *hwif;
struct wait_queue *wqueue;
struct hd_driveid *id;
struct hd_struct  *part;
char		name[4];
#ifdef CONFIG_BLK_DEV_IDECD
struct cdrom_info cdrom_info;
#endif
#ifdef CONFIG_BLK_DEV_IDETAPE
idetape_tape_t	tape;
#endif
#ifdef CONFIG_BLK_DEV_IDEFLOPPY
void *floppy;
#endif
#ifdef CONFIG_BLK_DEV_IDESCSI
void *scsi;
#endif
byte		ide_scsi;
} ide_drive_t;
typedef enum {	ide_dma_read = 0,	ide_dma_write = 1,
ide_dma_abort = 2,	ide_dma_check = 3,
ide_dma_status_bad = 4,	ide_dma_transferred = 5,
ide_dma_begin = 6 }
ide_dma_action_t;
typedef int (ide_dmaproc_t)(ide_dma_action_t, ide_drive_t *);
typedef void (ide_tuneproc_t)(ide_drive_t *, byte);
typedef void (ide_selectproc_t) (ide_drive_t *);
typedef enum {	ide_unknown,	ide_generic,	ide_triton,
ide_cmd640,	ide_dtc2278,	ide_ali14xx,
ide_qd6580,	ide_umc8672,	ide_ht6560b,
ide_promise,	ide_hpt343,	ide_udma,
ide_ultra66 }
hwif_chipset_t;
typedef struct hwif_s {
struct hwif_s	*next;
void		*hwgroup;
unsigned short	io_base;
unsigned short	ctl_port;
ide_drive_t	drives[MAX_DRIVES];
struct gendisk	*gd;
ide_tuneproc_t	*tuneproc;
#if defined(CONFIG_BLK_DEV_HT6560B) || defined(CONFIG_BLK_DEV_PROMISE)
ide_selectproc_t *selectproc;
#endif
ide_dmaproc_t	*dmaproc;
unsigned long	*dmatable;
unsigned short	dma_base;
byte		irq;
byte		major;
char 		name[5];
byte		index;
hwif_chipset_t	chipset;
unsigned	noprobe    : 1;
unsigned	present    : 1;
unsigned	serialized : 1;
unsigned	sharing_irq: 1;
#ifdef CONFIG_BLK_DEV_PROMISE
unsigned	is_promise2: 1;
#endif
#if (DISK_RECOVERY_TIME > 0)
unsigned long	last_time;
#endif
#ifdef CONFIG_BLK_DEV_IDECD
struct request request_sense_request;
struct packet_command request_sense_pc;
#endif
#ifdef CONFIG_BLK_DEV_IDETAPE
ide_drive_t	*tape_drive;
#endif
} ide_hwif_t;
typedef void (ide_handler_t)(ide_drive_t *);
typedef struct hwgroup_s {
ide_handler_t		*handler;
ide_drive_t		*drive;
ide_hwif_t		*hwif;
ide_hwif_t		*next_hwif;
struct request		*rq;
struct timer_list	timer;
struct request		wrq;
unsigned long		poll_timeout;
int			active;
} ide_hwgroup_t;
#ifndef _IDE_C
extern	ide_hwif_t	ide_hwifs[];
#endif
#define IDE_DRIVER
#include <linux/blk.h>
#if (DISK_RECOVERY_TIME > 0)
void ide_set_recovery_timer (ide_hwif_t *);
#define SET_RECOVERY_TIMER(drive) ide_set_recovery_timer (drive)
#else
#define SET_RECOVERY_TIMER(drive)
#endif
void ide_input_data (ide_drive_t *drive, void *buffer, unsigned int wcount);
void ide_output_data (ide_drive_t *drive, void *buffer, unsigned int wcount);
void atapi_input_bytes (ide_drive_t *drive, void *buffer, unsigned int bytecount);
void atapi_output_bytes (ide_drive_t *drive, void *buffer, unsigned int bytecount);
void ide_set_handler (ide_drive_t *drive, ide_handler_t *handler, unsigned int timeout);
byte ide_dump_status (ide_drive_t *drive, const char *msg, byte stat);
void ide_error (ide_drive_t *drive, const char *msg, byte stat);
void ide_fixstring (byte *s, const int bytecount, const int byteswap);
int ide_wait_stat (ide_drive_t *drive, byte good, byte bad, unsigned long timeout);
int ide_xlate_1024 (kdev_t, int, const char *);
void ide_do_reset (ide_drive_t *);
void ide_init_drive_cmd (struct request *rq);
typedef enum
{ide_wait,
ide_next,
ide_preempt,
ide_end}
ide_action_t;
int ide_do_drive_cmd (ide_drive_t *drive, struct request *rq, ide_action_t action);
void ide_end_drive_cmd (ide_drive_t *drive, byte stat, byte err);
int ide_system_bus_speed (void);
void ide_multwrite (ide_drive_t *drive, unsigned int mcount);
#ifdef CONFIG_BLK_DEV_IDECD
void ide_do_rw_cdrom (ide_drive_t *, unsigned long);
int ide_cdrom_ioctl (ide_drive_t *, struct inode *, struct file *, unsigned int, unsigned long);
int ide_cdrom_check_media_change (ide_drive_t *);
int ide_cdrom_open (struct inode *, struct file *, ide_drive_t *);
void ide_cdrom_release (struct inode *, struct file *, ide_drive_t *);
void ide_cdrom_setup (ide_drive_t *);
#endif
#ifdef CONFIG_BLK_DEV_IDETAPE
int idetape_identify_device (ide_drive_t *drive,struct hd_driveid *id);
void idetape_setup (ide_drive_t *drive);
void idetape_do_request (ide_drive_t *drive, struct request *rq, unsigned long block);
void idetape_end_request (byte uptodate, ide_hwgroup_t *hwgroup);
int idetape_blkdev_ioctl (ide_drive_t *drive, struct inode *inode, struct file *file,
unsigned int cmd, unsigned long arg);
int idetape_blkdev_open (struct inode *inode, struct file *filp, ide_drive_t *drive);
void idetape_blkdev_release (struct inode *inode, struct file *filp, ide_drive_t *drive);
void idetape_register_chrdev (void);
#endif
#ifdef CONFIG_BLK_DEV_IDEFLOPPY
int idefloppy_identify_device (ide_drive_t *drive,struct hd_driveid *id);
void idefloppy_setup (ide_drive_t *drive);
void idefloppy_do_request (ide_drive_t *drive, struct request *rq, unsigned long block);
void idefloppy_end_request (byte uptodate, ide_hwgroup_t *hwgroup);
int idefloppy_ioctl (ide_drive_t *drive, struct inode *inode, struct file *file,
unsigned int cmd, unsigned long arg);
int idefloppy_open (struct inode *inode, struct file *filp, ide_drive_t *drive);
void idefloppy_release (struct inode *inode, struct file *filp, ide_drive_t *drive);
int idefloppy_media_change (ide_drive_t *drive);
unsigned long idefloppy_capacity (ide_drive_t *drive);
#endif
#ifdef CONFIG_BLK_DEV_IDESCSI
void idescsi_setup (ide_drive_t *drive);
void idescsi_do_request (ide_drive_t *drive, struct request *rq, unsigned long block);
void idescsi_end_request (byte uptodate, ide_hwgroup_t *hwgroup);
int idescsi_ioctl (ide_drive_t *drive, struct inode *inode, struct file *file, unsigned int cmd, unsigned long arg);
int idescsi_open (struct inode *inode, struct file *filp, ide_drive_t *drive);
void idescsi_ide_release (struct inode *inode, struct file *filp, ide_drive_t *drive);
#endif
#ifdef CONFIG_BLK_DEV_TRITON
void ide_init_triton (byte, byte);
void ide_init_promise (byte bus, byte fn, ide_hwif_t *hwif0, ide_hwif_t *hwif1, unsigned short dma);
#endif