#ifndef LINUX_UCDROM_H
#define LINUX_UCDROM_H
#ifdef __KERNEL__
struct cdrom_device_ops {
int (*open) (kdev_t, int);
void (*release) (kdev_t);
int (*open_files) (kdev_t);
int (*drive_status) (kdev_t);
int (*disc_status) (kdev_t);
int (*media_changed) (kdev_t);
int (*tray_move) (kdev_t, int);
int (*lock_door) (kdev_t, int);
int (*select_speed) (kdev_t, int);
int (*select_disc) (kdev_t, int);
int (*get_last_session) (kdev_t, struct cdrom_multisession *);
int (*get_mcn) (kdev_t, struct cdrom_mcn *);
int (*reset) (kdev_t dev);
int (*audio_ioctl) (kdev_t, unsigned int, void *);
int (*dev_ioctl) (kdev_t, unsigned int, unsigned long);
const int capability;
int mask;
const int speed;
const int minors;
const int capacity;
int options;
long mc_flags;
};
#endif
#define CDC_CLOSE_TRAY	0x1
#define CDC_OPEN_TRAY	0x2
#define CDC_LOCK	0x4
#define CDC_SELECT_SPEED 0x8
#define CDC_SELECT_DISC	0x10
#define CDC_MULTI_SESSION 0x20
#define CDC_MCN		0x40
#define CDC_MEDIA_CHANGED 0x80
#define CDC_PLAY_AUDIO	0x100
#define CDS_NO_INFO	0
#define CDS_NO_DISC	1
#define CDS_TRAY_OPEN	2
#define CDS_DRIVE_NOT_READY	3
#define CDS_DISC_OK	4
#define CDS_AUDIO	100
#define CDS_DATA_1	101
#define CDS_DATA_2	102
#define CDS_XA_2_1	103
#define CDS_XA_2_2	104
#define CDO_AUTO_CLOSE	0x1
#define CDO_AUTO_EJECT	0x2
#define CDO_USE_FFLAGS	0x4
#define CDO_LOCK	0x8
#define CDO_CHECK_TYPE	0x10
#define CDROM_SET_OPTIONS	0x5320
#define CDROM_CLEAR_OPTIONS	0x5321
#define CDROM_SELECT_SPEED	0x5322
#define CDROM_SELECT_DISC	0x5323
#define CDROM_MEDIA_CHANGED	0x5325
#define CDROM_DRIVE_STATUS	0x5326
#define CDROM_DISC_STATUS	0x5327
#define CDROM_GET_MCN	CDROM_GET_UPC
#ifdef __KERNEL__
extern struct file_operations cdrom_fops;
extern int register_cdrom(int major, char *name,
struct cdrom_device_ops *cdo);
extern int unregister_cdrom(int major, char *name);
#endif
#endif