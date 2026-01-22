#ifndef _LINUX_CDROM_H
#define _LINUX_CDROM_H
#define EDRIVE_CANT_DO_THIS EOPNOTSUPP
#define CDROMPAUSE 0x5301
#define CDROMRESUME 0x5302
#define CDROMPLAYMSF 0x5303
#define CDROMPLAYTRKIND 0x5304
#define CDROMREADTOCHDR 0x5305
#define CDROMREADTOCENTRY 0x5306
#define CDROMSTOP 0x5307
#define CDROMSTART 0x5308
#define CDROMEJECT 0x5309
#define CDROMVOLCTRL 0x530a
#define CDROMSUBCHNL 0x530b
#define CDROMREADMODE2 0x530c
#define CDROMREADMODE1 0x530d
#define CDROMREADAUDIO 0x530e
#define CDROMEJECT_SW 0x530f
#define CDROMMULTISESSION 0x5310
#define CDROM_GET_MCN 0x5311
#define CDROM_GET_UPC CDROM_GET_MCN
#define CDROMRESET 0x5312
#define CDROMVOLREAD 0x5313
#define CDROMREADRAW 0x5314
#define CDROMREADCOOKED 0x5315
#define CDROMSEEK 0x5316
#define CDROMPLAYBLK 0x5317
#define CDROMREADALL 0x5318
#define CDROMCLOSETRAY 0x5319
#define CDROM_SET_OPTIONS 0x5320
#define CDROM_CLEAR_OPTIONS 0x5321
#define CDROM_SELECT_SPEED 0x5322
#define CDROM_SELECT_DISC 0x5323
#define CDROM_MEDIA_CHANGED 0x5325
#define CDROM_DRIVE_STATUS 0x5326
#define CDROM_DISC_STATUS 0x5327
#define CDROM_CHANGER_NSLOTS 0x5328
#define CDROM_LOCKDOOR 0x5329
#define CDROM_DEBUG 0x5330
#define CDROM_GET_CAPABILITY 0x5331
#define CDROMAUDIOBUFSIZ 0x5382
struct cdrom_msf0
{
u_char minute;
u_char second;
u_char frame;
};
union cdrom_addr
{
struct cdrom_msf0 msf;
int lba;
};
struct cdrom_msf
{
u_char cdmsf_min0;
u_char cdmsf_sec0;
u_char cdmsf_frame0;
u_char cdmsf_min1;
u_char cdmsf_sec1;
u_char cdmsf_frame1;
};
struct cdrom_ti
{
u_char cdti_trk0;
u_char cdti_ind0;
u_char cdti_trk1;
u_char cdti_ind1;
};
struct cdrom_tochdr
{
u_char cdth_trk0;
u_char cdth_trk1;
};
struct cdrom_volctrl
{
u_char channel0;
u_char channel1;
u_char channel2;
u_char channel3;
};
struct cdrom_subchnl
{
u_char cdsc_format;
u_char cdsc_audiostatus;
u_char cdsc_adr: 4;
u_char cdsc_ctrl: 4;
u_char cdsc_trk;
u_char cdsc_ind;
union cdrom_addr cdsc_absaddr;
union cdrom_addr cdsc_reladdr;
};
struct cdrom_tocentry
{
u_char cdte_track;
u_char cdte_adr :4;
u_char cdte_ctrl :4;
u_char cdte_format;
union cdrom_addr cdte_addr;
u_char cdte_datamode;
};
struct cdrom_read
{
int cdread_lba;
caddr_t cdread_bufaddr;
int cdread_buflen;
};
struct cdrom_read_audio
{
union cdrom_addr addr;
u_char addr_format;
int nframes;
u_char *buf;
};
struct cdrom_multisession
{
union cdrom_addr addr;
u_char xa_flag;
u_char addr_format;
};
struct cdrom_mcn
{
u_char medium_catalog_number[14];
};
struct cdrom_blk
{
unsigned from;
unsigned short len;
};
#define CD_MINS 74
#define CD_SECS 60
#define CD_FRAMES 75
#define CD_SYNC_SIZE 12
#define CD_MSF_OFFSET 150
#define CD_CHUNK_SIZE 24
#define CD_NUM_OF_CHUNKS 98
#define CD_FRAMESIZE_SUB 96
#define CD_HEAD_SIZE 4
#define CD_SUBHEAD_SIZE 8
#define CD_EDC_SIZE 4
#define CD_ZERO_SIZE 8
#define CD_ECC_SIZE 276
#define CD_FRAMESIZE 2048
#define CD_FRAMESIZE_RAW 2352
#define CD_FRAMESIZE_RAWER 2646
#define CD_FRAMESIZE_RAW1 (CD_FRAMESIZE_RAW-CD_SYNC_SIZE)
#define CD_FRAMESIZE_RAW0 (CD_FRAMESIZE_RAW-CD_SYNC_SIZE-CD_HEAD_SIZE)
#define CD_XA_HEAD (CD_HEAD_SIZE+CD_SUBHEAD_SIZE)
#define CD_XA_TAIL (CD_EDC_SIZE+CD_ECC_SIZE)
#define CD_XA_SYNC_HEAD (CD_SYNC_SIZE+CD_XA_HEAD)
#define CDROM_LBA 0x01
#define CDROM_MSF 0x02
#define CDROM_DATA_TRACK 0x04
#define CDROM_LEADOUT 0xAA
#define CDROM_AUDIO_INVALID 0x00
#define CDROM_AUDIO_PLAY 0x11
#define CDROM_AUDIO_PAUSED 0x12
#define CDROM_AUDIO_COMPLETED 0x13
#define CDROM_AUDIO_ERROR 0x14
#define CDROM_AUDIO_NO_STATUS 0x15
#define SCMD_READ_TOC 0x43
#define SCMD_PLAYAUDIO_MSF 0x47
#define SCMD_PLAYAUDIO_TI 0x48
#define SCMD_PAUSE_RESUME 0x4B
#define SCMD_READ_SUBCHANNEL 0x42
#define SCMD_PLAYAUDIO10 0x45
#define CDC_CLOSE_TRAY 0x1
#define CDC_OPEN_TRAY 0x2
#define CDC_LOCK 0x4
#define CDC_SELECT_SPEED 0x8
#define CDC_SELECT_DISC 0x10
#define CDC_MULTI_SESSION 0x20
#define CDC_MCN 0x40
#define CDC_MEDIA_CHANGED 0x80
#define CDC_PLAY_AUDIO 0x100
#define CDC_RESET 0x200
#define CDC_IOCTLS 0x400
#define CDC_DRIVE_STATUS 0x800
#define CDS_NO_INFO 0
#define CDS_NO_DISC 1
#define CDS_TRAY_OPEN 2
#define CDS_DRIVE_NOT_READY 3
#define CDS_DISC_OK 4
#define CDS_AUDIO 100
#define CDS_DATA_1 101
#define CDS_DATA_2 102
#define CDS_XA_2_1 103
#define CDS_XA_2_2 104
#define CDS_MIXED 105
#define CDO_AUTO_CLOSE 0x1
#define CDO_AUTO_EJECT 0x2
#define CDO_USE_FFLAGS 0x4
#define CDO_LOCK 0x8
#define CDO_CHECK_TYPE 0x10
#define CDSL_NONE ((int) (~0U>>1)-1)
#define CDSL_CURRENT ((int) (~0U>>1))
#ifdef __KERNEL__
struct cdrom_device_info {
struct cdrom_device_ops *ops;
struct cdrom_device_info *next;
void *handle;
kdev_t dev;
int mask;
int speed;
int capacity;
int options : 30;
unsigned mc_flags : 2;
int use_count;
char name[20];
};
struct cdrom_device_ops {
int (*open) (struct cdrom_device_info *, int);
void (*release) (struct cdrom_device_info *);
int (*drive_status) (struct cdrom_device_info *, int);
int (*media_changed) (struct cdrom_device_info *, int);
int (*tray_move) (struct cdrom_device_info *, int);
int (*lock_door) (struct cdrom_device_info *, int);
int (*select_speed) (struct cdrom_device_info *, int);
int (*select_disc) (struct cdrom_device_info *, int);
int (*get_last_session) (struct cdrom_device_info *,
struct cdrom_multisession *);
int (*get_mcn) (struct cdrom_device_info *,
struct cdrom_mcn *);
int (*reset) (struct cdrom_device_info *);
int (*audio_ioctl) (struct cdrom_device_info *,unsigned int, void *);
int (*dev_ioctl) (struct cdrom_device_info *,
unsigned int, unsigned long);
const int capability;
int n_minors;
};
extern struct file_operations cdrom_fops;
extern int register_cdrom(struct cdrom_device_info *cdi);
extern int unregister_cdrom(struct cdrom_device_info *cdi);
typedef struct {
int data;
int audio;
int cdi;
int xa;
long error;
} tracktype;
extern void cdrom_count_tracks(struct cdrom_device_info *cdi,tracktype* tracks);
#endif
#endif