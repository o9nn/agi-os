#ifndef _LINUX_GENHD_H
#define _LINUX_GENHD_H
#include <linux/config.h>
#define CONFIG_MSDOS_PARTITION 1
#ifdef __alpha__
#define CONFIG_OSF_PARTITION 1
#endif
#if defined(__sparc__) || defined(CONFIG_SMD_DISKLABEL)
#define CONFIG_SUN_PARTITION 1
#endif
#define DOS_EXTENDED_PARTITION 5
#define LINUX_EXTENDED_PARTITION 0x85
#define WIN98_EXTENDED_PARTITION 0x0f
#define DM6_PARTITION 0x54
#define EZD_PARTITION 0x55
#define DM6_AUX1PARTITION 0x51
#define DM6_AUX3PARTITION 0x53
struct partition {
unsigned char boot_ind;
unsigned char head;
unsigned char sector;
unsigned char cyl;
unsigned char sys_ind;
unsigned char end_head;
unsigned char end_sector;
unsigned char end_cyl;
unsigned int start_sect;
unsigned int nr_sects;
} __attribute((packed));
struct hd_struct {
long start_sect;
long nr_sects;
};
struct gendisk {
int major;
const char *major_name;
int minor_shift;
int max_p;
int max_nr;
void (*init)(struct gendisk *);
struct hd_struct *part;
int *sizes;
int nr_real;
void *real_devices;
struct gendisk *next;
};
#ifdef CONFIG_BSD_DISKLABEL
#define BSD_PARTITION 0xa5
#define BSD_DISKMAGIC (0x82564557UL)
#define BSD_MAXPARTITIONS 8
#define BSD_FS_UNUSED 0
struct bsd_disklabel {
__u32 d_magic;
__s16 d_type;
__s16 d_subtype;
char d_typename[16];
char d_packname[16];
__u32 d_secsize;
__u32 d_nsectors;
__u32 d_ntracks;
__u32 d_ncylinders;
__u32 d_secpercyl;
__u32 d_secperunit;
__u16 d_sparespertrack;
__u16 d_sparespercyl;
__u32 d_acylinders;
__u16 d_rpm;
__u16 d_interleave;
__u16 d_trackskew;
__u16 d_cylskew;
__u32 d_headswitch;
__u32 d_trkseek;
__u32 d_flags;
#define NDDATA 5
__u32 d_drivedata[NDDATA];
#define NSPARE 5
__u32 d_spare[NSPARE];
__u32 d_magic2;
__u16 d_checksum;
__u16 d_npartitions;
__u32 d_bbsize;
__u32 d_sbsize;
struct bsd_partition {
__u32 p_size;
__u32 p_offset;
__u32 p_fsize;
__u8 p_fstype;
__u8 p_frag;
__u16 p_cpg;
} d_partitions[BSD_MAXPARTITIONS];
};
#endif
extern struct gendisk *gendisk_head;
char *disk_name (struct gendisk *hd, int minor, char *buf);
#endif