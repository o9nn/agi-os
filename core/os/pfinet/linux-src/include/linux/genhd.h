#ifndef _LINUX_GENHD_H
#define _LINUX_GENHD_H
#include <linux/config.h>
#include <linux/types.h>
#include <asm/unaligned.h>
#define CONFIG_MSDOS_PARTITION 1
#ifdef __alpha__
#define CONFIG_OSF_PARTITION 1
#endif
#if defined(__sparc__) || defined(CONFIG_SMD_DISKLABEL)
#define CONFIG_SUN_PARTITION 1
#endif
#if defined(CONFIG_SGI)
#define CONFIG_SGI_PARTITION 1
#endif
#define DOS_EXTENDED_PARTITION 5
#define LINUX_EXTENDED_PARTITION 0x85
#define WIN98_EXTENDED_PARTITION 0x0f
#define LINUX_SWAP_PARTITION		0x82
#define LINUX_RAID_PARTITION		0xfd
#define LINUX_OLD_RAID_PARTITION	0x86
#ifdef CONFIG_SOLARIS_X86_PARTITION
#define SOLARIS_X86_PARTITION	LINUX_SWAP_PARTITION
#endif
#define DM6_PARTITION		0x54
#define EZD_PARTITION		0x55
#define DM6_AUX1PARTITION	0x51
#define DM6_AUX3PARTITION	0x53
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
} __attribute__((packed));
struct hd_struct {
long start_sect;
long nr_sects;
int type;
};
static inline unsigned int ptype (unsigned char raw_type)
{
switch (raw_type) {
case LINUX_OLD_RAID_PARTITION:
return LINUX_OLD_RAID_PARTITION;
case LINUX_RAID_PARTITION:
return LINUX_RAID_PARTITION;
default:
}
return 0;
}
#define MAX_DISKNAME_LEN 32
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
#ifdef CONFIG_SOLARIS_X86_PARTITION
#define SOLARIS_X86_NUMSLICE	8
#define SOLARIS_X86_VTOC_SANE	(0x600DDEEEUL)
struct solaris_x86_slice {
ushort	s_tag;
ushort	s_flag;
daddr_t s_start;
long	s_size;
};
struct solaris_x86_vtoc {
unsigned long v_bootinfo[3];
unsigned long v_sanity;
unsigned long v_version;
char	v_volume[8];
ushort	v_sectorsz;
ushort	v_nparts;
unsigned long v_reserved[10];
struct solaris_x86_slice
v_slice[SOLARIS_X86_NUMSLICE];
time_t	timestamp[SOLARIS_X86_NUMSLICE];
char	v_asciilabel[128];
};
#endif
#ifdef CONFIG_BSD_DISKLABEL
#define FREEBSD_PARTITION	0xa5
#define OPENBSD_PARTITION	0xa6
#define NETBSD_PARTITION	0xa9
#define BSDI_PARTITION		0xb7
#define BSD_PARTITION		FREEBSD_PARTITION
#define BSD_DISKMAGIC	(0x82564557UL)
#define BSD_MAXPARTITIONS	8
#define OPENBSD_MAXPARTITIONS	16
#define BSD_FS_UNUSED		0
struct bsd_disklabel {
__u32	d_magic;
__s16	d_type;
__s16	d_subtype;
char	d_typename[16];
char	d_packname[16];
__u32	d_secsize;
__u32	d_nsectors;
__u32	d_ntracks;
__u32	d_ncylinders;
__u32	d_secpercyl;
__u32	d_secperunit;
__u16	d_sparespertrack;
__u16	d_sparespercyl;
__u32	d_acylinders;
__u16	d_rpm;
__u16	d_interleave;
__u16	d_trackskew;
__u16	d_cylskew;
__u32	d_headswitch;
__u32	d_trkseek;
__u32	d_flags;
#define NDDATA 5
__u32	d_drivedata[NDDATA];
#define NSPARE 5
__u32	d_spare[NSPARE];
__u32	d_magic2;
__u16	d_checksum;
__u16	d_npartitions;
__u32	d_bbsize;
__u32	d_sbsize;
struct	bsd_partition {
__u32	p_size;
__u32	p_offset;
__u32	p_fsize;
__u8	p_fstype;
__u8	p_frag;
__u16	p_cpg;
} d_partitions[BSD_MAXPARTITIONS];
};
#endif
#ifdef CONFIG_UNIXWARE_DISKLABEL
#define UNIXWARE_PARTITION     0x63
#define UNIXWARE_DISKMAGIC     (0xCA5E600DUL)
#define UNIXWARE_DISKMAGIC2    (0x600DDEEEUL)
#define UNIXWARE_NUMSLICE      16
#define UNIXWARE_FS_UNUSED     0
struct unixware_slice {
__u16   s_label;
__u16   s_flags;
__u32   start_sect;
__u32   nr_sects;
};
struct unixware_disklabel {
__u32   d_type;
__u32   d_magic;
__u32   d_version;
char    d_serial[12];
__u32   d_ncylinders;
__u32   d_ntracks;
__u32   d_nsectors;
__u32   d_secsize;
__u32   d_part_start;
__u32   d_unknown1[12];
__u32	d_alt_tbl;
__u32	d_alt_len;
__u32	d_phys_cyl;
__u32	d_phys_trk;
__u32	d_phys_sec;
__u32	d_phys_bytes;
__u32	d_unknown2;
__u32   d_unknown3;
__u32	d_pad[8];
struct unixware_vtoc {
__u32	v_magic;
__u32	v_version;
char	v_name[8];
__u16	v_nslices;
__u16	v_unknown1;
__u32	v_reserved[10];
struct unixware_slice
v_slice[UNIXWARE_NUMSLICE];
} vtoc;
};
#endif
extern struct gendisk *gendisk_head;
char *disk_name (struct gendisk *hd, int minor, char *buf);
#endif