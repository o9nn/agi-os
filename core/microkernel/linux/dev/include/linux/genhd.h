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
#ifdef MACH_INCLUDE
struct linux_partition
{
#else
struct partition {
#endif
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
#ifdef CONFIG_GPT_DISKLABEL
#define GPT_PARTITION 0xee
#define GPT_GUID_SIZE 16
struct gpt_guid {
__u32 g_time_low;
__u16 g_time_mid;
__u16 g_time_high_version;
__u8 g_clock_sec_high;
__u8 g_clock_sec_low;
__u8 g_node_id[6];
} __attribute((packed));
typedef char __gpt_guid_right_size[(sizeof(struct gpt_guid) == GPT_GUID_SIZE) ? 1 : -1];
static const struct gpt_guid GPT_GUID_TYPE_UNUSED = {0,0,0,0,0,{0,0,0,0,0,0}};
#define GPT_SIGNATURE "EFI PART"
#define GPT_REVISION (0x00010000UL)
#define GPT_HEADER_SIZE 92
#define GPT_MAXPARTITIONS 128
struct gpt_disklabel_header {
char h_signature[8];
__u32 h_revision;
__u32 h_header_size;
__u32 h_header_crc;
__u32 h_reserved;
__u64 h_lba_current;
__u64 h_lba_backup;
__u64 h_lba_usable_first;
__u64 h_lba_usable_last;
struct gpt_guid h_guid;
__u64 h_part_table_lba;
__u32 h_part_table_len;
__u32 h_part_table_entry_size;
__u32 h_part_table_crc;
} __attribute((packed));
typedef char __gpt_header_right_size[(sizeof(struct gpt_disklabel_header) == GPT_HEADER_SIZE) ? 1 : -1];
#define GPT_PARTITION_ATTR_PLATFORM_REQUIRED (1ULL << 0)
#define GPT_PARTITION_ATTR_EFI_IGNORE (1ULL << 1)
#define GPT_PARTITION_ATTR_BIOS_BOOTABLE (1ULL << 2)
#define GPT_PARTITION_ENTRY_SIZE 128
struct gpt_disklabel_part {
struct gpt_guid p_type;
struct gpt_guid p_guid;
__u64 p_lba_first;
__u64 p_lba_last;
__u64 p_attrs;
__u16 p_name[36];
} __attribute((packed));
typedef char __gpt_part_entry_right_size[(sizeof(struct gpt_disklabel_part) == GPT_PARTITION_ENTRY_SIZE) ? 1 : -1];
#endif
extern struct gendisk *gendisk_head;
char *disk_name (struct gendisk *hd, int minor, char *buf);
#endif