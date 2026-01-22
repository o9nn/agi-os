#ifndef FAT_H
#define FAT_H
enum fat { FAT12, FAT16, FAT32 };
typedef enum fat fat_t;
#define FAT12_MAX_NR_OF_CLUSTERS 4084
#define FAT16_MAX_NR_OF_CLUSTERS 65524
#define FAT32_MAX_NR_OF_CLUSTERS (FAT32_BAD_CLUSTER - 1)
struct boot_sector
{
unsigned char jump_to_boot_code[3];
unsigned char oem_name[8];
unsigned char bytes_per_sector[2];
unsigned char sectors_per_cluster;
unsigned char reserved_sectors[2];
unsigned char nr_of_fat_tables;
unsigned char nr_of_root_dirents[2];
unsigned char total_sectors_16[2];
unsigned char media_descriptor;
unsigned char sectors_per_fat_16[2];
unsigned char sectors_per_track[2];
unsigned char nr_of_heads[2];
unsigned char nr_of_hidden_sectors[4];
unsigned char total_sectors_32[4];
union
{
struct
{
unsigned char drive;
unsigned char reserved;
unsigned char boot_signature;
unsigned char serial[4];
unsigned char label[11];
unsigned char fs_type[8];
} fat;
struct
{
unsigned char sectors_per_fat_32[4];
unsigned char extension_flags[2];
unsigned char fs_version[2];
unsigned char root_cluster[4];
unsigned char fs_info_sector[2];
unsigned char backup_boot_sector[2];
unsigned char reserved1[12];
unsigned char drive_number;
unsigned char reserved2;
unsigned char boot_signature;
unsigned char serial[4];
unsigned char label[11];
unsigned char fs_type[8];
} fat32;
} compat;
unsigned char unused[420];
#define BOOT_SECTOR_ID 0xaa55
unsigned char id[2];
};
#define FAT_FS_INFO_LEAD_SIGNATURE		0x41615252L
#define FAT_FS_INFO_STRUCT_SIGNATURE		0x61417272L
#define FAT_FS_INFO_TRAIL_SIGNAURE		0xaa550000L
#define FAT_FS_NR_OF_FREE_CLUSTERS_UNKNOWN	0xffffffffL
#define FAT_FS_NEXT_FREE_CLUSTER_UNKNOWN	0xffffffffL
struct fat_fs_info
{
unsigned char lead_signature[4];
unsigned char reserved1[480];
unsigned char struct_signature[4];
unsigned char nr_of_free_clusters[4];
unsigned char next_free_cluster[4];
unsigned char reserved2[12];
unsigned char trail_signature[4];
};
#define FAT_FREE_CLUSTER	0
#define FAT12_BAD_CLUSTER	0x0ff7
#define FAT16_BAD_CLUSTER	0xfff7
#define FAT32_BAD_CLUSTER	0x0ffffff7L
#define FAT_BAD_CLUSTER		FAT32_BAD_CLUSTER
#define FAT12_EOC	0x0ff8
#define FAT16_EOC	0xfff8
#define FAT32_EOC	0x0ffffff8
#define FAT_EOC		FAT32_EOC
#define FAT_DIR_REC_LEN		32
#define FAT_DIR_RECORDS(x)	FAT_DIR_REC_LEN
#define FAT_DIR_ATTR_RDONLY	0x01
#define FAT_DIR_ATTR_HIDDEN	0x02
#define FAT_DIR_ATTR_SYSTEM	0x04
#define FAT_DIR_ATTR_LABEL	0x08
#define FAT_DIR_ATTR_DIR	0x10
#define FAT_DIR_ATTR_ARCHIVE	0x20
#define FAT_DIR_ATTR_LONGNAME	(DIR_ATTR_RDONLY | DIR_ATTR_HIDDEN \
| DIR_ATTR_SYSTEM | DIR_ATTR_LABEL)
#define FAT_DIR_NAME_LAST	'\x00'
#define FAT_DIR_NAME_DELETED	'\xe5'
#define FAT_DIR_NAME_REPLACE_DELETED '\x05'
#define FAT_DIR_NAME_DOT	".          "
#define FAT_DIR_NAME_DOTDOT	"..         "
struct dirrect
{
unsigned char name[11];
unsigned char attribute;
unsigned char reserved;
unsigned char creation_time_centiseconds;
unsigned char creation_time[2];
unsigned char creation_date[2];
unsigned char last_access_date[2];
unsigned char first_cluster_high[2];
unsigned char write_time[2];
unsigned char write_date[2];
unsigned char first_cluster_low[2];
unsigned char file_size[4];
};
#define FAT_NAME_MAX 12
extern vm_offset_t first_data_byte;
extern size_t bytes_per_cluster;
typedef unsigned long cluster_t;
#define LOG2_CLUSTERS_PER_TABLE 10
#define CLUSTERS_PER_TABLE (1 << LOG2_CLUSTERS_PER_TABLE)
struct cluster_chain
{
struct cluster_chain *next;
cluster_t cluster[CLUSTERS_PER_TABLE];
};
void fat_read_sblock (void);
void fat_to_epoch (unsigned char *, unsigned char *, struct timespec *);
void fat_from_epoch (unsigned char *, unsigned char *, time_t *);
error_t fat_getcluster (struct node *, cluster_t, int, cluster_t *);
void fat_truncate_node (struct node *, cluster_t);
error_t fat_extend_chain (struct node *, cluster_t, int);
int fat_get_freespace (void);
extern struct boot_sector *sblock;
extern fat_t fat_type;
extern size_t bytes_per_sector;
extern size_t log2_bytes_per_sector;
extern size_t sectors_per_cluster;
extern size_t bytes_per_cluster;
extern unsigned int log2_bytes_per_cluster;
extern size_t sectors_per_fat;
extern size_t total_sectors;
extern size_t nr_of_root_dir_sectors;
extern size_t first_root_dir_byte;
extern size_t first_data_sector;
extern vm_offset_t first_data_byte;
extern size_t first_fat_sector;
extern cluster_t nr_of_clusters;
#include <endian.h>
#include <byteswap.h>
static inline unsigned int
read_dword (unsigned char *addr)
{
#if BYTE_ORDER == LITTLE_ENDIAN
return *(unsigned int *)addr;
#elif BYTE_ORDER == BIG_ENDIAN
return bswap_32 (*(unsigned int *) addr);
#else
#error unknown byte order
#endif
}
static inline unsigned int
read_word (unsigned char *addr)
{
#if BYTE_ORDER == LITTLE_ENDIAN
return *(unsigned short *)addr;
#elif BYTE_ORDER == BIG_ENDIAN
return bswap_16 (*(unsigned int *) addr);
#else
#error unknown byte order
#endif
}
static inline void
write_dword (unsigned char *addr, unsigned int value)
{
#if BYTE_ORDER == LITTLE_ENDIAN
*(unsigned int *)addr = value;
#elif BYTE_ORDER == BIG_ENDIAN
*(unsigned int *)addr = bswap_32 (value);
#else
#error unknown byte order
#endif
}
static inline void
write_word (unsigned char *addr, unsigned int value)
{
#if BYTE_ORDER == LITTLE_ENDIAN
*(unsigned short *)addr = value;
#elif BYTE_ORDER == BIG_ENDIAN
*(unsigned int *)addr = bswap_16 (value);
#else
#error unknown byte order
#endif
}
#endif