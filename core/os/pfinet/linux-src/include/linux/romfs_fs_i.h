#ifndef __ROMFS_FS_I
#define __ROMFS_FS_I
struct romfs_inode_info {
unsigned long i_metasize;
unsigned long i_dataoffset;
};
#endif