#ifndef _LINUX_NTFS_FS_SB_H
#define _LINUX_NTFS_FS_SB_H
struct ntfs_sb_info{
ntfs_uid_t uid;
ntfs_gid_t gid;
ntmode_t umask;
unsigned int nct;
void *nls_map;
unsigned int ngt;
ntfs_size_t partition_bias;
ntfs_u32 at_standard_information;
ntfs_u32 at_attribute_list;
ntfs_u32 at_file_name;
ntfs_u32 at_security_descriptor;
ntfs_u32 at_data;
ntfs_u32 at_index_root;
ntfs_u32 at_index_allocation;
ntfs_u32 at_bitmap;
ntfs_u32 at_symlink;
int blocksize;
int clusterfactor;
int clustersize;
int mft_recordsize;
int mft_clusters_per_record;
int index_recordsize;
int index_clusters_per_record;
int mft_cluster;
unsigned char *mft;
unsigned short *upcase;
unsigned int upcase_length;
struct ntfs_inode_info *mft_ino;
struct ntfs_inode_info *mftmirr;
struct ntfs_inode_info *bitmap;
struct super_block *sb;
};
#endif