typedef struct Xfs Xfs;
typedef struct Xfile Xfile;
typedef struct Iobuf Iobuf;
typedef struct Ext2 Ext2;
typedef struct SuperBlock SuperBlock;
typedef struct GroupDesc GroupDesc;
typedef struct Inode Inode;
typedef struct DirEntry DirEntry;
#define SECTORSIZE 512
#define OFFSET_SUPER_BLOCK 1024
#define EXT2_SUPER_MAGIC 0xEF53
#define EXT2_MIN_BLOCK_SIZE 1024
#define EXT2_MAX_BLOCK_SIZE 4096
#define EXT2_ROOT_INODE 2
#define EXT2_FIRST_INO 11
#define EXT2_VALID_FS 0x0001
#define EXT2_ERROR_FS 0x0002
struct SuperBlock {
uint s_inodes_count;
uint s_blocks_count;
uint s_r_blocks_count;
uint s_free_blocks_count;
uint s_free_inodes_count;
uint s_first_data_block;
uint s_log_block_size;
int s_log_frag_size;
uint s_blocks_per_group;
uint s_frags_per_group;
uint s_inodes_per_group;
uint s_mtime;
uint s_wtime;
ushort s_mnt_count;
short s_max_mnt_count;
ushort s_magic;
ushort s_state;
ushort s_errors;
ushort s_pad;
uint s_lastcheck;
uint s_checkinterval;
uint s_creator_os;
uint s_rev_level;
ushort s_def_resuid;
ushort s_def_resgid;
uint s_reserved[235];
};
struct GroupDesc
{
uint bg_block_bitmap;
uint bg_inode_bitmap;
uint bg_inode_table;
ushort bg_free_blocks_count;
ushort bg_free_inodes_count;
ushort bg_used_dirs_count;
ushort bg_pad;
uint bg_reserved[3];
};
#define EXT2_NDIR_BLOCKS 12
#define EXT2_IND_BLOCK EXT2_NDIR_BLOCKS
#define EXT2_DIND_BLOCK (EXT2_IND_BLOCK + 1)
#define EXT2_TIND_BLOCK (EXT2_DIND_BLOCK + 1)
#define EXT2_N_BLOCKS (EXT2_TIND_BLOCK + 1)
struct Inode {
ushort i_mode;
ushort i_uid;
uint i_size;
uint i_atime;
uint i_ctime;
uint i_mtime;
uint i_dtime;
ushort i_gid;
ushort i_links_count;
uint i_blocks;
uint i_flags;
uint osd1;
uint i_block[EXT2_N_BLOCKS];
uint i_version;
uint i_file_acl;
uint i_dir_acl;
uint i_faddr;
uchar osd2[12];
};
#define EXT2_NAME_LEN 255
#define DIR_REC_LEN(name_len) (((name_len) + 8 + 3) & ~3)
struct DirEntry {
uint inode;
ushort rec_len;
uchar name_len;
uchar reserved;
char name[EXT2_NAME_LEN];
};
#define S_IFMT 00170000
#define S_IFLNK 0120000
#define S_IFREG 0100000
#define S_IFDIR 0040000
#define S_ISLNK(m) (((m) & S_IFMT) == S_IFLNK)
#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
#define DEFAULT_UID 200
#define DEFAULT_GID 100
struct Iobuf
{
Xfs *dev;
long addr;
Iobuf *next;
Iobuf *prev;
Iobuf *hash;
int busy;
int dirty;
char *iobuf;
};
struct Xfs{
Xfs *next;
char *name;
Qid qid;
long ref;
Qid rootqid;
short dev;
short fmt;
void *ptr;
int block_size;
int desc_per_block;
int inodes_per_group;
int inodes_per_block;
int addr_per_block;
int blocks_per_group;
int ngroups;
int superaddr, superoff;
int grpaddr;
};
struct Xfile{
Xfile *next;
long client;
long fid;
Xfs * xf;
void * ptr;
uint inbr;
uint pinbr;
ulong bufaddr;
ulong bufoffset;
int root;
int dirindex;
};
#define EXT2_SUPER 1
#define EXT2_DESC 2
#define EXT2_BBLOCK 3
#define EXT2_BINODE 4
struct Ext2{
char type;
union{
SuperBlock *sb;
GroupDesc *gd;
char *bmp;
}u;
Iobuf *buf;
};
#define DESC_ADDR(xf,n) ( (xf)->grpaddr + ((n)/(xf)->desc_per_block) )
#define DESC_OFFSET(xf,d,n) ( ((GroupDesc *)(d)) + ((n)%(xf)->desc_per_block) )
enum{
Asis, Clean, Clunk
};
enum{
Enevermind,
Eformat,
Eio,
Enomem,
Enonexist,
Eexist,
Eperm,
Enofilsys,
Eauth,
Enospace,
Elink,
Elongname,
Eintern,
Ecorrupt,
Enotclean
};
extern int chatty;
extern int errno;
extern char *deffile;
extern int rdonly;