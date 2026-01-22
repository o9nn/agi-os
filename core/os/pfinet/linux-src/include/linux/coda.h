#ifndef _CODA_HEADER_
#define _CODA_HEADER_
#ifdef __NetBSD__
#include <sys/types.h>
#endif
#ifndef CODA_MAXSYMLINKS
#define CODA_MAXSYMLINKS 10
#endif
#if defined(DJGPP) || defined(__CYGWIN32__)
#ifdef KERNEL
typedef unsigned long u_long;
typedef unsigned int u_int;
typedef unsigned short u_short;
typedef u_long ino_t;
typedef u_long dev_t;
typedef void * caddr_t;
#ifdef DOS
typedef unsigned __int64 u_quad_t;
#else
typedef unsigned long long u_quad_t;
#endif
#define inline
struct timespec {
long ts_sec;
long ts_nsec;
};
#else
#include <sys/types.h>
#include <sys/time.h>
typedef unsigned long long u_quad_t;
#endif
#endif
#if defined(__linux__)
#define cdev_t u_quad_t
#ifndef __KERNEL__
#if !defined(_UQUAD_T_) && (!defined(__GLIBC__) || __GLIBC__ < 2)
#define _UQUAD_T_ 1
typedef unsigned long long u_quad_t;
#endif
#else
typedef unsigned long long u_quad_t;
#endif
#else
#define cdev_t dev_t
#endif
#ifdef __CYGWIN32__
struct timespec {
time_t tv_sec;
long tv_nsec;
};
#endif
#ifndef __BIT_TYPES_DEFINED__
#define __BIT_TYPES_DEFINED__
typedef signed char int8_t;
typedef unsigned char u_int8_t;
typedef short int16_t;
typedef unsigned short u_int16_t;
typedef int int32_t;
typedef unsigned int u_int32_t;
#endif
#define CODA_MAXNAMLEN 255
#define CODA_MAXPATHLEN 1024
#define CODA_MAXSYMLINK 10
#define C_O_READ 0x001
#define C_O_WRITE 0x002
#define C_O_TRUNC 0x010
#define C_O_EXCL 0x100
#define C_O_CREAT 0x200
#define C_M_READ 00400
#define C_M_WRITE 00200
#define C_A_C_OK 8
#define C_A_R_OK 4
#define C_A_W_OK 2
#define C_A_X_OK 1
#define C_A_F_OK 0
#ifndef _VENUS_DIRENT_T_
#define _VENUS_DIRENT_T_ 1
struct venus_dirent {
unsigned long d_fileno;
unsigned short d_reclen;
unsigned char d_type;
unsigned char d_namlen;
char d_name[CODA_MAXNAMLEN + 1];
};
#undef DIRSIZ
#define DIRSIZ(dp) ((sizeof (struct venus_dirent) - (CODA_MAXNAMLEN+1)) + \
(((dp)->d_namlen+1 + 3) &~ 3))
#define CDT_UNKNOWN 0
#define CDT_FIFO 1
#define CDT_CHR 2
#define CDT_DIR 4
#define CDT_BLK 6
#define CDT_REG 8
#define CDT_LNK 10
#define CDT_SOCK 12
#define CDT_WHT 14
#define IFTOCDT(mode) (((mode) & 0170000) >> 12)
#define CDTTOIF(dirtype) ((dirtype) << 12)
#endif
#ifndef _FID_T_
#define _FID_T_ 1
typedef u_long VolumeId;
typedef u_long VnodeId;
typedef u_long Unique_t;
typedef u_long FileVersion;
#endif
#ifndef _VICEFID_T_
#define _VICEFID_T_ 1
typedef struct ViceFid {
VolumeId Volume;
VnodeId Vnode;
Unique_t Unique;
} ViceFid;
#endif
#ifdef __linux__
static __inline__ ino_t coda_f2i(struct ViceFid *fid)
{
if ( ! fid )
return 0;
if (fid->Vnode == 0xfffffffe || fid->Vnode == 0xffffffff)
return ((fid->Volume << 20) | (fid->Unique & 0xfffff));
else
return (fid->Unique + (fid->Vnode<<10) + (fid->Volume<<20));
}
#else
#define coda_f2i(fid)\
((fid) ? ((fid)->Unique + ((fid)->Vnode<<10) + ((fid)->Volume<<20)) : 0)
#endif
#ifndef _VUID_T_
#define _VUID_T_
typedef u_int32_t vuid_t;
typedef u_int32_t vgid_t;
#endif
#ifndef _CODACRED_T_
#define _CODACRED_T_
struct coda_cred {
vuid_t cr_uid, cr_euid, cr_suid, cr_fsuid;
vgid_t cr_groupid, cr_egid, cr_sgid, cr_fsgid;
};
#endif
#ifndef _VENUS_VATTR_T_
#define _VENUS_VATTR_T_
enum coda_vtype { C_VNON, C_VREG, C_VDIR, C_VBLK, C_VCHR, C_VLNK, C_VSOCK, C_VFIFO, C_VBAD };
struct coda_vattr {
long va_type;
u_short va_mode;
short va_nlink;
vuid_t va_uid;
vgid_t va_gid;
long va_fileid;
u_quad_t va_size;
long va_blocksize;
struct timespec va_atime;
struct timespec va_mtime;
struct timespec va_ctime;
u_long va_gen;
u_long va_flags;
cdev_t va_rdev;
u_quad_t va_bytes;
u_quad_t va_filerev;
};
#endif
struct coda_statfs {
int32_t f_blocks;
int32_t f_bfree;
int32_t f_bavail;
int32_t f_files;
int32_t f_ffree;
};
#define CODA_ROOT 2
#define CODA_SYNC 3
#define CODA_OPEN 4
#define CODA_CLOSE 5
#define CODA_IOCTL 6
#define CODA_GETATTR 7
#define CODA_SETATTR 8
#define CODA_ACCESS 9
#define CODA_LOOKUP 10
#define CODA_CREATE 11
#define CODA_REMOVE 12
#define CODA_LINK 13
#define CODA_RENAME 14
#define CODA_MKDIR 15
#define CODA_RMDIR 16
#define CODA_READDIR 17
#define CODA_SYMLINK 18
#define CODA_READLINK 19
#define CODA_FSYNC 20
#define CODA_INACTIVE 21
#define CODA_VGET 22
#define CODA_SIGNAL 23
#define CODA_REPLACE 24
#define CODA_FLUSH 25
#define CODA_PURGEUSER 26
#define CODA_ZAPFILE 27
#define CODA_ZAPDIR 28
#define CODA_PURGEFID 30
#define CODA_OPEN_BY_PATH 31
#define CODA_RESOLVE 32
#define CODA_REINTEGRATE 33
#define CODA_STATFS 34
#define CODA_NCALLS 35
#define DOWNCALL(opcode) (opcode >= CODA_REPLACE && opcode <= CODA_PURGEFID)
#define VC_MAXDATASIZE 8192
#define VC_MAXMSGSIZE sizeof(union inputArgs)+sizeof(union outputArgs) +\
VC_MAXDATASIZE
#define CIOC_KERNEL_VERSION _IOWR('c', 10, sizeof (int))
#if 0
#define CODA_KERNEL_VERSION 0
#define CODA_KERNEL_VERSION 1
#endif
#define CODA_KERNEL_VERSION 2
struct coda_in_hdr {
unsigned long opcode;
unsigned long unique;
u_short pid;
u_short pgid;
u_short sid;
struct coda_cred cred;
};
struct coda_out_hdr {
unsigned long opcode;
unsigned long unique;
unsigned long result;
};
struct coda_root_out {
struct coda_out_hdr oh;
ViceFid VFid;
};
struct coda_root_in {
struct coda_in_hdr in;
};
struct coda_open_in {
struct coda_in_hdr ih;
ViceFid VFid;
int flags;
};
struct coda_open_out {
struct coda_out_hdr oh;
cdev_t dev;
ino_t inode;
};
struct coda_close_in {
struct coda_in_hdr ih;
ViceFid VFid;
int flags;
};
struct coda_close_out {
struct coda_out_hdr out;
};
struct coda_ioctl_in {
struct coda_in_hdr ih;
ViceFid VFid;
int cmd;
int len;
int rwflag;
char *data;
};
struct coda_ioctl_out {
struct coda_out_hdr oh;
int len;
caddr_t data;
};
struct coda_getattr_in {
struct coda_in_hdr ih;
ViceFid VFid;
};
struct coda_getattr_out {
struct coda_out_hdr oh;
struct coda_vattr attr;
};
struct coda_setattr_in {
struct coda_in_hdr ih;
ViceFid VFid;
struct coda_vattr attr;
};
struct coda_setattr_out {
struct coda_out_hdr out;
};
struct coda_access_in {
struct coda_in_hdr ih;
ViceFid VFid;
int flags;
};
struct coda_access_out {
struct coda_out_hdr out;
};
#define CLU_CASE_SENSITIVE 0x01
#define CLU_CASE_INSENSITIVE 0x02
struct coda_lookup_in {
struct coda_in_hdr ih;
ViceFid VFid;
int name;
int flags;
};
struct coda_lookup_out {
struct coda_out_hdr oh;
ViceFid VFid;
int vtype;
};
struct coda_create_in {
struct coda_in_hdr ih;
ViceFid VFid;
struct coda_vattr attr;
int excl;
int mode;
int name;
};
struct coda_create_out {
struct coda_out_hdr oh;
ViceFid VFid;
struct coda_vattr attr;
};
struct coda_remove_in {
struct coda_in_hdr ih;
ViceFid VFid;
int name;
};
struct coda_remove_out {
struct coda_out_hdr out;
};
struct coda_link_in {
struct coda_in_hdr ih;
ViceFid sourceFid;
ViceFid destFid;
int tname;
};
struct coda_link_out {
struct coda_out_hdr out;
};
struct coda_rename_in {
struct coda_in_hdr ih;
ViceFid sourceFid;
int srcname;
ViceFid destFid;
int destname;
};
struct coda_rename_out {
struct coda_out_hdr out;
};
struct coda_mkdir_in {
struct coda_in_hdr ih;
ViceFid VFid;
struct coda_vattr attr;
int name;
};
struct coda_mkdir_out {
struct coda_out_hdr oh;
ViceFid VFid;
struct coda_vattr attr;
};
struct coda_rmdir_in {
struct coda_in_hdr ih;
ViceFid VFid;
int name;
};
struct coda_rmdir_out {
struct coda_out_hdr out;
};
struct coda_readdir_in {
struct coda_in_hdr ih;
ViceFid VFid;
int count;
int offset;
};
struct coda_readdir_out {
struct coda_out_hdr oh;
int size;
caddr_t data;
};
struct coda_symlink_in {
struct coda_in_hdr ih;
ViceFid VFid;
int srcname;
struct coda_vattr attr;
int tname;
};
struct coda_symlink_out {
struct coda_out_hdr out;
};
struct coda_readlink_in {
struct coda_in_hdr ih;
ViceFid VFid;
};
struct coda_readlink_out {
struct coda_out_hdr oh;
int count;
caddr_t data;
};
struct coda_fsync_in {
struct coda_in_hdr ih;
ViceFid VFid;
};
struct coda_fsync_out {
struct coda_out_hdr out;
};
struct coda_inactive_in {
struct coda_in_hdr ih;
ViceFid VFid;
};
struct coda_vget_in {
struct coda_in_hdr ih;
ViceFid VFid;
};
struct coda_vget_out {
struct coda_out_hdr oh;
ViceFid VFid;
int vtype;
};
struct coda_purgeuser_out {
struct coda_out_hdr oh;
struct coda_cred cred;
};
struct coda_zapfile_out {
struct coda_out_hdr oh;
ViceFid CodaFid;
};
struct coda_zapdir_out {
struct coda_out_hdr oh;
ViceFid CodaFid;
};
struct coda_zapvnode_out {
struct coda_out_hdr oh;
struct coda_cred cred;
ViceFid VFid;
};
struct coda_purgefid_out {
struct coda_out_hdr oh;
ViceFid CodaFid;
};
struct coda_rdwr_in {
struct coda_in_hdr ih;
ViceFid VFid;
int rwflag;
int count;
int offset;
int ioflag;
caddr_t data;
};
struct coda_rdwr_out {
struct coda_out_hdr oh;
int rwflag;
int count;
caddr_t data;
};
struct coda_replace_out {
struct coda_out_hdr oh;
ViceFid NewFid;
ViceFid OldFid;
};
struct coda_open_by_path_in {
struct coda_in_hdr ih;
ViceFid VFid;
int flags;
};
struct coda_open_by_path_out {
struct coda_out_hdr oh;
int path;
};
struct coda_statfs_in {
struct coda_in_hdr in;
};
struct coda_statfs_out {
struct coda_out_hdr oh;
struct coda_statfs stat;
};
#define CODA_NOCACHE 0x80000000
union inputArgs {
struct coda_in_hdr ih;
struct coda_open_in coda_open;
struct coda_close_in coda_close;
struct coda_ioctl_in coda_ioctl;
struct coda_getattr_in coda_getattr;
struct coda_setattr_in coda_setattr;
struct coda_access_in coda_access;
struct coda_lookup_in coda_lookup;
struct coda_create_in coda_create;
struct coda_remove_in coda_remove;
struct coda_link_in coda_link;
struct coda_rename_in coda_rename;
struct coda_mkdir_in coda_mkdir;
struct coda_rmdir_in coda_rmdir;
struct coda_readdir_in coda_readdir;
struct coda_symlink_in coda_symlink;
struct coda_readlink_in coda_readlink;
struct coda_fsync_in coda_fsync;
struct coda_inactive_in coda_inactive;
struct coda_vget_in coda_vget;
struct coda_rdwr_in coda_rdwr;
struct coda_open_by_path_in coda_open_by_path;
struct coda_statfs_in coda_statfs;
};
union outputArgs {
struct coda_out_hdr oh;
struct coda_root_out coda_root;
struct coda_open_out coda_open;
struct coda_ioctl_out coda_ioctl;
struct coda_getattr_out coda_getattr;
struct coda_lookup_out coda_lookup;
struct coda_create_out coda_create;
struct coda_mkdir_out coda_mkdir;
struct coda_readdir_out coda_readdir;
struct coda_readlink_out coda_readlink;
struct coda_vget_out coda_vget;
struct coda_purgeuser_out coda_purgeuser;
struct coda_zapfile_out coda_zapfile;
struct coda_zapdir_out coda_zapdir;
struct coda_zapvnode_out coda_zapvnode;
struct coda_purgefid_out coda_purgefid;
struct coda_rdwr_out coda_rdwr;
struct coda_replace_out coda_replace;
struct coda_open_by_path_out coda_open_by_path;
struct coda_statfs_out coda_statfs;
};
union coda_downcalls {
struct coda_purgeuser_out purgeuser;
struct coda_zapfile_out zapfile;
struct coda_zapdir_out zapdir;
struct coda_zapvnode_out zapvnode;
struct coda_purgefid_out purgefid;
struct coda_replace_out replace;
};
#define PIOCPARM_MASK 0x0000ffff
struct ViceIoctl {
caddr_t in, out;
short in_size;
short out_size;
};
struct PioctlData {
const char *path;
int follow;
struct ViceIoctl vi;
};
#define CODA_CONTROL ".CONTROL"
#define CODA_CONTROLLEN 8
#define CTL_VOL -1
#define CTL_VNO -1
#define CTL_UNI -1
#define CTL_INO -1
#define CTL_FILE "/coda/.CONTROL"
#define IS_CTL_FID(fidp) ((fidp)->Volume == CTL_VOL &&\
(fidp)->Vnode == CTL_VNO &&\
(fidp)->Unique == CTL_UNI)
#endif