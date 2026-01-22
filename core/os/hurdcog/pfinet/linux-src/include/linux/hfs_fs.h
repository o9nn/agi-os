#ifndef _LINUX_HFS_FS_H
#define _LINUX_HFS_FS_H
#include <linux/hfs_sysdep.h>
#define HFS_DBL_MAGIC 0x00051607
#define HFS_SNGL_MAGIC 0x00051600
#define HFS_HDR_VERSION_1 0x00010000
#define HFS_HDR_VERSION_2 0x00020000
#define HFS_INO_MAGIC 0x4821
#define HFS_SB_MAGIC 0x4822
#define HFS_DBL_HDR_LEN 1024
#define HFS_NAT_HDR_LEN 1024
#define HFS_CNID(X) ((X) & 0x3FFFFFFF)
#define HFS_ITYPE(X) ((X) & 0xC0000000)
#define HFS_ITYPE_TO_INT(X) ((X) >> 30)
#define HFS_INT_TO_ITYPE(X) ((X) << 30)
#define HFS_ITYPE_0 0x00000000
#define HFS_ITYPE_1 0x40000000
#define HFS_ITYPE_2 0x80000000
#define HFS_ITYPE_3 0xC0000000
#define HFS_ITYPE_NORM HFS_ITYPE_0
#define HFS_CAP_NORM HFS_ITYPE_0
#define HFS_CAP_DATA HFS_ITYPE_0
#define HFS_CAP_NDIR HFS_ITYPE_0
#define HFS_CAP_FNDR HFS_ITYPE_1
#define HFS_CAP_RSRC HFS_ITYPE_2
#define HFS_CAP_RDIR HFS_ITYPE_2
#define HFS_CAP_FDIR HFS_ITYPE_3
#define HFS_DBL_NORM HFS_ITYPE_0
#define HFS_DBL_DATA HFS_ITYPE_0
#define HFS_DBL_DIR HFS_ITYPE_0
#define HFS_DBL_HDR HFS_ITYPE_1
#define HFS_NAT_NORM HFS_ITYPE_0
#define HFS_NAT_DATA HFS_ITYPE_0
#define HFS_NAT_NDIR HFS_ITYPE_0
#define HFS_NAT_HDR HFS_ITYPE_1
#define HFS_NAT_HDIR HFS_ITYPE_2
#define HFS_SGL_NORM HFS_ITYPE_0
#define HFS_SGL_SNGL HFS_ITYPE_0
#define HFS_SGL_DIR HFS_ITYPE_0
#define HFS_SGL_DINF HFS_ITYPE_1
#define HFS_HDR_DATA 1
#define HFS_HDR_RSRC 2
#define HFS_HDR_FNAME 3
#define HFS_HDR_COMNT 4
#define HFS_HDR_BWICN 5
#define HFS_HDR_CICON 6
#define HFS_HDR_OLDI 7
#define HFS_HDR_DATES 8
#define HFS_HDR_FINFO 9
#define HFS_HDR_MACI 10
#define HFS_HDR_PRODOSI 11
#define HFS_HDR_MSDOSI 12
#define HFS_HDR_SNAME 13
#define HFS_HDR_AFPI 14
#define HFS_HDR_DID 15
#define HFS_HDR_MAX 16
#define hfs_h_to_mtime(ARG) htonl((hfs_s32)ntohl(ARG)+3029529600U)
#define hfs_m_to_htime(ARG) ((hfs_s32)htonl(ntohl(ARG)-3029529600U))
#define hfs_h_to_utime(ARG) ((hfs_s32)hfs_to_utc(ntohl(ARG)+946684800U))
#define hfs_u_to_htime(ARG) ((hfs_s32)htonl(hfs_from_utc(ARG)-946684800U))
#define hfs_u_to_mtime(ARG) htonl(hfs_from_utc(ARG)+2082844800U)
#define hfs_m_to_utime(ARG) (hfs_to_utc(ntohl(ARG)-2082844800U))
struct hfs_hdr_descr {
hfs_u32 id;
hfs_u32 offset;
hfs_u32 length;
};
struct hfs_hdr_layout {
hfs_u32 magic;
hfs_u32 version;
hfs_u16 entries;
struct hfs_hdr_descr
descr[HFS_HDR_MAX];
struct hfs_hdr_descr
*order[HFS_HDR_MAX];
};
struct hfs_nat_hdr {
hfs_lword_t magic;
hfs_lword_t version;
hfs_byte_t homefs[16];
hfs_word_t entries;
hfs_byte_t descrs[12*5];
hfs_byte_t real_name[255];
hfs_byte_t comment[200];
hfs_byte_t old_info[16];
hfs_u8 finderinfo[32];
};
struct hfs_dbl_hdr {
hfs_lword_t magic;
hfs_lword_t version;
hfs_byte_t filler[16];
hfs_word_t entries;
hfs_byte_t descrs[12*HFS_HDR_MAX];
hfs_byte_t real_name[255];
hfs_byte_t comment[200];
hfs_u32 create_time;
hfs_u32 modify_time;
hfs_u32 backup_time;
hfs_u32 access_time;
hfs_u8 finderinfo[32];
hfs_u32 fileinfo;
hfs_u32 cnid;
hfs_u8 short_name[12];
hfs_u8 prodosi[8];
};
struct hfs_cap_info {
hfs_byte_t fi_fndr[32];
hfs_word_t fi_attr;
#define HFS_AFP_INV 0x001
#define HFS_AFP_EXPFOLDER 0x002
#define HFS_AFP_MULTI 0x002
#define HFS_AFP_SYS 0x004
#define HFS_AFP_DOPEN 0x008
#define HFS_AFP_MOUNTED 0x008
#define HFS_AFP_ROPEN 0x010
#define HFS_AFP_INEXPFOLDER 0x010
#define HFS_AFP_WRI 0x020
#define HFS_AFP_BACKUP 0x040
#define HFS_AFP_RNI 0x080
#define HFS_AFP_DEI 0x100
#define HFS_AFP_NOCOPY 0x400
#define HFS_AFP_RDONLY ( HFS_AFP_WRI|HFS_AFP_RNI|HFS_AFP_DEI)
hfs_byte_t fi_magic1;
#define HFS_CAP_MAGIC1 0xFF
hfs_byte_t fi_version;
#define HFS_CAP_VERSION 0x10
hfs_byte_t fi_magic;
#define HFS_CAP_MAGIC 0xDA
hfs_byte_t fi_bitmap;
#define HFS_CAP_SHORTNAME 0x01
#define HFS_CAP_LONGNAME 0x02
hfs_byte_t fi_shortfilename[12+1];
hfs_byte_t fi_macfilename[32+1];
hfs_byte_t fi_comln;
hfs_byte_t fi_comnt[200];
hfs_byte_t fi_datemagic;
#define HFS_CAP_DMAGIC 0xDA
hfs_byte_t fi_datevalid;
#define HFS_CAP_MDATE 0x01
#define HFS_CAP_CDATE 0x02
hfs_lword_t fi_ctime;
hfs_lword_t fi_mtime;
hfs_lword_t fi_utime;
hfs_byte_t pad;
};
#ifdef __KERNEL__
typedef ssize_t hfs_rwret_t;
typedef size_t hfs_rwarg_t;
#include <asm/uaccess.h>
struct hfs_fork;
struct hfs_cat_key;
struct hfs_cat_entry;
extern struct hfs_cat_entry *hfs_cat_get(struct hfs_mdb *,
const struct hfs_cat_key *);
extern hfs_rwret_t hfs_dir_read(struct file *, char *, hfs_rwarg_t,
loff_t *);
extern int hfs_create(struct inode *, struct dentry *, int);
extern int hfs_mkdir(struct inode *, struct dentry *, int);
extern int hfs_mknod(struct inode *, struct dentry *, int, int);
extern int hfs_unlink(struct inode *, struct dentry *);
extern int hfs_rmdir(struct inode *, struct dentry *);
extern int hfs_rename(struct inode *, struct dentry *,
struct inode *, struct dentry *);
extern const struct hfs_name hfs_cap_reserved1[];
extern const struct hfs_name hfs_cap_reserved2[];
extern struct inode_operations hfs_cap_ndir_inode_operations;
extern struct inode_operations hfs_cap_fdir_inode_operations;
extern struct inode_operations hfs_cap_rdir_inode_operations;
extern void hfs_cap_drop_dentry(struct dentry *, const ino_t);
extern const struct hfs_name hfs_dbl_reserved1[];
extern const struct hfs_name hfs_dbl_reserved2[];
extern struct inode_operations hfs_dbl_dir_inode_operations;
extern void hfs_dbl_drop_dentry(struct dentry *, const ino_t);
extern const struct hfs_name hfs_nat_reserved1[];
extern const struct hfs_name hfs_nat_reserved2[];
extern struct inode_operations hfs_nat_ndir_inode_operations;
extern struct inode_operations hfs_nat_hdir_inode_operations;
extern void hfs_nat_drop_dentry(struct dentry *, const ino_t);
extern const struct hfs_name hfs_sngl_reserved1[];
extern const struct hfs_name hfs_sngl_reserved2[];
extern struct inode_operations hfs_sngl_dir_inode_operations;
extern hfs_s32 hfs_do_read(struct inode *, struct hfs_fork *, hfs_u32,
char *, hfs_u32, int);
extern hfs_s32 hfs_do_write(struct inode *, struct hfs_fork *, hfs_u32,
const char *, hfs_u32);
extern void hfs_file_fix_mode(struct hfs_cat_entry *entry);
extern struct inode_operations hfs_file_inode_operations;
extern struct inode_operations hfs_cap_info_inode_operations;
extern struct inode_operations hfs_hdr_inode_operations;
extern const struct hfs_hdr_layout hfs_dbl_fil_hdr_layout;
extern const struct hfs_hdr_layout hfs_dbl_dir_hdr_layout;
extern const struct hfs_hdr_layout hfs_nat_hdr_layout;
extern const struct hfs_hdr_layout hfs_nat2_hdr_layout;
extern const struct hfs_hdr_layout hfs_sngl_hdr_layout;
extern void hfs_put_inode(struct inode *);
extern int hfs_notify_change(struct dentry *, struct iattr *);
extern struct inode *hfs_iget(struct hfs_cat_entry *, ino_t, struct dentry *);
extern void hfs_cap_ifill(struct inode *, ino_t, const int);
extern void hfs_dbl_ifill(struct inode *, ino_t, const int);
extern void hfs_nat_ifill(struct inode *, ino_t, const int);
extern void hfs_sngl_ifill(struct inode *, ino_t, const int);
extern struct super_block *hfs_read_super(struct super_block *,void *,int);
extern int init_hfs_fs(void);
extern void hfs_colon2mac(struct hfs_name *, const char *, int);
extern void hfs_prcnt2mac(struct hfs_name *, const char *, int);
extern void hfs_triv2mac(struct hfs_name *, const char *, int);
extern void hfs_latin2mac(struct hfs_name *, const char *, int);
extern int hfs_mac2cap(char *, const struct hfs_name *);
extern int hfs_mac2nat(char *, const struct hfs_name *);
extern int hfs_mac2latin(char *, const struct hfs_name *);
extern int hfs_mac2seven(char *, const struct hfs_name *);
extern int hfs_mac2eight(char *, const struct hfs_name *);
extern int hfs_mac2alpha(char *, const struct hfs_name *);
extern int hfs_mac2triv(char *, const struct hfs_name *);
extern void hfs_tolower(unsigned char *, int);
#define HFS_I(X) (&((X)->u.hfs_i))
#define HFS_SB(X) (&((X)->u.hfs_sb))
static __inline__ void hfs_nameout(struct inode *dir, struct hfs_name *out,
const char *in, int len) {
HFS_SB(dir->i_sb)->s_nameout(out, in, len);
}
static __inline__ int hfs_namein(struct inode *dir, char *out,
const struct hfs_name *in) {
int len = HFS_SB(dir->i_sb)->s_namein(out, in);
if (HFS_SB(dir->i_sb)->s_lowercase) {
hfs_tolower(out, len);
}
return len;
}
#endif
#endif