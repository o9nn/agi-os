#ifndef __LINUX_DCACHE_H
#define __LINUX_DCACHE_H
#ifdef __KERNEL__
#define D_MAXLEN 1024
#define IS_ROOT(x) ((x) == (x)->d_parent)
struct qstr {
const unsigned char * name;
unsigned int len;
unsigned int hash;
};
#define init_name_hash()		0
static __inline__ unsigned long partial_name_hash(unsigned long c, unsigned long prevhash)
{
prevhash = (prevhash << 4) | (prevhash >> (8*sizeof(unsigned long)-4));
return prevhash ^ c;
}
static __inline__ unsigned long end_name_hash(unsigned long hash)
{
if (sizeof(hash) > sizeof(unsigned int))
hash += hash >> 4*sizeof(hash);
return (unsigned int) hash;
}
static __inline__ unsigned int full_name_hash(const unsigned char * name, unsigned int len)
{
unsigned long hash = init_name_hash();
while (len--)
hash = partial_name_hash(*name++, hash);
return end_name_hash(hash);
}
#define DNAME_INLINE_LEN 16
struct dentry {
int d_count;
unsigned int d_flags;
struct inode  * d_inode;
struct dentry * d_parent;
struct dentry * d_mounts;
struct dentry * d_covers;
struct list_head d_hash;
struct list_head d_lru;
struct list_head d_child;
struct list_head d_subdirs;
struct list_head d_alias;
struct qstr d_name;
unsigned long d_time;
struct dentry_operations  *d_op;
struct super_block * d_sb;
unsigned long d_reftime;
void * d_fsdata;
unsigned char d_iname[DNAME_INLINE_LEN];
};
struct dentry_operations {
int (*d_revalidate)(struct dentry *, int);
int (*d_hash) (struct dentry *, struct qstr *);
int (*d_compare) (struct dentry *, struct qstr *, struct qstr *);
void (*d_delete)(struct dentry *);
void (*d_release)(struct dentry *);
void (*d_iput)(struct dentry *, struct inode *);
};
#define DCACHE_AUTOFS_PENDING 0x0001
#define DCACHE_NFSFS_RENAMED  0x0002
static __inline__ void d_drop(struct dentry * dentry)
{
list_del(&dentry->d_hash);
INIT_LIST_HEAD(&dentry->d_hash);
}
static __inline__ int dname_external(struct dentry *d)
{
return d->d_name.name != d->d_iname;
}
extern void d_instantiate(struct dentry *, struct inode *);
extern void d_delete(struct dentry *);
extern struct dentry * d_alloc(struct dentry * parent, const struct qstr *name);
extern int prune_dcache(int, int);
extern void shrink_dcache_sb(struct super_block *);
extern void shrink_dcache_parent(struct dentry *);
extern int d_invalidate(struct dentry *);
#define shrink_dcache() prune_dcache(0, -1)
extern void shrink_dcache_memory(int, unsigned int);
extern void check_dcache_memory(void);
extern void free_inode_memory(int);
extern struct dentry * d_alloc_root(struct inode * root_inode, struct dentry * old_root);
extern int is_root_busy(struct dentry *);
extern int have_submounts(struct dentry *);
extern void d_rehash(struct dentry * entry);
static __inline__ void d_add(struct dentry * entry, struct inode * inode)
{
d_rehash(entry);
d_instantiate(entry, inode);
}
extern void d_move(struct dentry * entry, struct dentry * newdentry);
extern struct dentry * d_lookup(struct dentry * dir, struct qstr * name);
extern int d_validate(struct dentry *dentry, struct dentry *dparent,
unsigned int hash, unsigned int len);
extern char * d_path(struct dentry * entry, char * buf, int buflen);
static __inline__ struct dentry * dget(struct dentry *dentry)
{
if (dentry)
dentry->d_count++;
return dentry;
}
extern void dput(struct dentry *);
#endif
#endif