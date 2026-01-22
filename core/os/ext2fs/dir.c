#include "ext2fs.h"
#include <string.h>
#include <stdio.h>
#include <dirent.h>
#include <stddef.h>
#include <inttypes.h>
#include <hurd/sigpreempt.h>
#define DIRBLKSIZ block_size
enum slot_status
{
LOOKING,
TAKE,
SHRINK,
COMPRESS,
EXTEND,
HERE_TIS,
};
struct dirstat
{
enum lookup_type type;
enum slot_status stat;
vm_address_t mapbuf;
vm_size_t mapextent;
int idx;
struct ext2_dir_entry_2 *entry;
struct ext2_dir_entry_2 *preventry;
size_t nbytes;
};
const size_t diskfs_dirstat_size = sizeof (struct dirstat);
void
diskfs_null_dirstat (struct dirstat *ds)
{
ds->type = LOOKUP;
}
static error_t
dirscanblock (vm_address_t blockoff, struct node *dp, int idx,
const char *name, size_t namelen, enum lookup_type type,
struct dirstat *ds, ino_t *inum);
#if 0
static const unsigned char ext2_file_type[EXT2_FT_MAX] =
{
[EXT2_FT_UNKNOWN]	= DT_UNKNOWN,
[EXT2_FT_REG_FILE]	= DT_REG,
[EXT2_FT_DIR]		= DT_DIR,
[EXT2_FT_CHRDEV]	= DT_CHR,
[EXT2_FT_BLKDEV]	= DT_BLK,
[EXT2_FT_FIFO]	= DT_FIFO,
[EXT2_FT_SOCK]	= DT_SOCK,
[EXT2_FT_SYMLINK]	= DT_LNK,
};
static const unsigned char file_type_ext2[] =
{
[DT_UNKNOWN]	= EXT2_FT_UNKNOWN,
[DT_REG]	= EXT2_FT_REG_FILE,
[DT_DIR]	= EXT2_FT_DIR,
[DT_CHR]	= EXT2_FT_CHRDEV,
[DT_BLK]	= EXT2_FT_BLKDEV,
[DT_FIFO]	= EXT2_FT_FIFO,
[DT_SOCK]	= EXT2_FT_SOCK,
[DT_LNK]	= EXT2_FT_SYMLINK,
};
#endif
error_t
diskfs_lookup_hard (struct node *dp, const char *name, enum lookup_type type,
struct node **npp, struct dirstat *ds, struct protid *cred)
{
error_t err;
ino_t inum;
size_t namelen;
int spec_dotdot;
struct node *np = 0;
ino_t retry_dotdot = 0;
vm_prot_t prot =
(type == LOOKUP) ? VM_PROT_READ : (VM_PROT_READ | VM_PROT_WRITE);
memory_object_t memobj;
vm_address_t buf = 0;
vm_size_t buflen = 0;
vm_address_t blockaddr;
int idx, lastidx;
int looped;
if ((type == REMOVE) || (type == RENAME))
assert_backtrace (npp);
if (npp)
*npp = 0;
spec_dotdot = type & SPEC_DOTDOT;
type &= ~SPEC_DOTDOT;
namelen = strlen (name);
if (namelen > EXT2_NAME_LEN)
{
if (ds)
diskfs_null_dirstat (ds);
return ENAMETOOLONG;
}
try_again:
if (ds)
{
ds->type = LOOKUP;
ds->mapbuf = 0;
ds->mapextent = 0;
}
if (buf)
{
munmap ((caddr_t) buf, buflen);
buf = 0;
}
if (ds && (type == CREATE || type == RENAME))
ds->stat = LOOKING;
memobj = diskfs_get_filemap (dp, prot);
if (memobj == MACH_PORT_NULL)
return errno;
buf = 0;
buflen = round_page (dp->dn_stat.st_size + DIRBLKSIZ);
err = vm_map (mach_task_self (),
&buf, buflen, 0, 1, memobj, 0, 0, prot, prot, 0);
mach_port_deallocate (mach_task_self (), memobj);
if (err)
return err;
inum = 0;
diskfs_set_node_atime (dp);
idx = diskfs_node_disknode (dp)->dir_idx;
if (idx * DIRBLKSIZ > dp->dn_stat.st_size)
idx = 0;
blockaddr = buf + idx * DIRBLKSIZ;
looped = (idx == 0);
lastidx = idx;
if (lastidx == 0)
lastidx = dp->dn_stat.st_size / DIRBLKSIZ;
while (!looped || idx < lastidx)
{
err = dirscanblock (blockaddr, dp, idx, name, namelen, type, ds, &inum);
if (!err)
{
diskfs_node_disknode (dp)->dir_idx = idx;
break;
}
if (err != ENOENT)
{
munmap ((caddr_t) buf, buflen);
return err;
}
blockaddr += DIRBLKSIZ;
idx++;
if (blockaddr - buf >= dp->dn_stat.st_size && !looped)
{
looped = 1;
blockaddr = buf;
idx = 0;
}
}
diskfs_set_node_atime (dp);
if (diskfs_synchronous)
diskfs_node_update (dp, 1);
err = 0;
if (inum && npp)
{
if (namelen != 2 || name[0] != '.' || name[1] != '.')
{
if (inum == dp->cache_id)
{
np = dp;
diskfs_nref (np);
}
else
{
err = diskfs_cached_lookup (inum, &np);
if (err)
goto out;
}
}
else if (dp->cache_id == 2)
{
err = EAGAIN;
goto out;
}
else if (retry_dotdot)
{
if (inum != retry_dotdot)
{
diskfs_nput (np);
pthread_mutex_unlock (&dp->lock);
err = diskfs_cached_lookup (inum, &np);
pthread_mutex_lock (&dp->lock);
if (err)
goto out;
retry_dotdot = inum;
goto try_again;
}
}
else if (!spec_dotdot)
{
pthread_mutex_unlock (&dp->lock);
err = diskfs_cached_lookup (inum, &np);
pthread_mutex_lock (&dp->lock);
if (err)
goto out;
retry_dotdot = inum;
goto try_again;
}
else if (type == RENAME || type == REMOVE)
np = diskfs_cached_ifind (inum);
else if (type == LOOKUP)
{
diskfs_nput (dp);
err = diskfs_cached_lookup (inum, &np);
if (err)
goto out;
}
else
assert_backtrace (0);
}
if ((type == CREATE || type == RENAME) && !inum && ds && ds->stat == LOOKING)
{
ds->type = CREATE;
ds->stat = EXTEND;
ds->idx = dp->dn_stat.st_size / DIRBLKSIZ;
}
out:
if ((err && err != ENOENT)
|| !ds
|| ds->type == LOOKUP)
{
munmap ((caddr_t) buf, buflen);
if (ds)
ds->type = LOOKUP;
}
else
{
ds->mapbuf = buf;
ds->mapextent = buflen;
}
if (np)
{
assert_backtrace (npp);
if (err)
{
if (!spec_dotdot)
{
if (np == dp)
diskfs_nrele (np);
else
diskfs_nput (np);
}
else if (type == RENAME || type == REMOVE)
;
else if (type == LOOKUP)
diskfs_nput (np);
}
else
*npp = np;
}
return err ? : inum ? 0 : ENOENT;
}
static error_t
dirscanblock (vm_address_t blockaddr, struct node *dp, int idx,
const char *name, size_t namelen, enum lookup_type type,
struct dirstat *ds, ino_t *inum)
{
size_t nfree = 0;
size_t needed = 0;
vm_address_t currentoff, prevoff;
struct ext2_dir_entry_2 *entry = 0;
int nentries = 0;
size_t nbytes = 0;
int looking = 0;
int countcopies = 0;
int consider_compress = 0;
if (ds && (ds->stat == LOOKING
|| ds->stat == COMPRESS))
{
looking = 1;
countcopies = 1;
needed = EXT2_DIR_REC_LEN (namelen);
}
for (currentoff = blockaddr, prevoff = 0;
currentoff < blockaddr + DIRBLKSIZ;
prevoff = currentoff, currentoff += le16toh (entry->rec_len))
{
entry = (struct ext2_dir_entry_2 *)currentoff;
if (!le16toh (entry->rec_len)
|| le16toh (entry->rec_len) % EXT2_DIR_PAD
|| entry->name_len > EXT2_NAME_LEN
|| currentoff + le16toh (entry->rec_len) > blockaddr + DIRBLKSIZ
|| EXT2_DIR_REC_LEN (entry->name_len) > le16toh (entry->rec_len)
|| memchr (entry->name, '\0', entry->name_len))
{
ext2_warning ("bad directory entry: inode: %" PRIu64 " offset: %lu",
dp->cache_id,
(unsigned long)(currentoff - blockaddr + idx * DIRBLKSIZ));
return ENOENT;
}
if (looking || countcopies)
{
size_t thisfree;
if (le32toh (entry->inode) == 0)
thisfree = le16toh (entry->rec_len);
else
thisfree = le16toh (entry->rec_len) - EXT2_DIR_REC_LEN (entry->name_len);
if (countcopies && currentoff != blockaddr)
nbytes += EXT2_DIR_REC_LEN (entry->name_len);
if (ds->stat == COMPRESS && nbytes > ds->nbytes)
countcopies = 0;
if (thisfree >= needed)
{
ds->type = CREATE;
ds->stat = le32toh (entry->inode) == 0 ? TAKE : SHRINK;
ds->entry = entry;
ds->idx = idx;
looking = countcopies = 0;
}
else
{
nfree += thisfree;
if (nfree >= needed)
consider_compress = 1;
}
}
if (le32toh (entry->inode))
nentries++;
if (entry->name_len == namelen
&& entry->name[0] == name[0]
&& le32toh (entry->inode)
&& !bcmp (entry->name, name, namelen))
break;
}
if (consider_compress
&& (ds->stat == LOOKING
|| (ds->stat == COMPRESS && ds->nbytes > nbytes)))
{
ds->type = CREATE;
ds->stat = COMPRESS;
ds->entry = (struct ext2_dir_entry_2 *) blockaddr;
ds->idx = idx;
ds->nbytes = nbytes;
}
if (currentoff >= blockaddr + DIRBLKSIZ)
{
int i;
if (!diskfs_node_disknode (dp)->dirents)
{
diskfs_node_disknode (dp)->dirents =
malloc ((dp->dn_stat.st_size / DIRBLKSIZ) * sizeof (int));
for (i = 0; i < dp->dn_stat.st_size/DIRBLKSIZ; i++)
diskfs_node_disknode (dp)->dirents[i] = -1;
}
assert_backtrace (diskfs_node_disknode (dp)->dirents[idx] == -1
|| diskfs_node_disknode (dp)->dirents[idx] == nentries);
diskfs_node_disknode (dp)->dirents[idx] = nentries;
return ENOENT;
}
if (ds && type == CREATE)
ds->type = LOOKUP;
else if (ds && (type == REMOVE || type == RENAME))
{
ds->type = type;
ds->stat = HERE_TIS;
ds->entry = entry;
ds->idx = idx;
ds->preventry = (struct ext2_dir_entry_2 *) prevoff;
}
*inum = le32toh (entry->inode);
return 0;
}
error_t
diskfs_direnter_hard (struct node *dp, const char *name, struct node *np,
struct dirstat *ds, struct protid *cred)
{
struct ext2_dir_entry_2 *new;
size_t namelen = strlen (name);
size_t needed = EXT2_DIR_REC_LEN (namelen);
size_t oldneeded;
vm_address_t fromoff, tooff;
size_t totfreed;
error_t err;
size_t oldsize = 0;
assert_backtrace (ds->type == CREATE);
assert_backtrace (!diskfs_readonly);
dp->dn_set_mtime = 1;
switch (ds->stat)
{
case TAKE:
assert_backtrace (le32toh (ds->entry->inode) == 0
&& le16toh (ds->entry->rec_len) >= needed);
new = ds->entry;
break;
case SHRINK:
oldneeded = EXT2_DIR_REC_LEN (ds->entry->name_len);
assert_backtrace (le16toh (ds->entry->rec_len) - oldneeded >= needed);
new = (struct ext2_dir_entry_2 *) ((vm_address_t) ds->entry + oldneeded);
new->rec_len = htole16 (le16toh (ds->entry->rec_len) - oldneeded);
ds->entry->rec_len = htole16 (oldneeded);
break;
case COMPRESS:
fromoff = tooff = (vm_address_t) ds->entry;
while (fromoff < (vm_address_t) ds->entry + DIRBLKSIZ)
{
struct ext2_dir_entry_2 *from = (struct ext2_dir_entry_2 *)fromoff;
struct ext2_dir_entry_2 *to = (struct ext2_dir_entry_2 *) tooff;
size_t fromreclen = le16toh (from->rec_len);
if (le32toh (from->inode) != 0)
{
assert_backtrace (fromoff >= tooff);
memmove (to, from, fromreclen);
to->rec_len = htole16 (EXT2_DIR_REC_LEN (to->name_len));
tooff += le16toh (to->rec_len);
}
fromoff += fromreclen;
}
totfreed = (vm_address_t) ds->entry + DIRBLKSIZ - tooff;
assert_backtrace (totfreed >= needed);
new = (struct ext2_dir_entry_2 *) tooff;
new->rec_len = htole16 (totfreed);
break;
case EXTEND:
assert_backtrace (needed <= DIRBLKSIZ);
oldsize = dp->dn_stat.st_size;
if ((off_t)(oldsize + DIRBLKSIZ) != (dp->dn_stat.st_size + DIRBLKSIZ))
{
munmap ((caddr_t) ds->mapbuf, ds->mapextent);
return EOVERFLOW;
}
while (oldsize + DIRBLKSIZ > dp->allocsize)
{
err = diskfs_grow (dp, oldsize + DIRBLKSIZ, cred);
if (err)
{
munmap ((caddr_t) ds->mapbuf, ds->mapextent);
return err;
}
}
new = (struct ext2_dir_entry_2 *) (ds->mapbuf + oldsize);
err = hurd_safe_memset (new, 0, DIRBLKSIZ);
if (err)
{
if (err == EKERN_MEMORY_ERROR)
err = ENOSPC;
munmap ((caddr_t) ds->mapbuf, ds->mapextent);
return err;
}
dp->dn_stat.st_size = oldsize + DIRBLKSIZ;
dp->dn_set_ctime = 1;
new->rec_len = htole16 (DIRBLKSIZ);
break;
default:
new = 0;
assert_backtrace (! "impossible: bogus status field in dirstat");
}
new->inode = htole32 (np->cache_id);
#if 0
new->file_type = (EXT2_HAS_INCOMPAT_FEATURE (sblock,
EXT2_FEATURE_INCOMPAT_FILETYPE)
? file_type_ext2[IFTODT (np->dn_stat.st_mode & S_IFMT)]
: 0);
#else
new->file_type = 0;
#endif
new->name_len = namelen;
memcpy (new->name, name, namelen);
diskfs_node_disknode (dp)->info.i_flags &= ~EXT2_BTREE_FL;
dp->dn_set_mtime = 1;
munmap ((caddr_t) ds->mapbuf, ds->mapextent);
if (ds->stat != EXTEND)
{
if (diskfs_node_disknode (dp)->dirents
&& diskfs_node_disknode (dp)->dirents[ds->idx] != -1)
diskfs_node_disknode (dp)->dirents[ds->idx]++;
}
else
{
int i;
if (diskfs_node_disknode (dp)->dirents)
{
diskfs_node_disknode (dp)->dirents =
realloc (diskfs_node_disknode (dp)->dirents,
(dp->dn_stat.st_size / DIRBLKSIZ * sizeof (int)));
for (i = oldsize / DIRBLKSIZ;
i < dp->dn_stat.st_size / DIRBLKSIZ;
i++)
diskfs_node_disknode (dp)->dirents[i] = -1;
diskfs_node_disknode (dp)->dirents[ds->idx] = 1;
}
else
{
diskfs_node_disknode (dp)->dirents =
malloc (dp->dn_stat.st_size / DIRBLKSIZ * sizeof (int));
for (i = 0; i < dp->dn_stat.st_size / DIRBLKSIZ; i++)
diskfs_node_disknode (dp)->dirents[i] = -1;
diskfs_node_disknode (dp)->dirents[ds->idx] = 1;
}
}
diskfs_file_update (dp, diskfs_synchronous);
return 0;
}
error_t
diskfs_dirremove_hard (struct node *dp, struct dirstat *ds)
{
assert_backtrace (ds->type == REMOVE);
assert_backtrace (ds->stat == HERE_TIS);
assert_backtrace (!diskfs_readonly);
if (ds->preventry == 0)
ds->entry->inode = htole32 (0);
else
{
assert_backtrace ((vm_address_t) ds->entry - (vm_address_t) ds->preventry
== le16toh (ds->preventry->rec_len));
ds->preventry->rec_len = htole16( le16toh (ds->preventry->rec_len) + ds->entry->rec_len);
}
dp->dn_set_mtime = 1;
diskfs_node_disknode (dp)->info.i_flags &= ~EXT2_BTREE_FL;
munmap ((caddr_t) ds->mapbuf, ds->mapextent);
if (diskfs_node_disknode (dp)->dirents
&& diskfs_node_disknode (dp)->dirents[ds->idx] != -1)
diskfs_node_disknode (dp)->dirents[ds->idx]--;
diskfs_file_update (dp, diskfs_synchronous);
return 0;
}
error_t
diskfs_dirrewrite_hard (struct node *dp, struct node *np, struct dirstat *ds)
{
assert_backtrace (ds->type == RENAME);
assert_backtrace (ds->stat == HERE_TIS);
assert_backtrace (!diskfs_readonly);
ds->entry->inode = htole32 (np->cache_id);
dp->dn_set_mtime = 1;
diskfs_node_disknode (dp)->info.i_flags &= ~EXT2_BTREE_FL;
munmap ((caddr_t) ds->mapbuf, ds->mapextent);
diskfs_file_update (dp, diskfs_synchronous);
return 0;
}
int
diskfs_dirempty (struct node *dp, struct protid *cred)
{
error_t err;
vm_address_t buf = 0, curoff;
struct ext2_dir_entry_2 *entry;
int hit = 0;
memory_object_t memobj = diskfs_get_filemap (dp, VM_PROT_READ);
if (memobj == MACH_PORT_NULL)
return 0;
err = vm_map (mach_task_self (), &buf, dp->dn_stat.st_size, 0,
1, memobj, 0, 0, VM_PROT_READ, VM_PROT_READ, 0);
mach_port_deallocate (mach_task_self (), memobj);
assert_backtrace (!err);
diskfs_set_node_atime (dp);
for (curoff = buf;
!hit && curoff < buf + dp->dn_stat.st_size;
curoff += le16toh (entry->rec_len))
{
entry = (struct ext2_dir_entry_2 *) curoff;
if (le32toh (entry->inode) != 0
&& (entry->name_len > 2
|| entry->name[0] != '.'
|| (entry->name[1] != '.'
&& entry->name[1] != '\0')))
hit = 1;
}
diskfs_set_node_atime (dp);
if (diskfs_synchronous)
diskfs_node_update (dp, 1);
munmap ((caddr_t) buf, dp->dn_stat.st_size);
return !hit;
}
error_t
diskfs_drop_dirstat (struct node *dp, struct dirstat *ds)
{
if (ds->type != LOOKUP)
{
assert_backtrace (ds->mapbuf);
munmap ((caddr_t) ds->mapbuf, ds->mapextent);
ds->type = LOOKUP;
}
return 0;
}
static error_t
count_dirents (struct node *dp, block_t nb, char *buf)
{
mach_msg_type_number_t amt;
char *offinblk;
struct ext2_dir_entry_2 *entry;
int count = 0;
error_t err;
assert_backtrace (diskfs_node_disknode (dp)->dirents);
assert_backtrace ((nb + 1) * DIRBLKSIZ <= dp->dn_stat.st_size);
err = diskfs_node_rdwr (dp, buf, nb * DIRBLKSIZ, DIRBLKSIZ, 0, 0, &amt);
if (err)
return err;
assert_backtrace (amt == DIRBLKSIZ);
for (offinblk = buf;
offinblk < buf + DIRBLKSIZ;
offinblk += le16toh (entry->rec_len))
{
entry = (struct ext2_dir_entry_2 *) offinblk;
if (le32toh (entry->inode))
count++;
}
assert_backtrace (diskfs_node_disknode (dp)->dirents[nb] == -1
|| diskfs_node_disknode (dp)->dirents[nb] == count);
diskfs_node_disknode (dp)->dirents[nb] = count;
return 0;
}
#define DIRENT_ALIGN __alignof (struct dirent)
error_t
diskfs_get_directs (struct node *dp,
int entry,
int nentries,
char **data,
mach_msg_type_number_t *datacnt,
vm_size_t bufsiz,
int *amt)
{
block_t blkno;
block_t nblks;
int curentry;
char buf[DIRBLKSIZ];
char *bufp;
int bufvalid;
error_t err;
int i;
char *datap;
struct ext2_dir_entry_2 *entryp;
int allocsize;
mach_msg_type_number_t checklen;
struct dirent *userp;
nblks = dp->dn_stat.st_size/DIRBLKSIZ;
if (!diskfs_node_disknode (dp)->dirents)
{
diskfs_node_disknode (dp)->dirents = malloc (nblks * sizeof (int));
for (i = 0; i < nblks; i++)
diskfs_node_disknode (dp)->dirents[i] = -1;
}
curentry = 0;
bufvalid = 0;
for (blkno = 0; blkno < nblks; blkno++)
{
if (diskfs_node_disknode (dp)->dirents[blkno] == -1)
{
err = count_dirents (dp, blkno, buf);
if (err)
return err;
bufvalid = 1;
}
if (curentry + diskfs_node_disknode (dp)->dirents[blkno] > entry)
break;
curentry += diskfs_node_disknode (dp)->dirents[blkno];
bufvalid = 0;
}
if (blkno == nblks)
{
*datacnt = 0;
*amt = 0;
return 0;
}
if (!bufsiz || bufsiz > dp->dn_stat.st_size)
{
size_t min_entry_size = EXT2_DIR_REC_LEN (0);
size_t min_dirent_size = offsetof (struct dirent, d_name) + 1;
size_t max_entries = dp->dn_stat.st_size / min_entry_size;
size_t entry_extra =
DIRENT_ALIGN
+ (min_dirent_size > min_entry_size
? min_dirent_size - min_entry_size : 0);
allocsize = round_page (dp->dn_stat.st_size + max_entries * entry_extra);
}
else
allocsize = round_page (bufsiz);
if (allocsize > *datacnt)
{
*data = mmap (0, allocsize, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
assert_backtrace (*data != MAP_FAILED);
}
bufp = buf;
if (curentry != entry)
{
if (!bufvalid)
{
err = diskfs_node_rdwr (dp, buf, blkno * DIRBLKSIZ, DIRBLKSIZ,
0, 0, &checklen);
if (err)
return err;
assert_backtrace (checklen == DIRBLKSIZ);
bufvalid = 1;
}
for (i = 0, bufp = buf;
i < entry - curentry && bufp - buf < DIRBLKSIZ;
bufp += le16toh (((struct ext2_dir_entry_2 *)bufp)->rec_len), i++)
;
assert_backtrace (bufp - buf < DIRBLKSIZ);
}
i = 0;
datap = *data;
while (((nentries == -1) || (i < nentries))
&& (!bufsiz || (datap - *data < bufsiz) )
&& blkno < nblks)
{
if (!bufvalid)
{
err = diskfs_node_rdwr (dp, buf, blkno * DIRBLKSIZ, DIRBLKSIZ,
0, 0, &checklen);
if (err)
return err;
assert_backtrace (checklen == DIRBLKSIZ);
bufvalid = 1;
bufp = buf;
}
entryp = (struct ext2_dir_entry_2 *)bufp;
if (le32toh (entryp->inode))
{
int rec_len;
int name_len = entryp->name_len;
userp = (struct dirent *) datap;
rec_len =
((offsetof (struct dirent, d_name)
+ name_len + 1
+ (DIRENT_ALIGN - 1))
& ~(DIRENT_ALIGN - 1));
if (bufsiz == 0)
assert_backtrace (datap + rec_len <= *data + allocsize);
else
if (datap + rec_len > *data + allocsize)
break;
userp->d_fileno = le32toh (entryp->inode);
userp->d_reclen = rec_len;
userp->d_namlen = name_len;
#if 0
if (entryp->file_type < EXT2_FT_MAX)
userp->d_type = ext2_file_type[entryp->file_type];
else
{
ext2_warning ("bad type %d in directory entry: "
"inode: %d offset: %d",
entryp->file_type,
dp->cache_id,
blkno * DIRBLKSIZ + bufp - buf);
userp->d_type = DT_UNKNOWN;
}
#else
userp->d_type = DT_UNKNOWN;
#endif
memcpy (userp->d_name, entryp->name, name_len);
userp->d_name[name_len] = '\0';
datap += rec_len;
i++;
}
if (le16toh (entryp->rec_len) == 0)
{
ext2_warning ("zero length directory entry: inode: %" PRIu64
" offset: %zd",
dp->cache_id,
blkno * DIRBLKSIZ + bufp - buf);
return EIO;
}
bufp += le16toh (entryp->rec_len);
if (bufp - buf == DIRBLKSIZ)
{
blkno++;
bufvalid = 0;
}
else if (bufp - buf > DIRBLKSIZ)
{
ext2_warning ("directory entry too long: inode: %" PRIu64
" offset: %zd",
dp->cache_id,
blkno * DIRBLKSIZ + bufp - buf - le16toh (entryp->rec_len));
return EIO;
}
}
if (allocsize > *datacnt)
{
if (round_page (datap - *data) < allocsize)
munmap ((caddr_t) (*data + round_page (datap - *data)),
allocsize - round_page (datap - *data));
}
*datacnt = datap - *data;
*amt = i;
return 0;
}