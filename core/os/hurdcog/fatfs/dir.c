#include <ctype.h>
#include <string.h>
#include <dirent.h>
#include <hurd/fsys.h>
#include "fatfs.h"
#define DIRBLKSIZ bytes_per_sector
#define LOG2_DIRBLKSIZ log2_bytes_per_sector
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
struct dirrect *entry;
struct dirrect *preventry;
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
const char *name, int namelen, enum lookup_type type,
struct dirstat *ds, ino_t *inum);
static int
fatnamematch (const char *dirname, const char *username, size_t unamelen)
{
char *dn = strdup(dirname);
int dpos = 0;
int upos = 0;
int ext = 0;
if (dn[0] == FAT_DIR_NAME_DELETED || dn[0] == FAT_DIR_NAME_LAST)
return 0;
if (dn[0] == FAT_DIR_NAME_REPLACE_DELETED)
dn[0] = FAT_DIR_NAME_DELETED;
if (!memcmp(dn, FAT_DIR_NAME_DOT, 11))
return unamelen == 1 && username[0] == '.';
if (!memcmp (dn, FAT_DIR_NAME_DOTDOT, 11))
return unamelen == 2 && username[0] == '.' && username[1] == '.';
if (unamelen > 12)
return 0;
do
{
if (dpos == 8 && !ext)
{
if (username[upos] == '.')
{
upos++;
ext = 1;
}
else
break;
}
if (dn[dpos] == ' ')
{
if (ext)
break;
while (dpos < 8 && dn[++dpos] == ' ');
if (username[upos] == '.')
upos++;
ext = 1;
}
else
{
if (tolower(dn[dpos]) == tolower(username[upos]))
{
dpos++;
upos++;
}
else
break;
}
} while (upos < unamelen && dpos < 11);
while (dpos < 11 && dn[dpos] == ' ')
dpos++;
return (upos == unamelen && dpos == 11);
}
error_t
diskfs_lookup_hard (struct node *dp, const char *name, enum lookup_type type,
struct node **npp, struct dirstat *ds, struct protid *cred)
{
error_t err;
ino_t inum;
int namelen;
int spec_dotdot;
struct node *np = 0;
int retry_dotdot = 0;
vm_prot_t prot =
(type == LOOKUP) ? VM_PROT_READ : (VM_PROT_READ | VM_PROT_WRITE);
memory_object_t memobj;
vm_address_t buf = 0;
vm_size_t buflen = 0;
int blockaddr;
int idx, lastidx;
int looped;
if ((type == REMOVE) || (type == RENAME))
assert_backtrace (npp);
if (npp)
*npp = 0;
spec_dotdot = type & SPEC_DOTDOT;
type &= ~SPEC_DOTDOT;
namelen = strlen (name);
if (namelen > FAT_NAME_MAX)
return ENAMETOOLONG;
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
idx = dp->dn->dir_idx;
if (idx << LOG2_DIRBLKSIZ > dp->dn_stat.st_size)
idx = 0;
blockaddr = buf + (idx << LOG2_DIRBLKSIZ);
looped = (idx == 0);
lastidx = idx;
if (lastidx == 0)
lastidx = dp->dn_stat.st_size >> LOG2_DIRBLKSIZ;
while (!looped || idx < lastidx)
{
err = dirscanblock (blockaddr, dp, idx, name, namelen, type, ds, &inum);
if (!err)
{
dp->dn->dir_idx = idx;
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
err = diskfs_cached_lookup_in_dirbuf (inum, &np, buf);
if (err)
goto out;
}
}
else if (dp == diskfs_root_node)
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
err = diskfs_cached_lookup_in_dirbuf (inum, &np, buf);
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
err = diskfs_cached_lookup_in_dirbuf (inum, &np, buf);
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
err = diskfs_cached_lookup_in_dirbuf (inum, &np, buf);
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
ds->idx = dp->dn_stat.st_size >> LOG2_DIRBLKSIZ;
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
const char *name, int namelen, enum lookup_type type,
struct dirstat *ds, ino_t *inum)
{
int nfree = 0;
int needed = 0;
vm_address_t currentoff, prevoff = 0;
struct dirrect *entry = 0;
size_t nbytes = 0;
int looking = 0;
int countcopies = 0;
int consider_compress = 0;
inode_t inode;
vi_key_t entry_key = vi_zero_key;
if (idx == 0 && dp == diskfs_root_node
&& (fatnamematch (FAT_DIR_NAME_DOT, name, namelen)
|| fatnamematch (FAT_DIR_NAME_DOTDOT, name, namelen)))
{
entry_key.dir_inode = diskfs_root_node->cache_id;
currentoff = blockaddr;
}
else
{
if (ds && (ds->stat == LOOKING
|| ds->stat == COMPRESS))
{
looking = 1;
countcopies = 1;
needed = FAT_DIR_RECORDS (namelen);
}
for (currentoff = blockaddr, prevoff = 0;
currentoff < blockaddr + DIRBLKSIZ;
prevoff = currentoff, currentoff += FAT_DIR_REC_LEN)
{
entry = (struct dirrect *)currentoff;
if (looking || countcopies)
{
int thisfree;
if ((char) entry->name[0] == FAT_DIR_NAME_LAST ||
(char) entry->name[0] == FAT_DIR_NAME_DELETED)
thisfree = FAT_DIR_REC_LEN;
else
thisfree = 0;
if (countcopies && currentoff != blockaddr)
nbytes += FAT_DIR_REC_LEN;
if (ds->stat == COMPRESS && nbytes > ds->nbytes)
countcopies = 0;
if (thisfree >= needed)
{
ds->type = CREATE;
ds->stat = TAKE;
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
if (entry->attribute & FAT_DIR_ATTR_LABEL)
continue;
if (fatnamematch ((const char *) entry->name, name, namelen))
break;
}
if (consider_compress
&& ((enum slot_status) ds->type == LOOKING
|| ((enum slot_status) ds->type == COMPRESS &&
ds->nbytes > nbytes)))
{
ds->type = CREATE;
ds->stat = COMPRESS;
ds->entry = (struct dirrect *) blockaddr;
ds->idx = idx;
ds->nbytes = nbytes;
}
}
if (currentoff >= blockaddr + DIRBLKSIZ)
{
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
ds->preventry = (struct dirrect *) prevoff;
}
if (entry_key.dir_inode)
{
*inum = entry_key.dir_inode;
}
else if ((entry->attribute & FAT_DIR_ATTR_DIR)
&& !memcmp (entry->name, FAT_DIR_NAME_DOT, 11))
{
*inum = dp->cache_id;
}
else if ((entry->attribute & FAT_DIR_ATTR_DIR)
&& !memcmp (entry->name, FAT_DIR_NAME_DOTDOT, 11))
{
if (entry->first_cluster_low[0] == 0
&& entry->first_cluster_low[1] == 0
&& entry->first_cluster_high[0] == 0
&& entry->first_cluster_high[1] == 0)
{
*inum = diskfs_root_node->cache_id;
}
else
{
struct vi_key vk = vi_key (dp->dn->inode);
*inum = vk.dir_inode;
}
}
else
{
entry_key.dir_inode = dp->cache_id;
entry_key.dir_offset = (currentoff - blockaddr) + (idx << LOG2_DIRBLKSIZ);
return vi_rlookup(entry_key, inum, &inode, 1);
}
return 0;
}
error_t
diskfs_direnter_hard (struct node *dp, const char *name, struct node *np,
struct dirstat *ds, struct protid *cred)
{
struct dirrect *new;
int namelen = strlen (name);
int needed = FAT_DIR_RECORDS (namelen);
error_t err;
loff_t oldsize = 0;
assert_backtrace (ds->type == CREATE);
assert_backtrace (!diskfs_readonly);
dp->dn_set_mtime = 1;
switch (ds->stat)
{
case TAKE:
assert_backtrace ((char)ds->entry->name[0] == FAT_DIR_NAME_LAST
|| (char)ds->entry->name[0] == FAT_DIR_NAME_DELETED);
new = ds->entry;
break;
case EXTEND:
assert_backtrace (needed <= bytes_per_cluster);
oldsize = dp->dn_stat.st_size;
while (oldsize + bytes_per_cluster > dp->allocsize)
{
err = diskfs_grow (dp, oldsize + bytes_per_cluster, cred);
if (err)
{
munmap ((caddr_t) ds->mapbuf, ds->mapextent);
return err;
}
memset ((caddr_t) ds->mapbuf + oldsize, 0, bytes_per_cluster);
}
new = (struct dirrect *) ((char *) ds->mapbuf + oldsize);
dp->dn_stat.st_size = oldsize + bytes_per_cluster;
dp->dn_set_ctime = 1;
break;
case SHRINK:
case COMPRESS:
default:
assert_backtrace (0);
}
memcpy (new->name, "           ", 11);
memcpy (new->name, name, namelen % 11);
write_word (new->first_cluster_low, np->dn->start_cluster & 0xffff);
write_word (new->first_cluster_high, np->dn->start_cluster >> 16);
write_dword (new->file_size, np->dn_stat.st_size);
if (!(name[0] == '.' && (name[1] == '\0'
|| (name[1] == '.' && name[2] =='\0'))))
{
vi_key_t entry_key;
entry_key.dir_inode = dp->cache_id;
entry_key.dir_offset = (uintptr_t) ds->entry - (uintptr_t) ds->mapbuf;
vi_change (vi_lookup (np->cache_id), entry_key);
if (np->dn_stat.st_mode & S_IFDIR)
new->attribute = FAT_DIR_ATTR_DIR;
}
else
new->attribute = FAT_DIR_ATTR_DIR;
dp->dn_set_mtime = 1;
munmap ((caddr_t) ds->mapbuf, ds->mapextent);
diskfs_file_update (dp, 1);
return 0;
}
error_t
diskfs_dirremove_hard (struct node *dp, struct dirstat *ds)
{
assert_backtrace (ds->type == REMOVE);
assert_backtrace (ds->stat == HERE_TIS);
assert_backtrace (!diskfs_readonly);
dp->dn_set_mtime = 1;
ds->entry->name[0] = FAT_DIR_NAME_DELETED;
dp->dn_set_mtime = 1;
munmap ((caddr_t) ds->mapbuf, ds->mapextent);
diskfs_file_update (dp, 1);
return 0;
}
error_t
diskfs_dirrewrite_hard (struct node *dp, struct node *np, struct dirstat *ds)
{
error_t err;
vi_key_t entry_key;
mach_port_t control = MACH_PORT_NULL;
struct node *oldnp;
ino_t inode;
inode_t vinode;
entry_key.dir_inode = dp->cache_id;
entry_key.dir_offset = (uintptr_t) ds->entry - (uintptr_t) ds->mapbuf;
err = vi_rlookup (entry_key, &inode, &vinode, 0);
assert_backtrace (err != EINVAL);
if (err)
return err;
oldnp = diskfs_cached_ifind (inode);
assert_backtrace (ds->type == RENAME);
assert_backtrace (ds->stat == HERE_TIS);
assert_backtrace (!diskfs_readonly);
oldnp->dn_stat.st_nlink--;
assert_backtrace (!oldnp->dn_stat.st_nlink);
fshelp_fetch_control (&oldnp->transbox, &control);
if (control)
{
fsys_goaway (control, FSYS_GOAWAY_UNLINK);
mach_port_deallocate (mach_task_self (), control);
}
vi_change (vi_lookup (np->cache_id), entry_key);
munmap ((caddr_t) ds->mapbuf, ds->mapextent);
dp->dn_set_mtime = 1;
diskfs_file_update (dp, 1);
return 0;
}
int
diskfs_dirempty (struct node *dp, struct protid *cred)
{
error_t err;
vm_address_t buf = 0, curoff;
struct dirrect *entry;
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
curoff += FAT_DIR_REC_LEN)
{
entry = (struct dirrect *) curoff;
if (entry->name[0] == FAT_DIR_NAME_LAST)
break;
if ((char) entry->name[0] != FAT_DIR_NAME_DELETED
&& memcmp (entry->name, FAT_DIR_NAME_DOT, 11)
&& memcmp (entry->name, FAT_DIR_NAME_DOTDOT, 11))
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
error_t
diskfs_get_directs (struct node *dp,
int entry,
int nentries,
char **data,
mach_msg_type_number_t *datacnt,
vm_size_t bufsiz,
int *amt)
{
vm_size_t allocsize;
struct dirrect *ep;
struct dirent *userp;
int i;
char *datap;
volatile int ouralloc = 0;
error_t err;
vm_prot_t prot = VM_PROT_READ;
memory_object_t memobj;
vm_address_t buf = 0, bufp;
vm_size_t buflen = 0;
allocsize = bufsiz ? round_page (bufsiz) : vm_page_size * 4;
if (allocsize > *datacnt)
{
*data = mmap (0, allocsize, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
ouralloc = 1;
}
memobj = diskfs_get_filemap (dp, prot);
if (memobj == MACH_PORT_NULL)
return errno;
buflen = round_page (dp->dn_stat.st_size);
err = vm_map (mach_task_self (),
&buf, buflen, 0, 1, memobj, 0, 0, prot, prot, 0);
mach_port_deallocate (mach_task_self (), memobj);
if (err)
return err;
bufp = buf;
for (i = 0; i < entry; i ++)
{
if (dp == diskfs_root_node && i < 2)
continue;
ep = (struct dirrect *) bufp;
if (bufp >= buf + buflen || (char)ep->name[0] == FAT_DIR_NAME_LAST)
{
if (allocsize > *datacnt)
munmap (data, allocsize);
munmap ((caddr_t) buf, buflen);
*datacnt = 0;
*amt = 0;
return 0;
}
if ((char)ep->name[0] == FAT_DIR_NAME_DELETED
|| (ep->attribute & FAT_DIR_ATTR_LABEL))
i--;
bufp = bufp + FAT_DIR_REC_LEN;
}
i = 0;
datap = *data;
while (((nentries == -1) || (i < nentries))
&& (!bufsiz || datap - *data < bufsiz)
&& bufp < buf + buflen)
{
char name[13];
size_t namlen, reclen;
struct dirrect dot = { FAT_DIR_NAME_DOT, FAT_DIR_ATTR_DIR };
struct dirrect dotdot = { FAT_DIR_NAME_DOTDOT, FAT_DIR_ATTR_DIR };
if (dp == diskfs_root_node && (i + entry == 0))
ep = &dot;
else if (dp == diskfs_root_node && (i + entry == 1))
ep = &dotdot;
else
ep = (struct dirrect *) bufp;
if ((char)ep->name[0] == FAT_DIR_NAME_LAST)
{
bufp = buf + buflen;
continue;
}
if ((char)ep->name[0] == FAT_DIR_NAME_DELETED || (ep->attribute & FAT_DIR_ATTR_LABEL))
{
bufp = bufp + FAT_DIR_REC_LEN;
continue;
}
fat_to_unix_filename ((const char *) ep->name, name);
namlen = strlen(name);
reclen = sizeof (struct dirent) + namlen;
reclen = (reclen + 3) & ~3;
if (datap - *data + reclen > allocsize)
{
vm_address_t newdata;
vm_allocate (mach_task_self (), &newdata,
(ouralloc
? (allocsize *= 2)
: (allocsize = vm_page_size * 2)), 1);
memcpy ((void *) newdata, (void *) *data, datap - *data);
if (ouralloc)
munmap (*data, allocsize / 2);
datap = (char *) newdata + (datap - *data);
*data = (char *) newdata;
ouralloc = 1;
}
userp = (struct dirent *) datap;
{
ino_t inode;
inode_t v_inode;
vi_key_t entry_key;
entry_key.dir_inode = dp->cache_id;
entry_key.dir_offset = bufp - buf;
vi_rlookup (entry_key, &inode, &v_inode, 1);
userp->d_fileno = inode;
}
userp->d_type = DT_UNKNOWN;
userp->d_reclen = reclen;
userp->d_namlen = namlen;
memcpy (userp->d_name, name, namlen);
userp->d_name[namlen] = '\0';
datap = datap + reclen;
if (!(dp == diskfs_root_node && i + entry < 2))
bufp = bufp + FAT_DIR_REC_LEN;
i++;
}
if (ouralloc
&& round_page (datap - *data) < round_page (allocsize))
munmap ((caddr_t) round_page (datap),
round_page (allocsize) - round_page (datap - *data));
munmap ((caddr_t) buf, buflen);
*amt = i;
*datacnt = datap - *data;
return 0;
}