#include <string.h>
#include <stdlib.h>
#include <dirent.h>
#include "isofs.h"
int use_file_start_id (struct dirrect *record, struct rrip_lookup *rr);
static error_t dirscanblock (void *, const char *, size_t,
struct dirrect **, struct rrip_lookup *);
static int
isonamematch (const char *dirname, size_t dnamelen,
const char *username, size_t unamelen)
{
if (dnamelen == 1 && dirname[0] == '\0')
return unamelen == 1 && username[0] == '.';
if (dnamelen == 1 && dirname[0] == '\1')
return unamelen == 2 && username[0] == '.' && username[1] == '.';
if (unamelen > dnamelen)
return 0;
if (!strncasecmp (dirname, username, unamelen))
{
if (dnamelen == unamelen)
return 1;
if (dirname[unamelen] == ';')
return 1;
if (dirname[unamelen] == '.'
&& (dirname[unamelen+1] == '\0' || dirname[unamelen+1] == ';'))
return 1;
}
return 0;
}
error_t
diskfs_lookup_hard (struct node *dp, const char *name, enum lookup_type type,
struct node **npp, struct dirstat *ds, struct protid *cred)
{
error_t err = ENOENT;
struct lookup_context ctx;
int namelen;
int spec_dotdot;
void *buf;
void *blockaddr;
ino_t id;
if ((type == REMOVE) || (type == RENAME))
assert_backtrace (npp);
if (npp)
*npp = 0;
spec_dotdot = type & SPEC_DOTDOT;
type &= ~SPEC_DOTDOT;
namelen = strlen (name);
if (type == RENAME)
return EROFS;
buf = disk_image + (dp->dn->file_start << store->log2_block_size);
for (blockaddr = buf;
blockaddr < buf + dp->dn_stat.st_size;
blockaddr += logical_sector_size)
{
err = dirscanblock (blockaddr, name, namelen, &ctx.dr, &ctx.rr);
if (!err)
break;
if (err != ENOENT)
return err;
}
if ((!err && type == REMOVE)
|| (err == ENOENT && type == CREATE))
err = EROFS;
if (err)
return err;
err = cache_id (ctx.dr, &ctx.rr, &id);
if (err)
return err;
if (namelen == 2 && name[0] == '.' && name[1] == '.')
{
if (dp == diskfs_root_node)
err = EAGAIN;
else if (spec_dotdot)
{
assert_backtrace (type == LOOKUP);
diskfs_nput (dp);
err = diskfs_cached_lookup_context (id, npp, &ctx);
}
else
{
pthread_mutex_unlock (&dp->lock);
err = diskfs_cached_lookup_context (id, npp, &ctx);
pthread_mutex_lock (&dp->lock);
}
}
else if (namelen == 1 && name[0] == '.')
{
*npp = dp;
diskfs_nref (dp);
}
else
err = diskfs_cached_lookup_context (id, npp, &ctx);
release_rrip (&ctx.rr);
return err;
}
static error_t
dirscanblock (void *blkaddr, const char *name, size_t namelen,
struct dirrect **record, struct rrip_lookup *rr)
{
struct dirrect *entry;
void *currentoff;
size_t reclen;
size_t entry_namelen;
int matchrr;
int matchnormal;
for (currentoff = blkaddr;
currentoff < blkaddr + logical_sector_size;
currentoff += reclen)
{
entry = (struct dirrect *) currentoff;
reclen = entry->len;
if (reclen == 0
|| reclen < sizeof (struct dirrect)
|| currentoff + reclen > blkaddr + logical_sector_size)
break;
entry_namelen = entry->namelen;
if (reclen < sizeof (struct dirrect) + entry_namelen)
break;
if (isonamematch ((const char *) entry->name, entry_namelen, name, namelen))
matchnormal = 1;
else
matchnormal = 0;
matchrr = rrip_match_lookup (entry, name, namelen, rr);
if (rr->valid & VALID_RE)
{
release_rrip (rr);
continue;
}
if (((rr->valid & VALID_NM) && matchrr)
|| (!(rr->valid & VALID_NM) && matchnormal))
{
*record = entry;
return 0;
}
release_rrip (rr);
}
*record = 0;
return ENOENT;
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
volatile vm_size_t allocsize;
struct dirrect *ep;
struct dirent *userp;
int i;
void *dirbuf, *bufp;
char *datap;
volatile int ouralloc = 0;
error_t err;
allocsize = bufsiz ? round_page (bufsiz) : vm_page_size * 4;
if (allocsize > *datacnt)
{
*data = mmap (0, allocsize, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
ouralloc = 1;
}
err = diskfs_catch_exception ();
if (err)
{
if (ouralloc)
munmap (*data, allocsize);
return err;
}
dirbuf = disk_image + (dp->dn->file_start << store->log2_block_size);
bufp = dirbuf;
for (i = 0; i < entry; i ++)
{
struct rrip_lookup rr;
ep = (struct dirrect *) bufp;
rrip_lookup (ep, &rr, 0);
if (rr.valid & VALID_RE)
i--;
else
{
if (bufp - dirbuf >= dp->dn_stat.st_size)
{
release_rrip (&rr);
if (allocsize > *datacnt)
munmap (data, allocsize);
*datacnt = 0;
*amt = 0;
return 0;
}
}
bufp = bufp + ep->len;
release_rrip (&rr);
if (*(char *)bufp == '\0')
bufp = (void *) (((long) bufp & ~(logical_sector_size - 1))
+ logical_sector_size);
}
i = 0;
datap = *data;
while (((nentries == -1) || (i < nentries))
&& (!bufsiz || datap - *data < bufsiz)
&& ((void *) bufp - dirbuf < dp->dn_stat.st_size))
{
struct rrip_lookup rr;
const char *name;
size_t namlen, reclen;
ep = (struct dirrect *) bufp;
rrip_lookup (ep, &rr, 0);
if (! (rr.valid & VALID_RE))
{
name = rr.valid & VALID_NM ? rr.name : (char *) ep->name;
namlen = rr.valid & VALID_NM ? strlen (name) : ep->namelen;
if (!(rr.valid & VALID_NM))
{
if (namlen == 1 && name[0] == '\0')
{
name = ".";
namlen = 1;
}
else if (namlen == 1 && name[0] == '\1')
{
name = "..";
namlen = 2;
}
}
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
if (use_file_start_id (ep, &rr))
{
off_t file_start;
err = calculate_file_start (ep, &file_start, &rr);
if (err)
{
release_rrip (&rr);
diskfs_end_catch_exception ();
if (ouralloc)
munmap (*data, allocsize);
return err;
}
userp->d_fileno = file_start << store->log2_block_size;
}
else
userp->d_fileno = (ino_t) ((void *) ep - (void *) disk_image);
userp->d_type = DT_UNKNOWN;
userp->d_reclen = reclen;
userp->d_namlen = namlen;
memcpy (userp->d_name, name, namlen);
userp->d_name[namlen] = '\0';
datap = datap + reclen;
i++;
}
release_rrip (&rr);
bufp = bufp + ep->len;
if (*(char *)bufp == '\0')
bufp = (void *) (((long) bufp & ~(logical_sector_size - 1))
+ logical_sector_size);
}
diskfs_end_catch_exception ();
if (ouralloc
&& round_page (datap - *data) < round_page (allocsize))
munmap ((caddr_t) round_page (datap),
round_page (allocsize) - round_page (datap - *data));
*amt = i;
*datacnt = datap - *data;
return 0;
}
const size_t diskfs_dirstat_size = 0;
void
diskfs_null_dirstat (struct dirstat *ds)
{
}
error_t
diskfs_drop_dirstat (struct node *dp, struct dirstat *ds)
{
return 0;
}
error_t
diskfs_direnter_hard(struct node *dp,
const char *name,
struct node *np,
struct dirstat *ds,
struct protid *cred)
{
abort ();
}
error_t
diskfs_dirremove_hard(struct node *dp,
struct dirstat *ds)
{
abort ();
}
error_t
diskfs_dirrewrite_hard(struct node *dp,
struct node *np,
struct dirstat *ds)
{
abort ();
}
int
diskfs_dirempty(struct node *dp,
struct protid *cred)
{
abort ();
}