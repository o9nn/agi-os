#include <stddef.h>
#include <string.h>
#include <dirent.h>
#include <pwd.h>
#include <sys/mman.h>
#include "usermux.h"
#define DIRENTS_CHUNK_SIZE (128*1024)
#define DIRENTS_CACHE_TIME 90
#define DIRENT_ALIGN 4
#define DIRENT_NAME_OFFS offsetof (struct dirent, d_name)
#define DIRENT_LEN(name_len) \
((DIRENT_NAME_OFFS + (name_len) + 1 + (DIRENT_ALIGN - 1)) \
& ~(DIRENT_ALIGN - 1))
static error_t lookup_user (struct usermux *mux, const char *user,
struct node **node);
error_t
netfs_attempt_lookup (struct iouser *user, struct node *dir,
const char *name, struct node **node)
{
error_t err;
if (dir->nn->name)
err = ENOTDIR;
else
err = lookup_user (dir->nn->mux, name, node);
fshelp_touch (&dir->nn_stat, TOUCH_ATIME, usermux_maptime);
pthread_mutex_unlock (&dir->lock);
if (! err)
pthread_mutex_lock (&(*node)->lock);
return err;
}
static error_t
get_dirents (struct node *dir,
int first_entry, int max_entries, char **data,
mach_msg_type_number_t *data_len,
vm_size_t max_data_len, int *data_entries)
{
error_t err = 0;
if (dir->nn->name)
return ENOTDIR;
setpwent ();
while (first_entry-- > 0)
if (! getpwent ())
{
max_entries = 0;
break;
}
if (max_entries != 0)
{
size_t size = (max_data_len == 0 ? DIRENTS_CHUNK_SIZE : max_data_len);
*data = mmap (0, size, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
err = (data != (void *) -1) ? errno : 0;
if (! err)
{
struct passwd *pw;
char *p = *data;
int count = 0;
int entry_type =
(S_ISLNK (dir->nn->mux->stat_template.st_mode) ? DT_LNK : DT_REG);
while ((max_entries == -1 || count < max_entries)
&& (pw = getpwent ()))
{
struct dirent hdr;
size_t name_len = strlen (pw->pw_name);
size_t sz = DIRENT_LEN (name_len);
if ((p - *data) + sz > size)
{
if (max_data_len > 0)
break;
else
{
vm_address_t extension = (vm_address_t)(*data + size);
err = vm_allocate (mach_task_self (), &extension,
DIRENTS_CHUNK_SIZE, 0);
if (err)
break;
size += DIRENTS_CHUNK_SIZE;
}
}
hdr.d_namlen = name_len;
hdr.d_fileno = pw->pw_uid + USERMUX_FILENO_UID_OFFSET;
hdr.d_reclen = sz;
hdr.d_type = entry_type;
memcpy (p, &hdr, DIRENT_NAME_OFFS);
strcpy (p + DIRENT_NAME_OFFS, pw->pw_name);
p += sz;
count++;
}
if (err)
munmap (*data, size);
else
{
vm_address_t alloc_end = (vm_address_t)(*data + size);
vm_address_t real_end = round_page (p);
if (alloc_end > real_end)
munmap ((caddr_t) real_end, alloc_end - real_end);
*data_len = p - *data;
*data_entries = count;
}
}
}
endpwent ();
return err;
}
error_t
netfs_get_dirents (struct iouser *cred, struct node *dir,
int first_entry, int max_entries, char **data,
mach_msg_type_number_t *data_len,
vm_size_t max_data_len, int *data_entries)
{
error_t err;
static time_t cache_timestamp = 0;
static pthread_rwlock_t cache_lock = PTHREAD_RWLOCK_INITIALIZER;
static char *cached_data = 0;
static mach_msg_type_number_t cached_data_len = 0;
static int cached_data_entries = 0;
struct timeval tv;
char *first;
size_t bytes_left, entries_left;
maptime_read (usermux_maptime, &tv);
if (tv.tv_sec > cache_timestamp + DIRENTS_CACHE_TIME)
{
pthread_rwlock_wrlock (&cache_lock);
if (cached_data_len > 0)
{
munmap (cached_data, cached_data_len);
cached_data = 0;
cached_data_len = 0;
}
err = get_dirents (dir, 0, -1, &cached_data, &cached_data_len, 0,
&cached_data_entries);
if (! err)
cache_timestamp = tv.tv_sec;
pthread_rwlock_unlock (&cache_lock);
if (err)
return err;
}
pthread_rwlock_rdlock (&cache_lock);
first = cached_data;
bytes_left = cached_data_len;
entries_left = cached_data_entries;
while (first_entry > 0)
{
struct dirent *e = (struct dirent *)first;
if (entries_left == 0)
{
pthread_rwlock_unlock (&cache_lock);
return EINVAL;
}
first += e->d_reclen;
bytes_left -= e->d_reclen;
entries_left--;
}
if ((max_data_len > 0 && max_data_len < bytes_left)
|| (max_entries > 0 && max_entries < entries_left))
{
char *lim = first;
int entries = 0;
while (entries_left > 0
&& max_entries > 0
&& max_data_len > ((struct dirent *)lim)->d_reclen)
{
size_t reclen = ((struct dirent *)lim)->d_reclen;
max_data_len -= reclen;
max_entries--;
entries++;
lim += reclen;
}
bytes_left = (lim - first);
entries_left = entries;
}
*data_len = bytes_left;
*data_entries = entries_left;
*data = mmap (0, bytes_left, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
err = (*data == (void *) -1) ? errno : 0;
if (! err)
bcopy (cached_data, *data, bytes_left);
pthread_rwlock_unlock (&cache_lock);
fshelp_touch (&dir->nn_stat, TOUCH_ATIME, usermux_maptime);
return err;
}
static void
free_name (struct usermux_name *nm)
{
free ((char *)nm->name);
free (nm);
}
static int
lookup_cached (struct usermux *mux, const char *user, int purge,
struct node **node)
{
struct usermux_name *nm = mux->names, **prevl = &mux->names;
while (nm)
{
struct usermux_name *next = nm->next;
if (strcasecmp (user, nm->name) == 0)
{
if (nm->node)
netfs_nref (nm->node);
if (nm->node)
{
*node = nm->node;
return 1;
}
}
if (purge && !nm->node)
{
*prevl = nm->next;
free_name (nm);
}
else
prevl = &nm->next;
nm = next;
}
return 0;
}
static error_t
lookup_pwent (struct usermux *mux, const char *user, struct passwd *pw,
struct node **node)
{
error_t err;
struct usermux_name *nm = malloc (sizeof (struct usermux_name));
if (! nm)
return ENOMEM;
nm->name = strdup (user);
err = create_user_node (mux, nm, pw, node);
if (err)
{
free_name (nm);
return err;
}
pthread_rwlock_wrlock (&mux->names_lock);
if (lookup_cached (mux, user, 1, node))
{
pthread_rwlock_unlock (&mux->names_lock);
nm->node->nn->name = 0;
netfs_nrele (nm->node);
free_name (nm);
}
else
{
nm->next = mux->names;
mux->names = nm;
pthread_rwlock_unlock (&mux->names_lock);
}
return 0;
}
static error_t
lookup_user (struct usermux *mux, const char *user, struct node **node)
{
int was_cached;
struct passwd _pw, *pw;
char pwent_data[2048];
pthread_rwlock_rdlock (&mux->names_lock);
was_cached = lookup_cached (mux, user, 0, node);
pthread_rwlock_unlock (&mux->names_lock);
if (was_cached)
return 0;
else
{
if (getpwnam_r (user, &_pw, pwent_data, sizeof pwent_data, &pw))
return ENOENT;
if (pw == NULL)
return ENOENT;
return lookup_pwent (mux, user, pw, node);
}
}
error_t
netfs_attempt_syncfs (struct iouser *cred, int wait)
{
return 0;
}
error_t
netfs_attempt_chown (struct iouser *cred, struct node *node, uid_t uid, uid_t gid)
{
if (node->nn->name)
return EOPNOTSUPP;
else
{
struct usermux *mux = node->nn->mux;
error_t err = file_chown (mux->underlying, uid, gid);
if (! err)
{
struct usermux_name *nm;
mux->stat_template.st_uid = uid;
mux->stat_template.st_gid = gid;
node->nn_stat.st_uid = uid;
node->nn_stat.st_gid = gid;
pthread_rwlock_rdlock (&mux->names_lock);
for (nm = mux->names; nm; nm = nm->next)
if (nm->node)
{
nm->node->nn_stat.st_uid = uid;
nm->node->nn_stat.st_gid = gid;
}
pthread_rwlock_unlock (&mux->names_lock);
fshelp_touch (&node->nn_stat, TOUCH_CTIME, usermux_maptime);
}
return err;
}
}
error_t
netfs_attempt_chauthor (struct iouser *cred, struct node *node, uid_t author)
{
if (node->nn->name)
return EOPNOTSUPP;
else
{
struct usermux *mux = node->nn->mux;
error_t err = file_chauthor (mux->underlying, author);
if (! err)
{
struct usermux_name *nm;
mux->stat_template.st_author = author;
node->nn_stat.st_author = author;
pthread_rwlock_rdlock (&mux->names_lock);
for (nm = mux->names; nm; nm = nm->next)
if (nm->node)
nm->node->nn_stat.st_author = author;
pthread_rwlock_unlock (&mux->names_lock);
fshelp_touch (&node->nn_stat, TOUCH_CTIME, usermux_maptime);
}
return err;
}
}
error_t
netfs_attempt_chmod (struct iouser *cred, struct node *node, mode_t mode)
{
mode &= ~S_ITRANS;
if ((mode & S_IFMT) == 0)
mode |= (node->nn_stat.st_mode & S_IFMT);
if (node->nn->name || ((mode & S_IFMT) != (node->nn_stat.st_mode & S_IFMT)))
return EOPNOTSUPP;
else
{
error_t err = file_chmod (node->nn->mux->underlying, mode & ~S_IFMT);
if (! err)
{
node->nn_stat.st_mode = mode;
fshelp_touch (&node->nn_stat, TOUCH_CTIME, usermux_maptime);
}
return err;
}
}