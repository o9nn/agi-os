#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <unistd.h>
#include <locale.h>
#include <argp.h>
#include <argz.h>
#include <error.h>
#include <string.h>
#include <fcntl.h>
#include <dirent.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <maptime.h>
#include <pthread.h>
#include <version.h>
#include <mach.h>
#include <hurd/netfs.h>
#include <hurd/ioctl_types.h>
#include <hurd/console.h>
#include "display.h"
#include "input.h"
#include "fs_notify_U.h"
#include "libnetfs/fs_S.h"
#include "libnetfs/io_S.h"
#include "tioctl_S.h"
const char *argp_program_version = STANDARD_HURD_VERSION (console);
char *netfs_server_name = "console";
char *netfs_server_version = HURD_VERSION;
int netfs_maxsymlinks = 16;
volatile struct mapped_time_value *console_maptime;
#define DEFAULT_ENCODING "UTF-8"
#define DEFAULT_INTENSITY CONS_ATTR_INTENSITY_NORMAL
#define DEFAULT_UNDERLINED 0
#define DEFAULT_BLINKING 0
#define DEFAULT_REVERSED 0
#define DEFAULT_CONCEALED 0
#define DEFAULT_WIDTH 80
#define DEFAULT_HEIGHT 25
#define DEFAULT_LINES 50
#define STRX(s) #s
#define STR(s)	STRX(s)
#define DEFAULT_ATTRIBUTE_NAME "normal"
#define DEFAULT_FOREGROUND CONS_COLOR_WHITE
#define DEFAULT_FOREGROUND_NAME "white"
#define DEFAULT_BACKGROUND CONS_COLOR_BLACK
#define DEFAULT_BACKGROUND_NAME "black"
typedef struct cons *cons_t;
typedef struct vcons *vcons_t;
struct vcons
{
vcons_t next;
vcons_t prev;
int refcnt;
int id;
char *name;
cons_t cons;
display_t display;
input_t input;
pthread_mutex_t lock;
struct node *dir_node;
struct node *cons_node;
struct node *disp_node;
struct node *inpt_node;
};
struct modreq
{
mach_port_t port;
struct modreq *next;
};
struct cons
{
pthread_mutex_t lock;
vcons_t vcons_list;
char *encoding;
conchar_attr_t attribute;
struct modreq *dirmod_reqs;
unsigned int dirmod_tick;
struct node *node;
mach_port_t underlying;
struct stat stat_template;
unsigned int lines;
unsigned int width;
unsigned int height;
};
static void
cons_notice_dirchange (cons_t cons, dir_changed_type_t type, char *name)
{
error_t err;
struct modreq **preq = &cons->dirmod_reqs;
cons->dirmod_tick++;
while (*preq)
{
struct modreq *req = *preq;
err = dir_changed (req->port, cons->dirmod_tick, type, name);
if (err && err != MACH_SEND_TIMEOUT)
{
*preq = req->next;
mach_port_deallocate (mach_task_self (), req->port);
free (req);
}
else
preq = &req->next;
}
}
error_t
vcons_lookup (cons_t cons, int id, int create, vcons_t *r_vcons)
{
error_t err;
vcons_t previous_vcons = 0;
vcons_t vcons;
if (!id && !create)
return EINVAL;
pthread_mutex_lock (&cons->lock);
if (id)
{
if (cons->vcons_list && cons->vcons_list->id <= id)
{
previous_vcons = cons->vcons_list;
while (previous_vcons->next && previous_vcons->next->id <= id)
previous_vcons = previous_vcons->next;
if (previous_vcons->id == id)
{
previous_vcons->refcnt++;
pthread_mutex_unlock (&cons->lock);
*r_vcons = previous_vcons;
return 0;
}
}
else if (!create)
{
pthread_mutex_unlock (&cons->lock);
return ESRCH;
}
}
else
{
id = 1;
if (cons->vcons_list && cons->vcons_list->id == 1)
{
previous_vcons = cons->vcons_list;
while (previous_vcons && previous_vcons->id == id)
{
id++;
previous_vcons = previous_vcons->next;
}
}
}
vcons = calloc (1, sizeof (struct vcons));
if (!vcons)
{
pthread_mutex_unlock (&cons->lock);
return ENOMEM;
}
vcons->cons = cons;
vcons->refcnt = 1;
vcons->id = id;
asprintf (&vcons->name, "%i", id);
pthread_mutex_init (&vcons->lock, NULL);
err = display_create (&vcons->display, cons->encoding ?: DEFAULT_ENCODING,
cons->attribute, cons->lines, cons->width,
cons->height);
if (err)
{
free (vcons->name);
free (vcons);
pthread_mutex_unlock (&cons->lock);
return err;
}
err = input_create (&vcons->input, cons->encoding ?: DEFAULT_ENCODING);
if (err)
{
display_destroy (vcons->display);
free (vcons->name);
free (vcons);
pthread_mutex_unlock (&cons->lock);
return err;
}
if (previous_vcons)
{
vcons->prev = previous_vcons;
if (previous_vcons->next)
{
previous_vcons->next->prev = vcons;
vcons->next =  previous_vcons->next;
}
previous_vcons->next = vcons;
}
else
{
if (cons->vcons_list)
{
cons->vcons_list->prev = vcons;
vcons->next = cons->vcons_list;
}
cons->vcons_list = vcons;
}
cons_notice_dirchange (cons, DIR_CHANGED_NEW, vcons->name);
pthread_mutex_unlock (&cons->lock);
*r_vcons = vcons;
return 0;
}
void
vcons_ref (vcons_t vcons)
{
cons_t cons = vcons->cons;
pthread_mutex_lock (&cons->lock);
vcons->refcnt++;
pthread_mutex_unlock (&cons->lock);
}
void
vcons_release (vcons_t vcons)
{
cons_t cons = vcons->cons;
pthread_mutex_lock (&cons->lock);
if (!--vcons->refcnt)
{
if (vcons->prev)
vcons->prev->next = vcons->next;
else
cons->vcons_list = vcons->next;
if (vcons->next)
vcons->next->prev = vcons->prev;
cons_notice_dirchange (cons, DIR_CHANGED_UNLINK, vcons->name);
display_destroy (vcons->display);
input_destroy (vcons->input);
free (vcons->name);
free (vcons);
}
pthread_mutex_unlock (&cons->lock);
}
struct netnode
{
cons_t cons;
vcons_t vcons;
};
typedef enum
{
VCONS_NODE_DIR = 0,
VCONS_NODE_CONSOLE,
VCONS_NODE_DISPLAY,
VCONS_NODE_INPUT
} vcons_node_type;
static error_t
new_node (struct node **np, vcons_t vcons, vcons_node_type type)
{
struct netnode *nn = calloc (1, sizeof *nn);
if (nn == 0)
return ENOMEM;
nn->vcons = vcons;
*np = netfs_make_node (nn);
if (*np == 0)
{
free (nn);
return ENOMEM;
}
(*np)->nn_stat = vcons->cons->stat_template;
(*np)->nn_translated = 0;
switch (type)
{
case VCONS_NODE_DIR:
(*np)->nn_stat.st_ino = vcons->id << 2;
(*np)->nn_stat.st_mode |= S_IFDIR;
(*np)->nn_stat.st_size = 0;
break;
case VCONS_NODE_CONSOLE:
(*np)->nn_stat.st_ino = (vcons->id << 2) + 1;
(*np)->nn_stat.st_mode |= S_IFCHR;
(*np)->nn_stat.st_mode &= ~(S_IXUSR | S_IXGRP | S_IXOTH);
(*np)->nn_stat.st_size = 0;
break;
case VCONS_NODE_DISPLAY:
(*np)->nn_stat.st_ino = (vcons->id << 2) + 2;
(*np)->nn_stat.st_mode |= S_IFREG;
(*np)->nn_stat.st_mode &= ~(S_IXUSR | S_IXGRP | S_IXOTH);
(*np)->nn_stat.st_size = display_get_size (vcons->display);
break;
case VCONS_NODE_INPUT:
(*np)->nn_stat.st_ino = (vcons->id << 2) + 3;
(*np)->nn_stat.st_mode |= S_IFIFO;
(*np)->nn_stat.st_mode &= ~(S_IXUSR | S_IXGRP | S_IXOTH);
(*np)->nn_stat.st_size = 0;
break;
}
if (! S_ISDIR (vcons->cons->stat_template.st_mode)
&& S_ISDIR ((*np)->nn_stat.st_mode))
{
if (vcons->cons->stat_template.st_mode & S_IRUSR)
(*np)->nn_stat.st_mode |= S_IXUSR;
if (vcons->cons->stat_template.st_mode & S_IRGRP)
(*np)->nn_stat.st_mode |= S_IXGRP;
if (vcons->cons->stat_template.st_mode & S_IROTH)
(*np)->nn_stat.st_mode |= S_IXOTH;
}
fshelp_touch (&(*np)->nn_stat, TOUCH_ATIME|TOUCH_MTIME|TOUCH_CTIME,
console_maptime);
return 0;
}
void
netfs_try_dropping_softrefs (struct node *np)
{
vcons_t vcons = np->nn->vcons;
int release = FALSE;
pthread_mutex_lock (&vcons->lock);
if (np == vcons->dir_node)
{
release = TRUE;
vcons->dir_node = 0;
}
else if (np == vcons->cons_node)
{
release = TRUE;
vcons->cons_node = 0;
}
else if (np == vcons->disp_node)
{
release = TRUE;
vcons->disp_node = 0;
}
else if (np == vcons->inpt_node)
{
release = TRUE;
vcons->inpt_node = 0;
}
if (release)
netfs_nrele_light (np);
pthread_mutex_unlock (&vcons->lock);
if (release)
vcons_release (vcons);
}
void
netfs_node_norefs (struct node *np)
{
assert_backtrace (!np->nn->cons && np->nn->vcons);
free (np->nn);
free (np);
}
error_t
netfs_attempt_create_file (struct iouser *user, struct node *dir,
const char *name, mode_t mode, struct node **np)
{
*np = 0;
pthread_mutex_unlock (&dir->lock);
return EOPNOTSUPP;
}
error_t
netfs_check_open_permissions (struct iouser *user, struct node *node,
int flags, int newnode)
{
error_t err = 0;
if (flags & O_READ)
err = fshelp_access (&node->nn_stat, S_IREAD, user);
if (!err && (flags & O_WRITE))
err = fshelp_access (&node->nn_stat, S_IWRITE, user);
if (!err && (flags & O_EXEC))
err = fshelp_access (&node->nn_stat, S_IEXEC, user);
return err;
}
error_t
netfs_attempt_utimes (struct iouser *cred, struct node *node,
struct timespec *atime, struct timespec *mtime)
{
error_t err = fshelp_isowner (&node->nn_stat, cred);
int flags = TOUCH_CTIME;
if (! err)
{
if (mtime)
node->nn_stat.st_mtim = *mtime;
if (atime)
node->nn_stat.st_atim = *atime;
fshelp_touch (&node->nn_stat, flags, console_maptime);
}
return err;
}
error_t
netfs_report_access (struct iouser *cred, struct node *node, int *types)
{
*types = 0;
if (fshelp_access (&node->nn_stat, S_IREAD, cred) == 0)
*types |= O_READ;
if (fshelp_access (&node->nn_stat, S_IWRITE, cred) == 0)
*types |= O_WRITE;
if (fshelp_access (&node->nn_stat, S_IEXEC, cred) == 0)
*types |= O_EXEC;
return 0;
}
error_t
netfs_validate_stat (struct node *np, struct iouser *cred)
{
return 0;
}
error_t
netfs_attempt_sync (struct iouser *cred, struct node *np, int wait)
{
return 0;
}
error_t
netfs_attempt_lookup (struct iouser *user, struct node *dir,
const char *name, struct node **node)
{
error_t err;
*node = 0;
err = fshelp_access (&dir->nn_stat, S_IEXEC, user);
if (err)
goto out;
if (strcmp (name, ".") == 0)
{
netfs_nref (dir);
*node = dir;
goto out;
}
if (strcmp (name, "..") == 0)
{
if (dir->nn->cons)
err = EAGAIN;
else
{
netfs_nref (netfs_root_node);
*node = netfs_root_node;
}
goto out;
}
if (dir->nn->cons)
{
vcons_t vcons;
int release_vcons = 0;
char *tail = NULL;
int id;
errno = 0;
id = strtol (name, &tail, 0);
if ((tail && *tail) || errno)
{
err = ENOENT;
goto out;
}
err = vcons_lookup (dir->nn->cons, id, 1, &vcons);
if (err == ESRCH || err == EINVAL)
err = ENOENT;
if (err)
goto out;
pthread_mutex_lock (&vcons->lock);
if (vcons->dir_node)
{
*node = vcons->dir_node;
netfs_nref (*node);
release_vcons = 1;
}
else
{
err = new_node (node, vcons, VCONS_NODE_DIR);
if (!err)
{
vcons->dir_node = *node;
netfs_nref_light (*node);
}
else
release_vcons = 1;
}
pthread_mutex_unlock (&vcons->lock);
if (release_vcons)
vcons_release (vcons);
}
else
{
vcons_t vcons = dir->nn->vcons;
int ref_vcons = 0;
assert_backtrace (dir == vcons->dir_node);
if (!strcmp (name, "console"))
{
pthread_mutex_lock (&vcons->lock);
if (vcons->cons_node)
{
*node = vcons->cons_node;
netfs_nref (*node);
}
else
{
err = new_node (node, vcons, VCONS_NODE_CONSOLE);
if (!err)
{
vcons->cons_node = *node;
netfs_nref_light (*node);
ref_vcons = 1;
}
}
pthread_mutex_unlock (&vcons->lock);
}
else if (!strcmp (name, "display"))
{
pthread_mutex_lock (&vcons->lock);
if (vcons->disp_node)
{
*node = vcons->disp_node;
netfs_nref (*node);
}
else
{
err = new_node (node, vcons, VCONS_NODE_DISPLAY);
if (!err)
{
vcons->disp_node = *node;
netfs_nref_light (*node);
ref_vcons = 1;
}
}
pthread_mutex_unlock (&vcons->lock);
}
else if (!strcmp (name, "input"))
{
pthread_mutex_lock (&vcons->lock);
if (vcons->inpt_node)
{
*node = vcons->inpt_node;
netfs_nref (*node);
}
else
{
err = new_node (node, vcons, VCONS_NODE_INPUT);
if (!err)
{
vcons->inpt_node = *node;
netfs_nref_light (*node);
ref_vcons = 1;
}
}
pthread_mutex_unlock (&vcons->lock);
}
else
err = ENOENT;
if (ref_vcons)
vcons_ref (vcons);
}
if (!err)
fshelp_touch (&dir->nn_stat, TOUCH_ATIME, console_maptime);
out:
pthread_mutex_unlock (&dir->lock);
if (err)
*node = 0;
else
pthread_mutex_lock (&(*node)->lock);
return err;
}
#define DIRENT_ALIGN 4
#define DIRENT_NAME_OFFS offsetof (struct dirent, d_name)
#define DIRENT_LEN(name_len)                                                  \
((DIRENT_NAME_OFFS + (name_len) + 1 + (DIRENT_ALIGN - 1))                   \
& ~(DIRENT_ALIGN - 1))
error_t
netfs_get_dirents (struct iouser *cred, struct node *dir,
int first_entry, int num_entries, char **data,
mach_msg_type_number_t *data_len,
vm_size_t max_data_len, int *data_entries)
{
error_t err = 0;
int count = 0;
size_t size = 0;
struct vcons *first_vcons = NULL;
struct vcons *vcons;
int bump_size (const char *name)
{
if (num_entries == -1 || count < num_entries)
{
size_t new_size = size + DIRENT_LEN (strlen (name));
if (max_data_len > 0 && new_size > max_data_len)
return 0;
size = new_size;
count++;
return 1;
}
else
return 0;
}
if (!dir->nn->cons && !(dir == dir->nn->vcons->dir_node))
return ENOTDIR;
if (dir->nn->cons)
{
pthread_mutex_lock (&dir->nn->cons->lock);
for (first_vcons = dir->nn->cons->vcons_list, count = 2;
first_vcons && first_entry > count;
first_vcons = first_vcons->next)
count++;
count = 0;
}
if (first_entry == 0)
bump_size (".");
if (first_entry <= 1)
bump_size ("..");
if (dir->nn->cons)
{
for (vcons = first_vcons; vcons; vcons = vcons->next)
if (!bump_size (vcons->name))
break;
}
else
{
if (first_entry <= 2)
bump_size ("console");
if (first_entry <= 3)
bump_size ("display");
if (first_entry <= 4)
bump_size ("input");
}
if (!size)
*data_len = *data_entries = 0;
else
{
*data = mmap (0, size, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
err = ((void *) *data == (void *) -1) ? errno : 0;
}
if (size && !err)
{
char *p = *data;
int add_dir_entry (const char *name, ino_t fileno, int type)
{
if (num_entries == -1 || count < num_entries)
{
struct dirent hdr;
size_t name_len = strlen (name);
size_t sz = DIRENT_LEN (name_len);
if (sz > size)
return 0;
else
size -= sz;
hdr.d_fileno = fileno;
hdr.d_reclen = sz;
hdr.d_type = type;
hdr.d_namlen = name_len;
memcpy (p, &hdr, DIRENT_NAME_OFFS);
strcpy (p + DIRENT_NAME_OFFS, name);
p += sz;
count++;
return 1;
}
else
return 0;
}
*data_len = size;
*data_entries = count;
count = 0;
if (dir->nn->cons)
{
if (first_entry == 0)
add_dir_entry (".", 2, DT_DIR);
if (first_entry <= 1)
add_dir_entry ("..", 2, DT_DIR);
for (vcons = first_vcons; vcons; vcons = vcons->next)
if (!add_dir_entry (vcons->name,
vcons->id << 2, DT_DIR))
break;
}
else
{
if (first_entry == 0)
add_dir_entry (".", dir->nn_stat.st_ino, DT_DIR);
if (first_entry <= 1)
add_dir_entry ("..", 2, DT_DIR);
if (first_entry <= 2)
add_dir_entry ("console", (dir->nn->vcons->id << 2) + 1, DT_REG);
if (first_entry <= 3)
add_dir_entry ("display", (dir->nn->vcons->id << 2) + 2, DT_REG);
if (first_entry <= 4)
add_dir_entry ("input", (dir->nn->vcons->id << 3) + 2, DT_FIFO);
}
}
if (dir->nn->cons)
pthread_mutex_unlock(&dir->nn->cons->lock);
fshelp_touch (&dir->nn_stat, TOUCH_ATIME, console_maptime);
return err;
}
error_t
netfs_attempt_syncfs (struct iouser *cred, int wait)
{
return 0;
}
error_t
netfs_attempt_chown (struct iouser *cred, struct node *node,
uid_t uid, uid_t gid)
{
cons_t cons = node->nn->cons;
vcons_t vcons;
error_t err;
if (!cons)
return EOPNOTSUPP;
err = file_chown (cons->underlying, uid, gid);
if (err)
return err;
node->nn_stat.st_uid = uid;
node->nn_stat.st_gid = gid;
pthread_mutex_lock (&cons->lock);
cons->stat_template.st_uid = uid;
cons->stat_template.st_gid = gid;
for (vcons = cons->vcons_list; vcons; vcons = vcons->next)
{
if (vcons->dir_node)
{
vcons->dir_node->nn_stat.st_uid = uid;
vcons->dir_node->nn_stat.st_gid = gid;
}
if (vcons->cons_node)
{
vcons->cons_node->nn_stat.st_uid = uid;
vcons->cons_node->nn_stat.st_gid = gid;
}
if (vcons->disp_node)
{
vcons->disp_node->nn_stat.st_uid = uid;
vcons->disp_node->nn_stat.st_gid = gid;
}
if (vcons->inpt_node)
{
vcons->inpt_node->nn_stat.st_uid = uid;
vcons->inpt_node->nn_stat.st_gid = gid;
}
}
pthread_mutex_unlock (&cons->lock);
fshelp_touch (&node->nn_stat, TOUCH_CTIME, console_maptime);
return err;
}
error_t
netfs_attempt_chauthor (struct iouser *cred, struct node *node, uid_t author)
{
cons_t cons = node->nn->cons;
vcons_t vcons;
error_t err;
if (!cons)
return EOPNOTSUPP;
err = file_chauthor (cons->underlying, author);
if (err)
return err;
node->nn_stat.st_author = author;
pthread_mutex_lock (&cons->lock);
cons->stat_template.st_author = author;
for (vcons = cons->vcons_list; vcons; vcons = vcons->next)
{
if (vcons->dir_node)
vcons->dir_node->nn_stat.st_author = author;
if (vcons->cons_node)
vcons->cons_node->nn_stat.st_author = author;
if (vcons->disp_node)
vcons->disp_node->nn_stat.st_author = author;
if (vcons->inpt_node)
vcons->inpt_node->nn_stat.st_author = author;
}
pthread_mutex_unlock (&cons->lock);
fshelp_touch (&node->nn_stat, TOUCH_CTIME, console_maptime);
return err;
}
error_t
netfs_attempt_chmod (struct iouser *cred, struct node *node, mode_t mode)
{
error_t err;
mode &= ~S_ITRANS;
if ((mode & S_IFMT) == 0)
mode |= (node->nn_stat.st_mode & S_IFMT);
if (!node->nn->cons || ((mode & S_IFMT) != (node->nn_stat.st_mode & S_IFMT)))
return EOPNOTSUPP;
err = file_chmod (node->nn->cons->underlying, mode & ~S_IFMT);
if (err)
return err;
node->nn_stat.st_mode = mode;
fshelp_touch (&node->nn_stat, TOUCH_CTIME, console_maptime);
return err;
}
error_t
netfs_attempt_mksymlink (struct iouser *cred, struct node *np, const char *name)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_mkdev (struct iouser *cred, struct node *np,
mode_t type, dev_t indexes)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_chflags (struct iouser *cred, struct node *np, int flags)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_set_size (struct iouser *cred, struct node *np, off_t size)
{
vcons_t vcons = np->nn->vcons;
if (!vcons || np == vcons->dir_node
|| np == vcons->disp_node)
return EOPNOTSUPP;
assert_backtrace (np == vcons->cons_node || np == vcons->inpt_node);
return 0;
}
error_t
netfs_attempt_statfs (struct iouser *cred, struct node *np, struct statfs *st)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_mkdir (struct iouser *user, struct node *dir,
const char *name, mode_t mode)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_unlink (struct iouser *user, struct node *dir, const char *name)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_rename (struct iouser *user, struct node *fromdir,
const char *fromname, struct node *todir,
const char *toname, int excl)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_rmdir (struct iouser *user,
struct node *dir, const char *name)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_link (struct iouser *user, struct node *dir,
struct node *file, const char *name, int excl)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_mkfile (struct iouser *user, struct node *dir,
mode_t mode, struct node **np)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_readlink (struct iouser *user, struct node *np, char *buf)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_read (struct iouser *cred, struct node *np,
off_t offset, size_t *len, void *data)
{
error_t err = 0;
vcons_t vcons = np->nn->vcons;
if (!vcons || np == vcons->dir_node
|| np == vcons->inpt_node)
return EOPNOTSUPP;
pthread_mutex_unlock (&np->lock);
if (np == vcons->cons_node)
{
ssize_t amt = input_dequeue (vcons->input,
0,
data, *len);
if (amt == -1)
err = errno;
else
*len = amt;
}
else
{
ssize_t amt = *len;
assert_backtrace (np == vcons->disp_node);
if (offset + amt > np->nn_stat.st_size)
amt = np->nn_stat.st_size - offset;
if (amt < 0)
amt = 0;
else
amt = display_read (vcons->display,
0,
offset, data, amt);
if (amt == -1)
err = errno;
else
*len = amt;
}
pthread_mutex_lock (&np->lock);
return err;
}
error_t
netfs_attempt_write (struct iouser *cred, struct node *np,
off_t offset, size_t *len, const void *data)
{
error_t err = 0;
vcons_t vcons = np->nn->vcons;
if (!vcons || np == vcons->dir_node
|| np == vcons->disp_node)
return EOPNOTSUPP;
pthread_mutex_unlock (&np->lock);
if (np == vcons->cons_node)
{
int amt = display_output (vcons->display,
0,
data, *len);
if (amt == -1)
err = errno;
else
*len = amt;
}
else
{
int amt;
assert_backtrace (np == vcons->inpt_node);
amt = input_enqueue (vcons->input,
1,
data, *len);
if (amt == -1)
err = errno;
else
*len = amt;
}
pthread_mutex_lock (&np->lock);
return err;
}
kern_return_t
netfs_S_io_map (struct protid *cred,
memory_object_t *rdobj,
mach_msg_type_name_t *rdtype,
memory_object_t *wrobj,
mach_msg_type_name_t *wrtype)
{
int flags;
struct node *np;
vcons_t vcons;
if (!cred)
return EOPNOTSUPP;
np = cred->po->np;
vcons = np->nn->vcons;
if (!vcons || np != vcons->disp_node)
return EOPNOTSUPP;
*wrobj = *rdobj = MACH_PORT_NULL;
flags = cred->po->openstat & (O_READ | O_WRITE);
pthread_mutex_lock (&np->lock);
switch (flags)
{
case O_READ | O_WRITE:
*wrobj = *rdobj = display_get_filemap (vcons->display,
VM_PROT_READ | VM_PROT_WRITE);
if (*wrobj == MACH_PORT_NULL)
goto error;
mach_port_mod_refs (mach_task_self (), *rdobj, MACH_PORT_RIGHT_SEND, 1);
break;
case O_READ:
*rdobj = display_get_filemap (vcons->display, VM_PROT_READ);
if (*rdobj == MACH_PORT_NULL)
goto error;
break;
case O_WRITE:
*wrobj = display_get_filemap (vcons->display, VM_PROT_WRITE);
if (*wrobj == MACH_PORT_NULL)
goto error;
break;
}
pthread_mutex_unlock (&np->lock);
*rdtype = MACH_MSG_TYPE_MOVE_SEND;
*wrtype = MACH_MSG_TYPE_MOVE_SEND;
return 0;
error:
pthread_mutex_unlock (&np->lock);
return errno;
}
kern_return_t
netfs_S_dir_notice_changes (struct protid *cred, mach_port_t notify)
{
error_t err;
cons_t cons;
struct modreq **preq;
struct modreq *req;
if (!cred)
return EOPNOTSUPP;
cons = cred->po->np->nn->cons;
if (!cons)
return EOPNOTSUPP;
pthread_mutex_lock (&cons->lock);
preq = &cons->dirmod_reqs;
while (*preq)
{
mach_port_type_t type;
req = *preq;
err = mach_port_type (mach_task_self (), req->port, &type);
if (!err && type == MACH_PORT_TYPE_DEAD_NAME)
{
*preq = req->next;
mach_port_deallocate (mach_task_self (), req->port);
free (req);
}
else
preq = &req->next;
}
err = dir_changed (notify, cons->dirmod_tick, DIR_CHANGED_NULL, "");
if (err)
{
pthread_mutex_unlock (&cons->lock);
return err;
}
req = malloc (sizeof (struct modreq));
if (!req)
{
pthread_mutex_unlock (&cons->lock);
return errno;
}
req->port = notify;
req->next = cons->dirmod_reqs;
cons->dirmod_reqs = req;
pthread_mutex_unlock (&cons->lock);
return 0;
}
kern_return_t
netfs_S_file_notice_changes (struct protid *cred, mach_port_t notify)
{
struct node *np;
vcons_t vcons;
if (!cred)
return EOPNOTSUPP;
np = cred->po->np;
vcons = np->nn->vcons;
if (!vcons || np != vcons->disp_node)
return EOPNOTSUPP;
return display_notice_changes (vcons->display, notify);
}
static const char *color_names[CONS_COLOR_MAX + 1] =
{
[CONS_COLOR_BLACK] = "black",
[CONS_COLOR_RED] = "red",
[CONS_COLOR_GREEN] = "green",
[CONS_COLOR_YELLOW] = "yellow",
[CONS_COLOR_BLUE] = "blue",
[CONS_COLOR_MAGENTA] = "magenta",
[CONS_COLOR_CYAN] = "cyan",
[CONS_COLOR_WHITE] = "white"
};
static const struct argp_option options[] =
{
{ "foreground", 'f', "COLOR", 0, "Set foreground color to"
" COLOR (default `" DEFAULT_FOREGROUND_NAME "')" },
{ "background", 'b', "COLOR", 0, "Set background color to"
" COLOR (default `" DEFAULT_BACKGROUND_NAME "')" },
{ "attribute", 'a', "ATTR[,...]", 0, "Set further default attributes"
" (default `" DEFAULT_ATTRIBUTE_NAME "')" },
{ "encoding",	'e', "NAME", 0, "Set encoding of virtual consoles to"
" NAME (default `" DEFAULT_ENCODING "')" },
{ "width", 'w', "WIDTH", 0, "Set width to WIDTH (default `"
STR(DEFAULT_WIDTH) "')" },
{ "height", 'h', "HEIGHT", 0, "Set height to HEIGHT (default `"
STR(DEFAULT_HEIGHT) "')" },
{ "lines", 'l', "LINES", 0, "Set amount of scrollback lines to LINES "
"(default `" STR(DEFAULT_LINES) "')" },
{0}
};
static error_t
parse_color (const char *name, int *number)
{
if (isdigit (*name))
{
long int nr;
char *tail;
errno = 0;
nr = strtol (name, &tail, 0);
if (errno || *tail || nr < 0 || nr > CONS_COLOR_MAX)
return EINVAL;
*number = nr;
return 0;
}
else
{
int i;
for (i = 0; i <= CONS_COLOR_MAX; i++)
if (!strcmp (color_names[i], name))
{
*number = i;
return 0;
}
return EINVAL;
}
}
static error_t
parse_attributes (const char *name, conchar_attr_t *attr)
{
while (*name)
{
int value = 1;
if (!strncmp (name, "not-", 4))
{
value = 0;
name += 4;
}
if (!strncmp (name, "normal", 6))
{
name += 6;
if (value != 1)
return EINVAL;
attr->intensity = CONS_ATTR_INTENSITY_NORMAL;
}
else if (!strncmp (name, "bright", 6))
{
name += 6;
if (value != 1)
return EINVAL;
attr->intensity = CONS_ATTR_INTENSITY_BOLD;
}
else if (!strncmp (name, "dim", 3))
{
name += 3;
if (value != 1)
return EINVAL;
attr->intensity = CONS_ATTR_INTENSITY_DIM;
}
else if (!strncmp (name, "underlined", 10))
{
name += 10;
attr->underlined = value;
}
else if (!strncmp (name, "blinking", 8))
{
name += 8;
attr->blinking = value;
}
else if (!strncmp (name, "concealed", 9))
{
name += 9;
attr->concealed = value;
}
else if (!strncmp (name, "italic", 6))
{
name += 6;
attr->italic = value;
}
else if (!strncmp (name, "bold", 4))
{
name += 4;
attr->bold = value;
}
else
return EINVAL;
if (name[0] == ',')
name++;
else if (name[0] != '\0')
return EINVAL;
}
return 0;
}
static error_t
parse_opt (int opt, char *arg, struct argp_state *state)
{
cons_t cons = state->input ?: netfs_root_node->nn->cons;
error_t err;
int color = 0;
char *tail;
switch (opt)
{
default:
return ARGP_ERR_UNKNOWN;
case ARGP_KEY_SUCCESS:
case ARGP_KEY_ERROR:
break;
case ARGP_KEY_INIT:
pthread_mutex_lock (&cons->lock);
break;
case ARGP_KEY_FINI:
pthread_mutex_unlock (&cons->lock);
break;
case 'f':
err = parse_color (arg, &color);
cons->attribute.fgcol = color;
if (err)
argp_error (state, "Invalid color name: %s", arg);
break;
case 'b':
err = parse_color (arg, &color);
cons->attribute.bgcol = color;
if (err)
argp_error (state, "Invalid color name: %s", arg);
break;
case 'a':
err = parse_attributes (arg, &cons->attribute);
if (err)
argp_error (state, "Invalid attribute specifier: %s", arg);
break;
case 'l':
errno = 0;
cons->lines = strtoul (arg, &tail, 0);
if (tail == NULL || tail == arg || *tail != '\0')
argp_error (state, "LINES is not a number: %s", arg);
if (errno)
argp_error (state, "Overflow in argument LINES %s", arg);
break;
case 'w':
errno = 0;
cons->width = strtoul (arg, &tail, 0);
if (tail == NULL || tail == arg || *tail != '\0')
argp_error (state, "WIDTH is not a number: %s", arg);
if (errno)
argp_error (state, "Overflow in argument WIDTH %s", arg);
break;
case 'h':
errno = 0;
cons->height = strtoul (arg, &tail, 0);
if (tail == NULL || tail == arg || *tail != '\0')
argp_error (state, "HEIGHT is not a number: %s", arg);
if (errno)
argp_error (state, "Overflow in argument HEIGHT %s", arg);
break;
case 'e':
{
char *new = strdup (arg);
if (!new)
return ENOMEM;
if (cons->encoding)
free (cons->encoding);
cons->encoding = new;
}
break;
}
return 0;
}
error_t
netfs_append_args (char **argz, size_t *argz_len)
{
error_t err = 0;
cons_t cons = netfs_root_node->nn->cons;
char attr_str[80] = "--attribute=";
char *attr = &attr_str[12];
char *attrp = attr;
if (cons->encoding && strcmp (cons->encoding, DEFAULT_ENCODING))
{
char *buf;
if (asprintf (&buf, "--encoding=%s", cons->encoding) < 0)
err = ENOMEM;
else
err = argz_add (argz, argz_len, buf);
}
if (!err && cons->attribute.fgcol != DEFAULT_FOREGROUND)
{
char *buf;
if (asprintf (&buf, "--foreground=%s",
color_names[cons->attribute.fgcol]) < 0)
err = ENOMEM;
else
err = argz_add (argz, argz_len, buf);
}
if (!err && cons->attribute.bgcol != DEFAULT_BACKGROUND)
{
char *buf;
if (asprintf (&buf, "--background=%s",
color_names[cons->attribute.bgcol]) < 0)
err = ENOMEM;
else
err = argz_add (argz, argz_len, buf);
}
if (!err && cons->lines != DEFAULT_LINES)
{
char *buf;
if (asprintf (&buf, "--lines=%d", cons->lines) < 0)
err = ENOMEM;
else
err = argz_add (argz, argz_len, buf);
}
if (!err && cons->width != DEFAULT_WIDTH)
{
char *buf;
if (asprintf (&buf, "--width=%d", cons->lines) < 0)
err = ENOMEM;
else
err = argz_add (argz, argz_len, buf);
}
if (!err && cons->height != DEFAULT_HEIGHT)
{
char *buf;
if (asprintf (&buf, "--height=%d", cons->height) < 0)
err = ENOMEM;
else
err = argz_add (argz, argz_len, buf);
}
if (!err && cons->attribute.intensity != DEFAULT_INTENSITY)
{
if (attrp != attr)
*(attrp++) = ',';
switch (cons->attribute.intensity)
{
case CONS_ATTR_INTENSITY_NORMAL:
attrp = stpcpy (attrp, "normal");
break;
case CONS_ATTR_INTENSITY_BOLD:
attrp = stpcpy (attrp, "bold");
break;
case CONS_ATTR_INTENSITY_DIM:
attrp = stpcpy (attrp, "dim");
break;
}
}
if (!err && cons->attribute.underlined != DEFAULT_UNDERLINED)
{
if (attrp != attr)
*(attrp++) = ',';
if (!cons->attribute.underlined)
attrp = stpcpy (attrp, "not-");
attrp = stpcpy (attrp, "underlined");
}
if (!err && cons->attribute.blinking != DEFAULT_BLINKING)
{
if (attrp != attr)
*(attrp++) = ',';
if (!cons->attribute.blinking)
attrp = stpcpy (attrp, "not-");
attrp = stpcpy (attrp, "blinking");
}
if (!err && cons->attribute.reversed != DEFAULT_REVERSED)
{
if (attrp != attr)
*(attrp++) = ',';
if (!cons->attribute.reversed)
attrp = stpcpy (attrp, "not-");
attrp = stpcpy (attrp, "reversed");
}
if (!err && cons->attribute.concealed != DEFAULT_CONCEALED)
{
if (attrp != attr)
*(attrp++) = ',';
if (!cons->attribute.concealed)
attrp = stpcpy (attrp, "not-");
attrp = stpcpy (attrp, "concealed");
}
if (!err && attrp != attr)
err = argz_add (argz, argz_len, attr_str);
return err;
}
kern_return_t
S_tioctl_tiocflush (struct protid *cred, int queue_selector)
{
struct node *np;
vcons_t vcons;
if (!cred)
return EOPNOTSUPP;
if (!(cred->po->openstat & (O_READ | O_WRITE)))
return EBADF;
np = cred->po->np;
vcons = np->nn->vcons;
if (!vcons || np != vcons->cons_node)
return EOPNOTSUPP;
if (!queue_selector)
queue_selector = O_READ | O_WRITE;
if (queue_selector & O_READ)
input_flush (vcons->input);
if (queue_selector & O_WRITE)
display_discard_output (vcons->display);
return 0;
}
kern_return_t
S_tioctl_tiocgwinsz (struct protid *cred, struct winsize *size)
{
struct node *np;
vcons_t vcons;
if (!cred)
return EOPNOTSUPP;
np = cred->po->np;
vcons = np->nn->vcons;
if (!vcons || np != vcons->cons_node)
return EOPNOTSUPP;
display_getsize (vcons->display, size);
return 0;
}
kern_return_t
S_tioctl_tiocstart (struct protid *cred)
{
struct node *np;
vcons_t vcons;
if (!cred)
return EOPNOTSUPP;
if (!(cred->po->openstat & (O_READ | O_WRITE)))
return EBADF;
np = cred->po->np;
vcons = np->nn->vcons;
if (!vcons || np != vcons->cons_node)
return EOPNOTSUPP;
display_start_output (vcons->display);
return 0;
}
kern_return_t
S_tioctl_tiocstop (struct protid *cred)
{
struct node *np;
vcons_t vcons;
if (!cred)
return EOPNOTSUPP;
if (!(cred->po->openstat & (O_READ | O_WRITE)))
return EBADF;
np = cred->po->np;
vcons = np->nn->vcons;
if (!vcons || np != vcons->cons_node)
return EOPNOTSUPP;
display_stop_output (vcons->display);
return 0;
}
kern_return_t
S_tioctl_tiocoutq (struct protid *cred, int *queue_size)
{
struct node *np;
vcons_t vcons;
if (!cred)
return EOPNOTSUPP;
if (!(cred->po->openstat & (O_READ | O_WRITE)))
return EBADF;
np = cred->po->np;
vcons = np->nn->vcons;
if (!vcons || np != vcons->cons_node)
return EOPNOTSUPP;
*queue_size = display_pending_output (vcons->display);
return 0;
}
kern_return_t
S_tioctl_tiocspgrp (struct protid *cred, int pgrp)
{
struct node *np;
vcons_t vcons;
if (!cred)
return EOPNOTSUPP;
if (!(cred->po->openstat & (O_READ | O_WRITE)))
return EBADF;
np = cred->po->np;
vcons = np->nn->vcons;
if (!vcons || np != vcons->cons_node)
return EOPNOTSUPP;
display_set_owner (vcons->display, -pgrp);
return 0;
}
kern_return_t
S_tioctl_tiocgpgrp (struct protid *cred, int *pgrp)
{
error_t err;
struct node *np;
vcons_t vcons;
if (!cred)
return EOPNOTSUPP;
if (!(cred->po->openstat & (O_READ | O_WRITE)))
return EBADF;
np = cred->po->np;
vcons = np->nn->vcons;
if (!vcons || np != vcons->cons_node)
return EOPNOTSUPP;
err = display_get_owner (vcons->display, pgrp);
if (!err)
*pgrp = -*pgrp;
return err;
}
kern_return_t
S_tioctl_tiocmodg (struct protid *cred, int *state)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocmods (struct protid *cred, int state)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocexcl (struct protid *cred)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocnxcl (struct protid *cred)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocgeta (struct protid *cred, modes_t modes, ccs_t ccs,
speeds_t speeds)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocseta (struct protid *cred, const modes_t modes, const ccs_t ccs,
const speeds_t speeds)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocsetaw (struct protid *cred, const modes_t modes, const ccs_t ccs,
const speeds_t speeds)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocsetaf (struct protid *cred, const modes_t modes, const ccs_t ccs,
const speeds_t speeds)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocgetd (struct protid *cred, int *disc)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocsetd (struct protid *cred, int disc)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocdrain (struct protid *cred)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocmget (struct protid *cred, int *bits)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocmset (struct protid *cred, int bits)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocsig (struct protid *cred, int sig)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocext (struct protid *cred, int mode)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocucntl (struct protid *cred, int mode)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocswinsz (struct protid *cred, struct winsize size)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocremote (struct protid *cred, int how)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocmbic (struct protid *cred, int bits)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocmbis (struct protid *cred, int bits)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocpkt (struct protid *cred, int mode)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocsti (struct protid *cred, char c)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tioccdtr (struct protid *cred)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocsdtr (struct protid *cred)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tioccbrk (struct protid *cred)
{
return EOPNOTSUPP;
}
kern_return_t
S_tioctl_tiocsbrk (struct protid *cred)
{
return EOPNOTSUPP;
}
int
console_demuxer (mach_msg_header_t *inp,
mach_msg_header_t *outp)
{
extern int netfs_demuxer (mach_msg_header_t *inp, mach_msg_header_t *outp);
extern int tioctl_server (mach_msg_header_t *inp, mach_msg_header_t *outp);
return (netfs_demuxer (inp, outp)
|| tioctl_server (inp, outp));
}
const struct argp netfs_std_runtime_argp =
{ options, parse_opt, NULL,
"A translator that provides virtual consoles." };
int
main (int argc, char **argv)
{
error_t err;
mach_port_t bootstrap;
struct stat ul_stat;
cons_t cons;
struct netnode root_nn = { vcons: 0 };
cons = malloc (sizeof (struct cons));
if (!cons)
error (1, ENOMEM, "Cannot create console structure");
pthread_mutex_init (&cons->lock, NULL);
cons->encoding = NULL;
cons->width = DEFAULT_WIDTH;
cons->height = DEFAULT_HEIGHT;
cons->lines = DEFAULT_LINES;
cons->attribute.intensity = DEFAULT_INTENSITY;
cons->attribute.underlined = DEFAULT_UNDERLINED;
cons->attribute.blinking = DEFAULT_BLINKING;
cons->attribute.reversed = DEFAULT_REVERSED;
cons->attribute.concealed = DEFAULT_CONCEALED;
cons->attribute.fgcol = DEFAULT_FOREGROUND;
cons->attribute.bgcol = DEFAULT_BACKGROUND;
cons->vcons_list = NULL;
cons->dirmod_reqs = NULL;
cons->dirmod_tick = 0;
root_nn.cons = cons;
argp_parse (&netfs_std_runtime_argp, argc, argv, 0, 0, cons);
setlocale (LC_CTYPE, "C.UTF-8");
task_get_bootstrap_port (mach_task_self (), &bootstrap);
netfs_init ();
display_init ();
netfs_root_node = netfs_make_node (&root_nn);
if (! netfs_root_node)
error (2, ENOMEM, "Cannot create root node");
err = maptime_map (0, 0, &console_maptime);
if (err)
error (3, err, "Cannot map time");
cons->node = netfs_root_node;
cons->underlying = netfs_startup (bootstrap, O_READ);
if (cons->underlying == MACH_PORT_NULL)
error (4, 0, "Cannot get underlying node");
err = io_stat (cons->underlying, &ul_stat);
if (err)
error (5, err, "Cannot stat underlying node");
cons->stat_template.st_uid = ul_stat.st_uid;
cons->stat_template.st_gid = ul_stat.st_gid;
cons->stat_template.st_author = ul_stat.st_author;
cons->stat_template.st_mode = (ul_stat.st_mode & ~S_IFMT & ~S_ITRANS);
cons->stat_template.st_fsid = getpid ();
cons->stat_template.st_nlink = 1;
cons->stat_template.st_fstype = FSTYPE_MISC;
netfs_root_node->nn_stat = cons->stat_template;
netfs_root_node->nn_stat.st_ino = 2;
netfs_root_node->nn_stat.st_mode |= S_IFDIR;
netfs_root_node->nn_translated = 0;
if (! S_ISDIR (ul_stat.st_mode))
{
if (ul_stat.st_mode & S_IRUSR)
netfs_root_node->nn_stat.st_mode |= S_IXUSR;
if (ul_stat.st_mode & S_IRGRP)
netfs_root_node->nn_stat.st_mode |= S_IXGRP;
if (ul_stat.st_mode & S_IROTH)
netfs_root_node->nn_stat.st_mode |= S_IXOTH;
}
fshelp_touch (&netfs_root_node->nn_stat, TOUCH_ATIME|TOUCH_MTIME|TOUCH_CTIME,
console_maptime);
do
{
ports_manage_port_operations_multithread (netfs_port_bucket,
console_demuxer,
1000 * 60 * 2,
1000 * 60 * 10,
0);
err = netfs_shutdown (0);
}
while (err);
exit (err);
}