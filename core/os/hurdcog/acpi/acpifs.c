#include <acpifs.h>
#include <error.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <pthread.h>
#include <hurd/netfs.h>
#include <ncache.h>
#include <func_files.h>
static error_t
create_dir_entry (char *name, struct acpi_table *t,
struct acpifs_dirent *parent, io_statbuf_t stat,
struct node *node, struct acpifs_dirent *entry)
{
uint16_t parent_num_entries;
strncpy (entry->name, name, NAME_SIZE-1);
entry->name[NAME_SIZE-1] = '\0';
entry->acpitable = t;
entry->parent = parent;
entry->stat = stat;
entry->dir = 0;
entry->node = node;
if (entry->parent)
{
if (!entry->parent->dir)
{
entry->parent->dir = calloc (1, sizeof (struct acpifs_dir));
if (!entry->parent->dir)
return ENOMEM;
}
parent_num_entries = entry->parent->dir->num_entries++;
entry->parent->dir->entries = realloc (entry->parent->dir->entries,
entry->parent->dir->num_entries *
sizeof (struct acpifs_dirent *));
if (!entry->parent->dir->entries)
return ENOMEM;
entry->parent->dir->entries[parent_num_entries] = entry;
}
return 0;
}
error_t
alloc_file_system (struct acpifs **fs)
{
*fs = calloc (1, sizeof (struct acpifs));
if (!*fs)
return ENOMEM;
return 0;
}
error_t
init_root_node (file_t underlying_node)
{
error_t err;
struct node *np;
io_statbuf_t underlying_node_stat = { 0 };
if (underlying_node != MACH_PORT_NULL)
{
err = io_stat (underlying_node, &underlying_node_stat);
if (err)
return err;
}
np = netfs_make_node_alloc (sizeof (struct netnode));
if (!np)
return ENOMEM;
np->nn_stat = underlying_node_stat;
np->nn_stat.st_fsid = getpid ();
np->nn_stat.st_mode =
S_IFDIR | S_IROOT | S_IRUSR | S_IXUSR | S_IRGRP | S_IXGRP | S_IROTH |
S_IXOTH;
np->nn_translated = np->nn_stat.st_mode;
fshelp_touch (&np->nn_stat, TOUCH_ATIME | TOUCH_MTIME | TOUCH_CTIME,
acpifs_maptime);
netfs_root_node = np;
return 0;
}
error_t
init_file_system (struct acpifs *fs)
{
error_t err;
struct node *np = netfs_root_node;
fs->entries = calloc (1, sizeof (struct acpifs_dirent));
if (!fs->entries)
return ENOMEM;
err = create_dir_entry ("", 0, 0, np->nn_stat, np, fs->entries);
if (err)
{
free(fs->entries);
return err;
}
fs->num_entries = 1;
fs->root = np;
fs->root->nn->ln = fs->entries;
pthread_mutex_init (&fs->node_cache_lock, 0);
return 0;
}
error_t
create_fs_tree (struct acpifs *fs)
{
error_t err = 0;
int i;
size_t nentries, ntables = 0;
struct acpifs_dirent *e, *list, *parent;
struct stat e_stat;
char entry_name[NAME_SIZE];
struct acpi_table *iter = NULL;
e_stat = fs->entries->stat;
err = acpi_get_num_tables(&ntables);
if (err)
return err;
nentries = ntables + 2;
list = realloc (fs->entries, nentries * sizeof (struct acpifs_dirent));
if (!list) {
if (fs->entries)
free(fs->entries);
return ENOMEM;
}
e = list + 1;
parent = list;
e_stat.st_mode &= ~S_IROOT;
memset (entry_name, 0, NAME_SIZE);
strncpy (entry_name, DIR_TABLES_NAME, NAME_SIZE);
err = create_dir_entry (entry_name, 0, parent, e_stat, 0, e);
if (err)
return err;
parent = e;
e_stat.st_mode &= ~(S_IROTH | S_IWOTH | S_IXOTH);
e_stat.st_mode &= ~(S_IFDIR | S_IXUSR | S_IXGRP | S_IWUSR | S_IWGRP);
e_stat.st_mode |= S_IFREG;
err = acpi_get_tables(&iter);
if (err)
return err;
for (i = 0; i < ntables; i++, iter++)
{
e_stat.st_size = iter->datalen;
memset (entry_name, 0, NAME_SIZE);
snprintf (entry_name, NAME_SIZE, "%c%c%c%c",
iter->h.signature[0],
iter->h.signature[1],
iter->h.signature[2],
iter->h.signature[3]);
e++;
err = create_dir_entry (entry_name, iter, parent, e_stat, 0, e);
if (err)
return err;
}
fs->entries = list;
fs->num_entries = nentries;
fs->root->nn->ln = fs->entries;
return err;
}
error_t
entry_check_perms (struct iouser *user, struct acpifs_dirent *e, int flags)
{
error_t err = 0;
if (!err && (flags & O_READ))
err = fshelp_access (&e->stat, S_IREAD, user);
if (!err && (flags & O_WRITE))
err = fshelp_access (&e->stat, S_IWRITE, user);
if (!err && (flags & O_EXEC))
err = fshelp_access (&e->stat, S_IEXEC, user);
return err;
}
static void
entry_default_perms (struct acpifs *fs, struct acpifs_dirent *e)
{
UPDATE_OWNER (e, fs->root->nn->ln->stat.st_uid);
UPDATE_GROUP (e, fs->root->nn->ln->stat.st_gid);
UPDATE_TIMES (e, TOUCH_CTIME);
return;
}
static void
entry_set_perms (struct acpifs *fs, struct acpifs_dirent *e)
{
struct acpifs_perm *perm = &fs->perm;
if (perm->uid >= 0)
UPDATE_OWNER (e, perm->uid);
if (perm->gid >= 0)
UPDATE_GROUP (e, perm->gid);
UPDATE_TIMES (e, TOUCH_CTIME);
return;
}
error_t
fs_set_permissions (struct acpifs *fs)
{
int i;
struct acpifs_dirent *e;
for (i = 0, e = fs->entries; i < fs->num_entries; i++, e++)
{
entry_default_perms (fs, e);
entry_set_perms (fs, e);
}
return 0;
}