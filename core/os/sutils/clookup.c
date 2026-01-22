#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <hurd.h>
#include <hurd/lookup.h>
#include <hurd/id.h>
#include <hurd/fsys.h>
file_t
file_name_lookup_carefully (const char *name, int flags, mode_t mode)
{
error_t err;
file_t node;
uid_t *uids;
gid_t *gids;
size_t num_uids, num_gids;
error_t lookup (file_t dir, const char *name, int flags, mode_t mode,
retry_type *retry, string_t retry_name,
mach_port_t *node)
{
error_t err;
const char *head, *tail;
char *slash = index (name, '/');
if (slash)
{
char *str = alloca (slash - name + 1);
*stpncpy (str, name, slash - name) = 0;
head = str;
tail = slash + 1;
}
else
{
head = name;
tail = 0;
}
err = dir_lookup (dir, head, flags | O_NOTRANS, mode,
retry, retry_name, node);
if (err)
return err;
if (*node != MACH_PORT_NULL
&& (!(flags & O_NOTRANS) || tail || *retry_name))
{
fsys_t fsys;
err = file_get_translator_cntl (*node, &fsys);
if (! err)
{
file_t unauth_dir;
err = io_restrict_auth (dir, &unauth_dir, 0, 0, 0, 0);
if (! err)
{
file_t old_node = *node;
err = fsys_getroot (fsys,
unauth_dir, MACH_MSG_TYPE_COPY_SEND,
uids, num_uids, gids, num_gids,
flags & ~O_NOTRANS, retry,
retry_name, node);
mach_port_deallocate (mach_task_self (), unauth_dir);
if (! err)
mach_port_deallocate (mach_task_self (), old_node);
}
mach_port_deallocate (mach_task_self (), fsys);
}
if (!err && tail)
{
size_t rtn_len = strlen (retry_name);
if (rtn_len + 1 + strlen (tail) + 1 > sizeof (string_t))
err = ENAMETOOLONG;
else
{
if (rtn_len > 0 && retry_name[rtn_len - 1] != '/')
retry_name[rtn_len++] = '/';
strcpy (retry_name + rtn_len, tail);
}
}
if (err)
mach_port_deallocate (mach_task_self (), *node);
}
return err;
}
num_uids = geteuids (0, 0);
if (num_uids < 0)
return errno;
uids = alloca (num_uids * sizeof (uid_t));
num_uids = geteuids (num_uids, uids);
if (num_uids < 0)
return errno;
num_gids = getgroups (0, 0);
if (num_gids < 0)
return errno;
gids = alloca (num_gids * sizeof (gid_t));
num_gids = getgroups (num_gids, gids);
if (num_gids < 0)
return errno;
err = hurd_file_name_lookup (&_hurd_ports_use, &getdport, lookup,
name, flags, mode & ~getumask (),
&node);
return err ? (__hurd_fail (err), MACH_PORT_NULL) : node;
}