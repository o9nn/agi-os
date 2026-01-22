#include "priv.h"
#include "fsys_S.h"
#include <argz.h>
kern_return_t
diskfs_S_fsys_get_children (struct diskfs_control *fsys,
mach_port_t reply,
mach_msg_type_name_t replytype,
char **names,
mach_msg_type_number_t *names_len,
mach_port_t **controls,
mach_msg_type_name_t *controlsPoly,
mach_msg_type_number_t *controlsCnt)
{
error_t err;
char *n = NULL, *orig_names = *names;
size_t n_len = 0;
mach_port_t *c;
size_t c_count;
if (! fsys)
return EOPNOTSUPP;
err = fshelp_get_active_translators (&n, &n_len, &c, &c_count);
if (err)
goto errout;
err = iohelp_return_malloced_buffer (n, n_len, names, names_len);
n = NULL;
if (err)
goto errout;
err = iohelp_return_malloced_buffer ((char *) c, c_count * sizeof *c,
(char **) controls, controlsCnt);
c = NULL;
if (err)
{
if (*names != orig_names)
munmap (*names, n_len);
goto errout;
}
*controlsPoly = MACH_MSG_TYPE_MOVE_SEND;
*controlsCnt = c_count;
errout:
free (n);
free (c);
return err;
}