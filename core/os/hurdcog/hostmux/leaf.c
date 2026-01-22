#include <string.h>
#include <argz.h>
#include "hostmux.h"
error_t
netfs_attempt_readlink (struct iouser *user, struct node *node, char *buf)
{
assert_backtrace (node->nn->name);
memcpy (buf, node->nn->name->canon, node->nn_stat.st_size);
fshelp_touch (&node->nn_stat, TOUCH_ATIME, hostmux_maptime);
return 0;
}
error_t
netfs_get_translator (struct node *node, char **args, mach_msg_type_number_t *args_len)
{
if (! node->nn->name)
return EINVAL;
else
{
error_t err = 0;
unsigned replace_count = 0;
struct hostmux *mux = node->nn->mux;
char *argz = 0;
size_t argz_len = 0;
err = argz_append (&argz, &argz_len,
mux->trans_template, mux->trans_template_len);
if (! err)
err = argz_replace (&argz, &argz_len,
mux->host_pat, node->nn->name->canon,
&replace_count);
if (!err && replace_count == 0)
err = argz_add (&argz, &argz_len, node->nn->name->canon);
if (err && argz_len > 0)
free (argz);
*args = argz;
*args_len = argz_len;
return err;
}
}
error_t
create_host_node (struct hostmux *mux, struct hostmux_name *name,
struct node **node)
{
struct node *new;
struct netnode *nn = malloc (sizeof (struct netnode));
if (! nn)
return ENOMEM;
nn->mux = mux;
nn->name = name;
new = netfs_make_node (nn);
if (! new)
{
free (nn);
return ENOMEM;
}
new->nn_stat = mux->stat_template;
new->nn_stat.st_ino = name->fileno;
if (strcmp (name->name, name->canon) == 0)
{
new->nn_stat.st_mode = (S_IFREG | S_IPTRANS | 0666);
new->nn_stat.st_size = 0;
}
else
{
new->nn_stat.st_mode = (S_IFLNK | 0666);
new->nn_stat.st_size = strlen (name->canon);
}
new->nn_translated = new->nn_stat.st_mode;
fshelp_touch (&new->nn_stat, TOUCH_ATIME|TOUCH_MTIME|TOUCH_CTIME,
hostmux_maptime);
name->node = new;
*node = new;
return 0;
}