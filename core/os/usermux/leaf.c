#include <stdio.h>
#include <string.h>
#include <pwd.h>
#include <argz.h>
#include <hurd/paths.h>
#include "usermux.h"
error_t
netfs_attempt_readlink (struct iouser *user, struct node *node, char *buf)
{
assert_backtrace (node->nn->name);
memcpy (buf, node->nn->trans, node->nn->trans_len);
fshelp_touch (&node->nn_stat, TOUCH_ATIME, usermux_maptime);
return 0;
}
error_t
netfs_get_translator (struct node *node, char **trans, mach_msg_type_number_t *trans_len)
{
if (! node->nn->name)
return EINVAL;
else
{
char *argz = 0;
size_t argz_len = 0;
fshelp_touch (&node->nn_stat, TOUCH_ATIME, usermux_maptime);
if (S_ISLNK (node->nn_stat.st_mode))
argz_add (&argz, &argz_len, _HURD_SYMLINK);
return
argz_append (&argz, &argz_len, node->nn->trans, node->nn->trans_len);
*trans = argz;
*trans_len = argz_len;
}
}
error_t
create_user_node (struct usermux *mux, struct usermux_name *name,
struct passwd *pw, struct node **node)
{
error_t err;
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
new->nn_stat.st_ino = pw->pw_uid + USERMUX_FILENO_UID_OFFSET;
if (strcmp (mux->trans_template, _HURD_SYMLINK) == 0
&& mux->trans_template_len == sizeof _HURD_SYMLINK)
{
err = argz_create_sep (pw->pw_dir, 0, &nn->trans, &nn->trans_len);
new->nn_stat.st_mode = (S_IFLNK | 0666);
new->nn_stat.st_size = nn->trans_len;
}
else
{
unsigned replace_count = 0;
nn->trans = 0;
nn->trans_len = 0;
err = argz_append (&nn->trans, &nn->trans_len,
mux->trans_template, mux->trans_template_len);
if (!err && mux->user_pat && *mux->user_pat)
err = argz_replace (&nn->trans, &nn->trans_len,
mux->user_pat, pw->pw_name,
&replace_count);
if (!err && mux->home_pat && *mux->home_pat)
err = argz_replace (&nn->trans, &nn->trans_len,
mux->home_pat, pw->pw_dir,
&replace_count);
if (!err && mux->uid_pat && *mux->uid_pat)
{
char uid_buf[10];
snprintf (uid_buf, sizeof uid_buf, "%d", pw->pw_uid);
err = argz_replace (&nn->trans, &nn->trans_len,
mux->uid_pat, uid_buf,
&replace_count);
}
if (!err && replace_count == 0)
err = argz_add (&nn->trans, &nn->trans_len, pw->pw_dir);
if (err && nn->trans_len > 0)
free (nn->trans);
new->nn_stat.st_mode = (S_IFREG | S_IPTRANS | 0666);
new->nn_stat.st_size = 0;
}
new->nn_translated = new->nn_stat.st_mode;
if (err)
{
free (nn);
free (new);
return err;
}
fshelp_touch (&new->nn_stat, TOUCH_ATIME|TOUCH_MTIME|TOUCH_CTIME,
usermux_maptime);
name->node = new;
*node = new;
return 0;
}