#include <argp.h>
#include "priv.h"
static const struct argp_option
std_runtime_options[] =
{
{"update", 'u',  0, 0, "Flush any meta-data cached in core"},
{"remount", 0, 0, OPTION_HIDDEN | OPTION_ALIAS},
{0, 0}
};
struct parse_hook
{
int readonly, sync, sync_interval, remount, nosuid, noexec, noatime,
noinheritdirgroup, relatime;
};
static error_t
set_opts (struct parse_hook *h)
{
error_t err = 0;
if (h->remount)
{
err = diskfs_set_readonly (1);
if (!err)
err = diskfs_remount ();
}
if (h->readonly != diskfs_readonly)
{
if (err)
diskfs_set_readonly (h->readonly);
else
err = diskfs_set_readonly (h->readonly);
}
if (h->sync)
{
diskfs_synchronous = 1;
diskfs_set_sync_interval (0);
}
else
{
diskfs_synchronous = 0;
if (h->sync_interval >= 0)
diskfs_set_sync_interval (h->sync_interval);
}
if (h->nosuid != -1)
_diskfs_nosuid = h->nosuid;
if (h->noexec != -1)
_diskfs_noexec = h->noexec;
if (h->noatime != -1)
_diskfs_noatime = h->noatime;
if (h->relatime != -1)
_diskfs_relatime = h->relatime;
if (h->noinheritdirgroup != -1)
_diskfs_no_inherit_dir_group = h->noinheritdirgroup;
free (h);
return err;
}
static error_t
parse_opt (int opt, char *arg, struct argp_state *state)
{
struct parse_hook *h = state->hook;
switch (opt)
{
case 'r': h->readonly = 1; break;
case 'w': h->readonly = 0; break;
case 'u': h->remount = 1; break;
case 'S': h->nosuid = 1; break;
case 'E': h->noexec = 1; break;
case 'A':
{
h->relatime = -1;
h->noatime = 1;
break;
}
case 'R': h->relatime = 1; break;
case OPT_SUID_OK: h->nosuid = 0; break;
case OPT_EXEC_OK: h->noexec = 0; break;
case OPT_ATIME: h->noatime = h->relatime = 0; break;
case OPT_NO_INHERIT_DIR_GROUP: h->noinheritdirgroup = 1; break;
case OPT_INHERIT_DIR_GROUP: h->noinheritdirgroup = 0; break;
case 'n': h->sync_interval = 0; h->sync = 0; break;
case 's':
if (arg)
{
h->sync = 0;
h->sync_interval = atoi (arg);
}
else
h->sync = 1;
break;
case ARGP_KEY_INIT:
if (state->input)
state->hook = state->input;
else
{
h = state->hook = malloc (sizeof (struct parse_hook));
if (! h)
return ENOMEM;
h->readonly = diskfs_readonly;
h->sync = diskfs_synchronous;
h->sync_interval = -1;
h->remount = 0;
h->nosuid = h->noexec = h->noatime = h->noinheritdirgroup = h->relatime = -1;
state->child_inputs[0] = h;
}
break;
case ARGP_KEY_ERROR:
if (! state->input)
free (h);
break;
case ARGP_KEY_SUCCESS:
if (! state->input)
return set_opts (h);
break;
default:
return ARGP_ERR_UNKNOWN;
}
return 0;
}
static const struct argp common_argp = { diskfs_common_options, parse_opt };
static const struct argp_child children[] = { {&common_argp}, {0} };
const struct argp diskfs_std_runtime_argp =
{
std_runtime_options, parse_opt, 0, 0, children
};