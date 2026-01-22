#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <argp.h>
#include <pwd.h>
#include <grp.h>
#include <errno.h>
#include "ugids.h"
#define OA OPTION_ARG_OPTIONAL
static const struct argp_option options[] =
{
{"user", 'u', "USER", 0, "Add USER to the effective uids"},
{"avail-user",'U', "USER", 0, "Add USER to the available uids"},
{"group", 'g', "GROUP", 0, "Add GROUP to the effective groups"},
{"avail-group",'G',"GROUP", 0, "Add GROUP to the available groups"},
{ 0 }
};
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
char id_lookup_buf[1024];
struct ugids_argp_params *params = state->input;
struct ugids *ugids = params->ugids;
switch (key)
{
uid_t uid;
case 'u':
case 'U':
case ARGP_KEY_ARG:
case ARGP_KEY_END:
if (key == ARGP_KEY_ARG && !params->parse_user_args)
return ARGP_ERR_UNKNOWN;
if (key == ARGP_KEY_END)
{
if (ugids_is_empty (ugids))
{
if (params->default_user >= 0)
uid = params->default_user;
else if (params->require_ids)
{
argp_error (state, "No ids specified");
return EINVAL;
}
else
break;
}
else
break;
}
else if (isdigit (*arg))
uid = atoi (arg);
else if (strcmp (arg, "-") == 0)
break;
else
{
struct passwd _pw, *pw;
int err;
err = getpwnam_r (arg, &_pw, id_lookup_buf,
sizeof id_lookup_buf, &pw);
if (err == 0)
{
if (pw == NULL)
{
argp_failure (state, 10, 0, "%s: Unknown user", arg);
return EINVAL;
}
uid = pw->pw_uid;
}
else
{
argp_failure (state, 12, err,
"Could not get uid for user: %s", arg);
return err;
}
}
if (key == ARGP_KEY_ARG || key == ARGP_KEY_END)
{
if (!params->user_args_are_effective
&& !params->user_args_are_available)
return ugids_set_posix_user (ugids, uid);
else
{
error_t err = 0;
if (params->user_args_are_effective)
err = ugids_add_user (ugids, uid, 0);
if (!err && params->user_args_are_available)
err = ugids_add_user (ugids, uid, 1);
return err;
}
}
else
return ugids_add_uid (ugids, uid, key == 'U');
case 'g':
case 'G':
if (isdigit (*arg))
return ugids_add_gid (ugids, atoi (arg), key == 'G');
else
{
struct group _gr, *gr;
int err = getgrnam_r (arg, &_gr, id_lookup_buf,
sizeof id_lookup_buf, &gr);
if (err == 0)
{
if (gr == NULL)
{
argp_failure (state, 11, 0, "%s: Unknown group", arg);
return EINVAL;
}
return ugids_add_gid (ugids, gr->gr_gid, key == 'G');
}
else
{
argp_failure (state, 13, err,
"Could not get gid for group: %s", arg);
return err;
}
}
default:
return ARGP_ERR_UNKNOWN;
}
return 0;
}
static char *
help_filter (int key, const char *text, void *input)
{
struct ugids_argp_params *params = input;
if (key == ARGP_KEY_HELP_ARGS_DOC && params->parse_user_args)
return strdup ("[USER...]");
return (char *)text;
}
struct argp ugids_argp = { options, parse_opt, 0, 0, 0, help_filter };