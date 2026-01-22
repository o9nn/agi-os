#include <options.h>
#include <stdlib.h>
#include <argp.h>
#include <argz.h>
#include <error.h>
#include <acpifs.h>
static error_t
parse_opt (int opt, char *arg, struct argp_state *state)
{
error_t err = 0;
struct parse_hook *h = state->hook;
#define RETURN(_err) \
do { return _err; } while (0)
#define PERR(err, fmt, args...) \
do { argp_error (state, fmt , ##args); RETURN (err); } while (0)
#define FAIL(rerr, status, perr, fmt, args...) \
do{ argp_failure (state, status, perr, fmt , ##args); RETURN (rerr); } while(0)
if (!arg && state->next < state->argc && (*state->argv[state->next] != '-'))
{
arg = state->argv[state->next];
state->next++;
}
switch (opt)
{
case 'U':
h->perm.uid = atoi (arg);
break;
case 'G':
h->perm.gid = atoi (arg);
break;
case 'N':
h->next_task = atoi (arg);
break;
case 'H':
h->host_priv_port = atoi (arg);
break;
case 'P':
h->dev_master_port = atoi (arg);
break;
case ARGP_KEY_INIT:
h = malloc (sizeof (struct parse_hook));
if (!h)
FAIL (ENOMEM, 1, ENOMEM, "option parsing");
h->ncache_len = NODE_CACHE_MAX;
h->perm.uid = 0;
h->perm.gid = 0;
h->next_task = MACH_PORT_NULL;
h->host_priv_port = MACH_PORT_NULL;
h->dev_master_port = MACH_PORT_NULL;
state->hook = h;
break;
case ARGP_KEY_SUCCESS:
fs->perm = h->perm;
fs->node_cache_max = h->ncache_len;
fs->next_task = h->next_task;
_hurd_host_priv = h->host_priv_port;
_hurd_device_master = h->dev_master_port;
if (fs->root)
{
err = ports_inhibit_all_rpcs ();
if (err)
return err;
err = fs_set_permissions (fs);
ports_resume_all_rpcs ();
}
free (h);
break;
case ARGP_KEY_ERROR:
free (h);
break;
default:
return ARGP_ERR_UNKNOWN;
}
return err;
}
error_t
netfs_append_args (char **argz, size_t * argz_len)
{
error_t err = 0;
struct acpifs_perm *p;
#define ADD_OPT(fmt, args...) \
do { char buf[100]; \
if (! err) { \
snprintf (buf, sizeof buf, fmt , ##args); \
err = argz_add (argz, argz_len, buf); } } while (0)
p = &fs->perm;
if (p->uid >= 0)
ADD_OPT ("--uid=%u", p->uid);
if (p->gid >= 0)
ADD_OPT ("--gid=%u", p->gid);
if (fs->next_task != MACH_PORT_NULL)
ADD_OPT ("--next-task=%u", fs->next_task);
#undef ADD_OPT
return err;
}
struct argp acpi_argp = { options, parse_opt, 0, doc };
struct argp *netfs_runtime_argp = &acpi_argp;