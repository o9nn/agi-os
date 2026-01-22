#include "options.h"
#include <stdlib.h>
#include <argp.h>
#include <argz.h>
#include <error.h>
#include <regex.h>
#include "pcifs.h"
#define PCI_SLOT_REGEX "^(([0-9a-fA-F]{4}):)?([0-9a-fA-F]{2}):([0-9a-fA-F]{2})\\.([0-7])$"
#define PCI_SLOT_REGEX_GROUPS 6
static error_t
parse_hook_add_set (struct parse_hook *h)
{
struct pcifs_perm *new = realloc (h->permsets,
(h->num_permsets +
1) * sizeof (struct pcifs_perm));
if (!new)
return ENOMEM;
h->permsets = new;
h->num_permsets++;
h->curset = new + h->num_permsets - 1;
h->curset->domain = -1;
h->curset->bus = -1;
h->curset->dev = -1;
h->curset->func = -1;
h->curset->d_class = -1;
h->curset->d_subclass = -1;
h->curset->uid = -1;
h->curset->gid = -1;
return 0;
}
static error_t
check_options_validity (struct parse_hook *h)
{
int i;
struct pcifs_perm *p;
for (p = h->permsets, i = 0; i < h->num_permsets; i++, p++)
{
if ((p->func >= 0 && p->dev < 0)
|| (p->dev >= 0 && p->bus < 0)
|| (p->bus >= 0 && p->domain < 0)
|| (p->d_subclass >= 0 && p->d_class < 0)
|| ((p->uid >= 0 || p->gid >= 0)
&& (p->d_class < 0 && p->domain < 0)) || ((p->d_class >= 0
|| p->domain >= 0)
&& !(p->uid >= 0
|| p->gid >= 0)))
return EINVAL;
}
return 0;
}
static long int
parse_number (const char *s)
{
long int val;
char *endptr;
errno = 0;
val = strtol (s, &endptr, 16);
if (*endptr != '\0' || errno)
{
val = -1;
errno = EINVAL;
}
return val;
}
static error_t
parse_opt (int opt, char *arg, struct argp_state *state)
{
error_t err = 0;
struct parse_hook *h = state->hook;
regex_t slot_regex;
regmatch_t slot_regex_groups[PCI_SLOT_REGEX_GROUPS];
char regex_group_val[5];
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
err = regcomp (&slot_regex, PCI_SLOT_REGEX, REG_EXTENDED);
if (err)
FAIL (err, 1, err, "option parsing");
switch (opt)
{
case 'C':
h->curset->d_class = parse_number (arg);
if (errno)
PERR (errno, "Invalid class");
break;
case 'c':
h->curset->d_subclass = parse_number (arg);
if (errno)
PERR (errno, "Invalid subclass");
break;
case 'd':
h->curset->domain = parse_number (arg);
if (errno)
PERR (errno, "Invalid domain");
break;
case 'b':
if (h->curset->domain < 0)
h->curset->domain = 0;
h->curset->bus = parse_number (arg);
if (errno)
PERR (errno, "Invalid bus");
break;
case 's':
h->curset->dev = parse_number (arg);
if (errno)
PERR (errno, "Invalid slot");
break;
case 'f':
h->curset->func = parse_number (arg);
if (errno)
PERR (errno, "Invalid function");
break;
case 'D':
err =
regexec (&slot_regex, arg, PCI_SLOT_REGEX_GROUPS, slot_regex_groups,
0);
if (!err)
{
if (slot_regex_groups[2].rm_so >= 0)
{
strncpy (regex_group_val, arg + slot_regex_groups[2].rm_so, 4);
regex_group_val[4] = 0;
}
else
{
strncpy (regex_group_val, "0000", 5);
}
h->curset->domain = parse_number (regex_group_val);
if (errno)
PERR (errno, "Invalid domain");
strncpy (regex_group_val, arg + slot_regex_groups[3].rm_so, 2);
regex_group_val[2] = 0;
h->curset->bus = parse_number (regex_group_val);
if (errno)
PERR (errno, "Invalid bus");
strncpy (regex_group_val, arg + slot_regex_groups[4].rm_so, 2);
regex_group_val[2] = 0;
h->curset->dev = parse_number (regex_group_val);
if (errno)
PERR (errno, "Invalid slot");
regex_group_val[0] = arg[slot_regex_groups[5].rm_so];
regex_group_val[1] = 0;
h->curset->func = parse_number (regex_group_val);
if (errno)
PERR (errno, "Invalid func");
}
else
{
PERR (err, "Wrong PCI slot. Format: [<domain>:]<bus>:<dev>.<func>");
}
break;
case 'U':
if (h->curset->uid >= 0)
parse_hook_add_set (h);
h->curset->uid = atoi (arg);
break;
case 'G':
if (h->curset->gid >= 0)
parse_hook_add_set (h);
h->curset->gid = atoi (arg);
break;
case 'n':
h->ncache_len = atoi (arg);
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
h->permsets = 0;
h->num_permsets = 0;
h->ncache_len = NODE_CACHE_MAX;
h->next_task = MACH_PORT_NULL;
h->host_priv_port = MACH_PORT_NULL;
h->dev_master_port = MACH_PORT_NULL;
err = parse_hook_add_set (h);
if (err)
FAIL (err, 1, err, "option parsing");
state->hook = h;
break;
case ARGP_KEY_SUCCESS:
err = check_options_validity (h);
if (err)
{
if (fs->root)
{
free (h->permsets);
free (h);
PERR (err, "Invalid options, no changes were applied");
}
else
PERR (err, "Option dependence error");
}
if (fs->params.perms)
free (fs->params.perms);
fs->params.perms = h->permsets;
fs->params.num_perms = h->num_permsets;
fs->params.node_cache_max = h->ncache_len;
fs->params.next_task = h->next_task;
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
free (h->permsets);
free (h);
break;
default:
return ARGP_ERR_UNKNOWN;
}
regfree (&slot_regex);
return err;
}
error_t
netfs_append_args (char **argz, size_t * argz_len)
{
error_t err = 0;
struct pcifs_perm *p;
int i;
#define ADD_OPT(fmt, args...) \
do { char buf[100]; \
if (! err) { \
snprintf (buf, sizeof buf, fmt , ##args); \
err = argz_add (argz, argz_len, buf); } } while (0)
for (i = 0, p = fs->params.perms; i < fs->params.num_perms; i++, p++)
{
if (p->d_class >= 0)
ADD_OPT ("--class=0x%02x", p->d_class);
if (p->d_subclass >= 0)
ADD_OPT ("--subclass=0x%02x", p->d_subclass);
if (p->domain >= 0)
ADD_OPT ("--domain=0x%04x", p->domain);
if (p->bus >= 0)
ADD_OPT ("--bus=0x%02x", p->bus);
if (p->dev >= 0)
ADD_OPT ("--slot=0x%02x", p->dev);
if (p->func >= 0)
ADD_OPT ("--func=%01u", p->func);
if (p->uid >= 0)
ADD_OPT ("--uid=%u", p->uid);
if (p->gid >= 0)
ADD_OPT ("--gid=%u", p->gid);
}
if (fs->params.node_cache_max != NODE_CACHE_MAX)
ADD_OPT ("--ncache=%zu", fs->params.node_cache_max);
if (fs->params.next_task != MACH_PORT_NULL)
ADD_OPT ("--next-task=%u", fs->params.next_task);
#undef ADD_OPT
return err;
}
struct argp pci_argp = { options, parse_opt, 0, doc };
struct argp *netfs_runtime_argp = &pci_argp;