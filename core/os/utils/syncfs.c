#include <hurd.h>
#include <argp.h>
#include <error.h>
#include <version.h>
const char *argp_program_version = STANDARD_HURD_VERSION (syncfs);
static int synchronous = 0, do_children = 1;
static void
sync_one (const char *name, file_t port)
{
error_t err = (port == MACH_PORT_NULL ? errno
: file_syncfs (port, synchronous, do_children));
if (err)
error (1, err, "%s", name);
}
static error_t
parser (int key, char *arg, struct argp_state *state)
{
switch (key)
{
case 's': synchronous = 1; break;
case 'c': do_children = 0; break;
case ARGP_KEY_NO_ARGS:
sync_one ("/", getcrdir ());
break;
case ARGP_KEY_ARG:
sync_one (arg, file_name_lookup (arg, 0, 0));
break;
default:
return ARGP_ERR_UNKNOWN;
}
return 0;
}
int
main (int argc, char *argv[])
{
static struct argp_option options[] =
{
{"synchronous", 's', 0, 0, "Wait for completion of all disk writes"},
{"no-children", 'c', 0, 0, "Do not synchronize child filesystems"},
{0}
};
struct argp argp =
{options, parser,
"[FILE...]", "Force all pending disk writes to be done immediately"
"\vThe filesystem containing each FILE is synchronized, and its child"
" filesystems unless --no-children is specified.  With no FILE argument"
" synchronizes the root filesystem."};
argp_parse (&argp, argc, argv, 0, 0, 0);
return 0;
}