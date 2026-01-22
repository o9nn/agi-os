#include <hurd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <argp.h>
#include <fcntl.h>
#include <unistd.h>
#include <version.h>
#include <sys/mman.h>
#include <error.h>
#include <argz.h>
const char *argp_program_version = STANDARD_HURD_VERSION (showtrans);
static struct argp_option options[] =
{
{"prefix", 'p', 0, 0, "Always display `FILENAME: ' before translators"},
{"no-prefix", 'P', 0, 0, "Never display `FILENAME: ' before translators"},
{"silent", 's', 0, 0, "No output; useful when checking error status"},
{"quiet", 'q', 0, OPTION_ALIAS | OPTION_HIDDEN},
{"translated",'t', 0, 0, "Only display files that have translators"},
{0, 0}
};
static char *args_doc = "FILE...";
static char *doc = "Show the passive translator of FILE..."
"\vA FILE argument of `-' prints the translator on the node"
" attached to standard input.";
int
main (int argc, char *argv[])
{
int status = 1;
int print_prefix = -1, silent = 0, show_untrans = 1;
void print_node_trans (file_t node, char *name)
{
if (node == MACH_PORT_NULL)
error (0, errno, "%s", name);
else
{
char buf[1024], *trans = buf;
mach_msg_type_number_t trans_len = sizeof (buf);
error_t err = file_get_translator (node, &trans, &trans_len);
switch (err)
{
case 0:
argz_stringify (trans, trans_len, ' ');
if (!silent)
{
if (print_prefix)
printf ("%s: %.*s\n", name, (int) trans_len, trans);
else
printf ("%.*s\n", (int) trans_len, trans);
}
if (trans != buf)
munmap (trans, trans_len);
status = 0;
break;
case EINVAL:
if (!silent && print_prefix && show_untrans)
puts (name);
break;
default:
error (0, err, "%s", name);
}
mach_port_deallocate (mach_task_self (), node);
}
}
error_t parse_opt (int key, char *arg, struct argp_state *state)
{
switch (key)
{
case ARGP_KEY_ARG:
if (print_prefix < 0)
print_prefix = state->next < state->argc;
if (strcmp (arg, "-") != 0)
print_node_trans (file_name_lookup (arg, O_NOTRANS, 0), arg);
else
print_node_trans (getdport (0), "-");
break;
case 'p': print_prefix = 1; break;
case 'P': print_prefix = 0; break;
case 's': case 'q': silent = 1; break;
case 't': show_untrans = 0; break;
case ARGP_KEY_NO_ARGS:
argp_usage (state);
default:
return ARGP_ERR_UNKNOWN;
}
return 0;
}
struct argp argp = {options, parse_opt, args_doc, doc};
argp_parse (&argp, argc, argv, 0, 0, 0);
return status;
}