#include <mach.h>
#include <argp.h>
#include <argz.h>
#include <alloca.h>
#include "fshelp.h"
error_t
fshelp_set_options (const struct argp *argp, int flags,
const char *argz, size_t argz_len, void *input)
{
int argc = argz_count (argz, argz_len);
char **argv = alloca (sizeof (char *) * (argc + 1));
argz_extract ((char *) argz, argz_len, argv);
return
argp_parse (argp, argc, argv,
flags | ARGP_NO_ERRS | ARGP_NO_HELP | ARGP_PARSE_ARGV0,
0, input);
}