#include <stdio.h>
#include <argp.h>
#include <version.h>
#include "priv.h"
static void
_print_version (FILE *stream, struct argp_state *state)
{
if (argp_program_version)
fputs (argp_program_version, stream);
else if (cons_extra_version && *cons_extra_version)
fprintf (stream, "%s (%s) %s\n",
cons_client_name, cons_extra_version, cons_client_version);
else
fprintf (stream, "%s %s\n", cons_client_name, cons_client_version);
fputs (STANDARD_HURD_VERSION (libcons) "\n", stream);
}
void (*argp_program_version_hook) (FILE *stream, struct argp_state *state)
= _print_version;