#include <stdio.h>
#include <argp.h>
#include <version.h>
#include "priv.h"
static void
_print_version (FILE *stream, struct argp_state *state)
{
if (argp_program_version)
fputs (argp_program_version, stream);
else if (diskfs_extra_version && *diskfs_extra_version)
fprintf (stream, "%s (%s) %s\n",
diskfs_server_name, diskfs_extra_version, diskfs_server_version);
else
fprintf (stream, "%s %s\n", diskfs_server_name, diskfs_server_version);
fputs (STANDARD_HURD_VERSION (libdiskfs) "\n", stream);
}
void (*argp_program_version_hook) (FILE *stream, struct argp_state *state)
= _print_version;