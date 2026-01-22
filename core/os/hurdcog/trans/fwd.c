#include <error.h>
#include <stdio.h>
#include <hurd/fshelp.h>
int
main (int argc, char **argv)
{
error_t err;
mach_port_t bootstrap;
if (argc < 2 || *argv[1] == '-')
{
fprintf (stderr, "Usage: %s SERVER [TRANS_NAME [TRANS_ARG...]]\n",
program_invocation_name);
return 1;
}
task_get_bootstrap_port (mach_task_self (), &bootstrap);
if (bootstrap == MACH_PORT_NULL)
error (2, 0, "must be started as a translator");
err = fshelp_delegate_translation (argv[1], bootstrap, argv + 2);
if (err)
error (3, err, "%s", argv[1]);
return 0;
}