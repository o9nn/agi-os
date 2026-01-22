#include <sys/reboot.h>
#include <unistd.h>
#include <stdio.h>
#include <argp.h>
#include <error.h>
#include <hurd.h>
#include <version.h>
const char *argp_program_version = STANDARD_HURD_VERSION (halt);
int
main (int argc, char *argv[])
{
struct argp argp = {0, 0, 0, "Halt the system"};
argp_parse (&argp, argc, argv, 0, 0, 0);
reboot (RB_HALT);
error (1, errno, "reboot");
return 1;
}