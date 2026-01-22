#include <argp.h>
#include <error.h>
#include <nullauth.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <version.h>
static char **args;
const char *argp_program_version = STANDARD_HURD_VERSION (nullauth);
static const struct argp_option options[] =
{
{ 0 }
};
static const char doc[] =
"Drop all authentication credentials and run the given program.";
static const char args_doc[] =
"PROGRAM [ARGUMENTS...]\tThe program to run";
error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
switch (key)
{
case ARGP_KEY_ARGS:
args = state->argv + state->next;
break;
case ARGP_KEY_NO_ARGS:
argp_error (state, "expected program to run");
return EINVAL;
default:
return ARGP_ERR_UNKNOWN;
}
return 0;
}
static struct argp argp = {
options,
parse_opt,
args_doc,
doc,
NULL,
};
int
main (int argc, char *argv[])
{
error_t err;
argp_parse (&argp, argc, argv, 0, 0, NULL);
err = setnullauth();
if (err)
error (1, err, "Could not drop privileges");
execv (args[0], args);
error (1, errno, "execv");
return EXIT_FAILURE;
}