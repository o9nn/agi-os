#include <stdio.h>
#include <argp.h>
#include "netfs.h"
static const struct argp_option
startup_options[] =
{
{0}
};
static error_t
parse_startup_opt (int opt, char *arg, struct argp_state *state)
{
switch (opt)
{
default:
return ARGP_ERR_UNKNOWN;
}
return 0;
}
static const struct argp
startup_argp =
{ startup_options, parse_startup_opt };
const struct argp *netfs_startup_argp = &startup_argp;