#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <hurd.h>
#include <argp.h>
#include <error.h>
#include <version.h>
#include "frobauth.h"
#include "pids.h"
const char *argp_program_version = STANDARD_HURD_VERSION (unsu);
static struct argp_child child_argps[] = {{ &frobauth_no_ugids_argp }, { 0 }};
static char doc[] =
"Attempt to undo a previous setauth --save"
"\vThis command is convenient, but will only correctly undo a limited"
" subset of possible setauth commands.  It works by simply deleting all"
" current effective ids and the first two available ids, and then"
" making the first remaining available id the current effective id.";
int
main (int argc, char *argv[])
{
struct frobauth frobauth = FROBAUTH_INIT;
error_t modify (struct ugids *ugids, const struct ugids *noise,
pid_t pid, void *hook)
{
error_t err = 0;
idvec_clear (&ugids->eff_uids);
idvec_clear (&ugids->eff_gids);
idvec_clear (&ugids->imp_eff_gids);
idvec_delete (&ugids->avail_uids, 0);
idvec_delete (&ugids->avail_uids, 0);
idvec_delete (&ugids->avail_gids, 0);
idvec_delete (&ugids->avail_gids, 0);
idvec_keep (&ugids->imp_avail_gids, &ugids->avail_gids);
if (ugids->avail_uids.num > 0)
err = ugids_set_posix_user (ugids, ugids->avail_uids.ids[0]);
return err;
}
void print_info (const struct ugids *new,
const struct ugids *old,
const struct ugids *removed,
pid_t pid, void *hook)
{
char *new_rep = ugids_rep (new, 1, 1, 0, 0, 0);
printf ("%d: Changed auth to %s\n", pid, new_rep);
free (new_rep);
}
struct argp argp = { 0, 0, 0, doc, child_argps };
argp_parse (&argp, argc, argv, 0, 0, &frobauth);
if (frobauth_modify (&frobauth, 0, 0, modify, print_info, 0))
return 0;
else
return 1;
}