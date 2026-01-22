#ifndef __FROBAUTH_H__
#define __FROBAUTH_H__
#include <sys/types.h>
#include <ugids.h>
#include <argp.h>
struct frobauth
{
struct ugids ugids;
pid_t *pids;
mach_msg_type_number_t num_pids;
int verbose, dry_run;
uid_t default_user;
int require_ids;
};
#define FROBAUTH_INIT { UGIDS_INIT, 0, 0, 0, 0, -1 }
error_t frobauth_modify (struct frobauth *frobauth,
const auth_t *auths, size_t num_auths,
error_t (*modify) (struct ugids *ugids,
const struct ugids *change,
pid_t pid, void *hook),
void (*print_info) (const struct ugids *new,
const struct ugids *old,
const struct ugids *change,
pid_t pid, void *hook),
void *hook);
extern struct argp frobauth_ea_argp;
extern struct argp frobauth_posix_argp;
extern struct argp frobauth_no_ugids_argp;
#endif