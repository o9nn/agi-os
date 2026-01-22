#ifndef __UGIDS_H__
#define __UGIDS_H__
#include <stdlib.h>
#include <idvec.h>
#include <features.h>
#include <errno.h>
#include <sys/types.h>
#ifdef UGIDS_DEFINE_EI
#define UGIDS_EI
#else
#define UGIDS_EI __extern_inline
#endif
struct ugids
{
struct idvec eff_uids;
struct idvec eff_gids;
struct idvec avail_uids;
struct idvec avail_gids;
struct idvec imp_eff_gids;
struct idvec imp_avail_gids;
};
#define UGIDS_INIT { IDVEC_INIT, IDVEC_INIT, IDVEC_INIT, IDVEC_INIT, IDVEC_INIT, IDVEC_INIT }
struct ugids *make_ugids (void);
extern void ugids_fini (struct ugids *ugids);
extern void ugids_free (struct ugids *ugids);
extern int ugids_is_empty (const struct ugids *ugids);
extern int ugids_equal (const struct ugids *ugids1, const struct ugids *ugids2);
#if defined(__USE_EXTERN_INLINES) || defined(UGIDS_DEFINE_EI)
UGIDS_EI void
ugids_fini (struct ugids *ugids)
{
idvec_fini (&ugids->eff_uids);
idvec_fini (&ugids->eff_gids);
idvec_fini (&ugids->avail_uids);
idvec_fini (&ugids->avail_gids);
idvec_fini (&ugids->imp_eff_gids);
idvec_fini (&ugids->imp_avail_gids);
}
UGIDS_EI void
ugids_free (struct ugids *ugids)
{
ugids_fini (ugids);
free (ugids);
}
UGIDS_EI int
ugids_is_empty (const struct ugids *ugids)
{
return
idvec_is_empty (&ugids->eff_uids)
&& idvec_is_empty (&ugids->eff_gids)
&& idvec_is_empty (&ugids->avail_uids)
&& idvec_is_empty (&ugids->avail_gids);
}
UGIDS_EI int
ugids_equal (const struct ugids *ugids1, const struct ugids *ugids2)
{
return
idvec_equal (&ugids1->eff_uids, &ugids2->eff_uids)
&& idvec_equal (&ugids1->eff_gids, &ugids2->eff_gids)
&& idvec_equal (&ugids1->avail_uids, &ugids2->avail_uids)
&& idvec_equal (&ugids1->avail_gids, &ugids2->avail_gids)
&& idvec_equal (&ugids1->imp_eff_gids, &ugids2->imp_eff_gids)
&& idvec_equal (&ugids1->imp_avail_gids, &ugids2->imp_avail_gids);
}
#endif
error_t ugids_merge (struct ugids *ugids, const struct ugids *new);
error_t ugids_set (struct ugids *ugids, const struct ugids *new);
error_t ugids_subtract (struct ugids *ugids, const struct ugids *sub);
error_t ugids_imply_all (struct ugids *ugids);
error_t ugids_save (struct ugids *ugids);
error_t ugids_verify (const struct ugids *ugids,
const struct idvec *have_uids,
const struct idvec *have_gids,
char *(*getpass_fn) (const char *prompt,
uid_t id, int is_group,
void *pwd_or_grp, void *hook),
void *getpass_hook,
error_t (*verify_fn) (const char *password,
uid_t id, int is_group,
void *pwd_or_grp, void *hook),
void *verify_hook);
error_t ugids_make_auth (const struct ugids *ugids,
const auth_t *from, size_t num_from,
auth_t *auth);
error_t ugids_verify_make_auth (const struct ugids *ugids,
const struct idvec *have_uids,
const struct idvec *have_gids,
char *(*getpass_fn) (const char *prompt,
uid_t id, int is_group,
void *pwd_or_grp,
void *hook),
void *getpass_hook,
const auth_t *from, size_t num_from,
auth_t *auth);
error_t ugids_merge_auth (struct ugids *ugids, auth_t auth);
char *ugids_rep (const struct ugids *ugids, int show_values, int show_names,
const char *id_sep, const char *type_sep,
const char *hdr_sep);
error_t ugids_add_uid (struct ugids *ugids, uid_t uid, int avail);
error_t ugids_add_gid (struct ugids *ugids, gid_t gid, int avail);
error_t ugids_add_user (struct ugids *ugids, uid_t uid, int avail);
error_t ugids_set_posix_user (struct ugids *ugids, uid_t uid);
struct ugids_argp_params
{
struct ugids *ugids;
int parse_user_args;
int user_args_are_effective;
int user_args_are_available;
int default_user;
int require_ids;
};
extern struct argp ugids_argp;
#endif