#include <hurd.h>
#include <argp.h>
#include "idvec.h"
#include "ugids.h"
error_t
ugids_verify (const struct ugids *ugids,
const struct idvec *have_uids, const struct idvec *have_gids,
char *(*getpass_fn) (const char *prompt,
uid_t id, int is_group,
void *pwd_or_grp, void *hook),
void *getpass_hook,
error_t (*verify_fn) (const char *password,
uid_t id, int is_group,
void *pwd_or_grp, void *hook),
void *verify_hook)
{
error_t err;
struct idvec check_uids = IDVEC_INIT;
struct idvec check_gids = IDVEC_INIT;
err = idvec_merge (&check_uids, &ugids->eff_uids);
if (! err)
err = idvec_merge (&check_uids, &ugids->avail_uids);
if (! err)
err = idvec_merge (&check_gids, &ugids->eff_gids);
if (! err)
err = idvec_merge (&check_gids, &ugids->avail_gids);
if (! err)
err = idvec_verify (&check_uids, &check_gids, have_uids, have_gids,
getpass_fn, getpass_hook, verify_fn, verify_hook);
idvec_fini (&check_uids);
idvec_fini (&check_gids);
return err;
}