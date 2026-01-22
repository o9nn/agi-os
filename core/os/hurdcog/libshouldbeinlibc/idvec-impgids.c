#include <stdlib.h>
#include <errno.h>
#include <idvec.h>
#include <pwd.h>
#include <grp.h>
#define NUM_STATIC_GIDS 100
struct uid_implies
{
uid_t uid;
struct idvec *implies;
struct uid_implies *next;
};
static struct uid_implies *uid_implies_cache = 0;
static error_t
_merge_implied_gids (struct idvec *implied_gids, uid_t uid)
{
struct uid_implies *ui;
for (ui = uid_implies_cache; ui; ui = ui->next)
if (ui->uid == uid)
return idvec_merge (implied_gids, ui->implies);
{
error_t err = 0;
struct passwd *pw = getpwuid (uid);
if (! pw)
err = EINVAL;
else
{
struct idvec *cache = make_idvec ();
gid_t _gids[NUM_STATIC_GIDS], *gids = _gids;
int maxgids = NUM_STATIC_GIDS;
int ngids = getgrouplist (pw->pw_name, pw->pw_gid, gids, &maxgids);
if (ngids == -1)
{
gids = malloc (maxgids * sizeof (gid_t));
if (! gids)
err = ENOMEM;
else
ngids = getgrouplist (pw->pw_name, pw->pw_gid, gids, &maxgids);
}
if (! cache)
err = ENOMEM;
if (! err)
{
err = idvec_merge_ids (cache, gids, ngids);
if (gids != _gids)
free (gids);
}
if (! err)
{
idvec_merge (implied_gids, cache);
ui = malloc (sizeof (struct uid_implies));
if (ui)
{
ui->uid = uid;
ui->implies = cache;
ui->next = uid_implies_cache;
uid_implies_cache = ui;
}
else
idvec_free (cache);
}
else if (cache)
idvec_free (cache);
}
return err;
}
}
error_t
idvec_merge_implied_gids (struct idvec *gids, const struct idvec *uids)
{
unsigned int i;
error_t err = 0;
for (i = 0; i < uids->num; i++)
{
error_t this_err = _merge_implied_gids (gids, uids->ids[i]);
if (this_err && !err)
err = this_err;
}
return err;
}