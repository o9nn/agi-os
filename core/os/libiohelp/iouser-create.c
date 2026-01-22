#include "iohelp.h"
#include <stdlib.h>
error_t
iohelp_create_iouser (struct iouser **user, struct idvec *uids,
struct idvec *gids)
{
struct iouser *new;
*user = new = malloc (sizeof (struct iouser));
if (!new)
return ENOMEM;
new->uids = uids;
new->gids = gids;
new->hook = 0;
return 0;
}
#define E(err_)				\
do {				\
error_t err = err_;		\
if (err)			\
{				\
*user = 0;		\
if (! uids)		\
return err;		\
idvec_free (uids);	\
if (! gids)		\
return err;		\
idvec_free (gids);	\
return err;		\
}				\
} while (0)
error_t
iohelp_create_empty_iouser (struct iouser **user)
{
struct idvec *uids, *gids;
uids = make_idvec ();
if (! uids)
E (ENOMEM);
gids = make_idvec ();
if (! gids)
E (ENOMEM);
E (iohelp_create_iouser (user, uids, gids));
return 0;
}
error_t
iohelp_create_simple_iouser (struct iouser **user, uid_t uid, gid_t gid)
{
struct idvec *uids, *gids;
uids = make_idvec ();
if (! uids)
E (ENOMEM);
gids = make_idvec ();
if (! gids)
E (ENOMEM);
E (idvec_add (uids, uid));
E (idvec_add (gids, gid));
E (iohelp_create_iouser (user, uids, gids));
return 0;
}
error_t
iohelp_create_complex_iouser (struct iouser **user,
const uid_t *uvec, int nuids,
const gid_t *gvec, int ngids)
{
struct idvec *uids, *gids;
uids = make_idvec ();
if (! uids)
E (ENOMEM);
gids = make_idvec ();
if (! gids)
E (ENOMEM);
E (idvec_set_ids (uids, uvec, nuids));
E (idvec_set_ids (gids, gvec, ngids));
E (iohelp_create_iouser (user, uids, gids));
return 0;
}