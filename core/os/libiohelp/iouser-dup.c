#include "iohelp.h"
#include <stdlib.h>
error_t
iohelp_dup_iouser (struct iouser **clone, struct iouser *iouser)
{
struct iouser *new;
error_t err = 0;
*clone = new = malloc (sizeof (struct iouser));
if (!new)
return ENOMEM;
new->uids = make_idvec ();
new->gids = make_idvec ();
new->hook = 0;
if (!new->uids || !new->gids)
{
err = ENOMEM;
goto lose;
}
err = idvec_set (new->uids, iouser->uids);
if (!err)
err = idvec_set (new->gids, iouser->gids);
if (err)
{
lose:
if (new->uids)
idvec_free (new->uids);
if (new->gids)
idvec_free (new->gids);
free (new);
*clone = 0;
return err;
}
return 0;
}