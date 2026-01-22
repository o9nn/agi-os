#include "iohelp.h"
#include <stdlib.h>
void
iohelp_free_iouser (struct iouser *iouser)
{
idvec_free (iouser->uids);
idvec_free (iouser->gids);
free (iouser);
}