#include "iohelp.h"
#include <hurd/auth.h>
#include <sys/mman.h>
#include <stdlib.h>
error_t iohelp_reauth (struct iouser **user,
auth_t authserver, mach_port_t rend_port,
mach_port_t newright, int permit_failure)
{
uid_t gubuf[20], ggbuf[20], aubuf[20], agbuf[20];
uid_t *gen_uids, *gen_gids, *aux_uids, *aux_gids;
mach_msg_type_number_t genuidlen, gengidlen, auxuidlen, auxgidlen;
error_t err;
struct iouser *new;
*user = new = malloc (sizeof (struct iouser));
if (!new)
return ENOMEM;
new->uids = make_idvec ();
new->gids = make_idvec ();
if (!new->uids || !new->gids)
{
if (new->uids)
idvec_free (new->uids);
if (new->gids)
idvec_free (new->gids);
free (new);
return ENOMEM;
}
genuidlen = gengidlen = auxuidlen = auxgidlen = 20;
gen_uids = gubuf;
gen_gids = ggbuf;
aux_uids = aubuf;
aux_gids = agbuf;
do
err = auth_server_authenticate (authserver,
rend_port,
MACH_MSG_TYPE_COPY_SEND,
newright,
MACH_MSG_TYPE_COPY_SEND,
&gen_uids, &genuidlen,
&aux_uids, &auxuidlen,
&gen_gids, &gengidlen,
&aux_gids, &auxgidlen);
while (err == EINTR);
if (err)
{
if (permit_failure)
genuidlen = gengidlen = 0;
else
goto out;
}
err = idvec_set_ids (new->uids, gen_uids, genuidlen);
if (!err)
err = idvec_set_ids (new->gids, gen_gids, gengidlen);
if (gubuf != gen_uids)
munmap ((caddr_t) gen_uids, genuidlen * sizeof (uid_t));
if (ggbuf != gen_gids)
munmap ((caddr_t) gen_gids, gengidlen * sizeof (uid_t));
if (aubuf != aux_uids)
munmap ((caddr_t) aux_uids, auxuidlen * sizeof (uid_t));
if (agbuf != aux_gids)
munmap ((caddr_t) aux_gids, auxgidlen * sizeof (uid_t));
if (err)
{
out:
idvec_free (new->uids);
idvec_free (new->gids);
free (new);
*user = 0;
return err;
}
*user = new;
return 0;
}