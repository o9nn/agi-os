#include "iohelp.h"
#include <errno.h>
error_t
iohelp_verify_user_conch (struct conch *c, void *user)
{
struct shared_io *user_sh;
if (user != c->holder)
return EPERM;
user_sh = c->holder_shared_page;
pthread_spin_lock (&user_sh->lock);
if (user_sh->conch_status != USER_HAS_CONCH
&& user_sh->conch_status != USER_RELEASE_CONCH)
{
pthread_spin_unlock (&user_sh->lock);
return EPERM;
}
pthread_spin_unlock (&user_sh->lock);
return 0;
}