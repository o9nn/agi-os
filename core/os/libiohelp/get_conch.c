#include "iohelp.h"
void
iohelp_get_conch (struct conch *c)
{
struct shared_io *user_sh;
again:
user_sh = c->holder_shared_page;
if (user_sh)
{
pthread_spin_lock (&user_sh->lock);
switch (user_sh->conch_status)
{
case USER_HAS_CONCH:
user_sh->conch_status = USER_RELEASE_CONCH;
case USER_RELEASE_CONCH:
pthread_spin_unlock (&user_sh->lock);
pthread_cond_wait (&c->wait, c->lock);
goto again;
case USER_COULD_HAVE_CONCH:
user_sh->conch_status = USER_HAS_NOT_CONCH;
pthread_spin_unlock (&user_sh->lock);
iohelp_fetch_shared_data (c->holder);
break;
case USER_HAS_NOT_CONCH:
pthread_spin_unlock (&user_sh->lock);
break;
}
}
c->holder = 0;
c->holder_shared_page = 0;
}