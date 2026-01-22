#include "iohelp.h"
void
iohelp_handle_io_release_conch (struct conch *c, void *user)
{
struct shared_io *user_sh = c->holder_shared_page;
pthread_spin_lock (&user_sh->lock);
if (c->holder_shared_page->conch_status != USER_HAS_NOT_CONCH)
{
c->holder_shared_page->conch_status = USER_HAS_NOT_CONCH;
iohelp_fetch_shared_data (c->holder);
}
pthread_spin_unlock (&user_sh->lock);
if (c->holder == user)
{
c->holder = 0;
c->holder_shared_page = 0;
}
pthread_cond_broadcast (&c->wait);
}