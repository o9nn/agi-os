#include "iohelp.h"
void
iohelp_handle_io_get_conch (struct conch *c, void *user,
struct shared_io *user_sh)
{
if (c->holder == user)
{
if (user_sh->conch_status != USER_HAS_NOT_CONCH)
iohelp_fetch_shared_data (user);
else
user_sh->accessed = user_sh->written = 0;
iohelp_put_shared_data (user);
user_sh->conch_status = USER_HAS_CONCH;
}
else
{
iohelp_get_conch (c);
c->holder = user;
c->holder_shared_page = user_sh;
if (user_sh->conch_status == USER_HAS_NOT_CONCH)
user_sh->accessed = user_sh->written = 0;
user_sh->conch_status = USER_HAS_CONCH;
iohelp_put_shared_data (user);
}
}