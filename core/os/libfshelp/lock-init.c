#include <sys/file.h>
#include "fshelp.h"
void
fshelp_lock_init (struct lock_box *box)
{
box->type = LOCK_UN;
pthread_cond_init (&box->wait, NULL);
box->waiting = 0;
box->shcount = 0;
}