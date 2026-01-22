#include <assert-backtrace.h>
#include <hurd.h>
#include <hurd/ports.h>
#include <pthread.h>
#include "cons.h"
void
cons_vcons_close (vcons_t vcons)
{
cons_t cons = vcons->cons;
vcons_list_t vcons_entry = vcons->vcons_entry;
pthread_mutex_lock (&cons->lock);
assert_backtrace (vcons_entry->vcons == vcons);
vcons_entry->vcons = NULL;
pthread_mutex_unlock (&cons->lock);
ports_port_deref (vcons);
ports_destroy_right (vcons);
}