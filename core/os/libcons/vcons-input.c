#include <errno.h>
#include <unistd.h>
#include <pthread.h>
#include "cons.h"
#include "priv.h"
error_t
_cons_vcons_input (vcons_t vcons, char *buf, size_t size)
{
int ret;
do
{
ret = write (vcons->input, buf, size);
if (ret > 0)
{
size -= ret;
buf += ret;
}
}
while (size && (ret != -1 || errno == EINTR));
return 0;
}
error_t
cons_vcons_input (vcons_t vcons, char *buf, size_t size)
{
pthread_mutex_lock (&vcons->lock);
_cons_vcons_console_event (vcons, CONS_EVT_KEYPRESS);
if (vcons->scrolling && _cons_jump_down_on_input)
_cons_vcons_scrollback (vcons, CONS_SCROLL_ABSOLUTE_LINE, 0);
_cons_vcons_input (vcons, buf, size);
pthread_mutex_unlock (&vcons->lock);
return 0;
}