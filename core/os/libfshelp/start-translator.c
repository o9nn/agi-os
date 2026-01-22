#include "fshelp.h"
#include <unistd.h>
#include <string.h>
#include <hurd.h>
error_t
fshelp_start_translator (fshelp_open_fn_t underlying_open_fn,
void *cookie, char *name, char *argz,
mach_msg_type_number_t argz_len,
int timeout, fsys_t *control)
{
mach_port_t ports[INIT_PORT_MAX];
mach_port_t fds[STDERR_FILENO + 1];
int ints[INIT_INT_MAX];
int i;
error_t err;
for (i = 0; i < INIT_PORT_MAX; i++)
ports[i] = MACH_PORT_NULL;
for (i = 0; i < STDERR_FILENO + 1; i++)
fds[i] = MACH_PORT_NULL;
memset (ints, 0, INIT_INT_MAX * sizeof(int));
ports[INIT_PORT_CWDIR] = getcwdir ();
ports[INIT_PORT_CRDIR] = getcrdir ();
ports[INIT_PORT_AUTH] = getauth ();
fds[STDERR_FILENO] = getdport (STDERR_FILENO);
err = fshelp_start_translator_long (underlying_open_fn, cookie,
name, argz, argz_len,
fds, MACH_MSG_TYPE_COPY_SEND,
STDERR_FILENO + 1,
ports, MACH_MSG_TYPE_COPY_SEND,
INIT_PORT_MAX,
ints, INIT_INT_MAX,
geteuid (),
timeout, control);
for (i = 0; i < INIT_PORT_MAX; i++)
mach_port_deallocate (mach_task_self (), ports[i]);
for (i = 0; i <= STDERR_FILENO; i++)
mach_port_deallocate (mach_task_self (), fds[i]);
return err;
}