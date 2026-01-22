#include <mach.h>
#include <hurd/auth.h>
#include <hurd/io.h>
#include <hurd/process.h>
error_t
exec_reauth (auth_t auth, int secure, int must_reauth,
mach_port_t *ports, unsigned num_ports,
mach_port_t *fds, unsigned num_fds)
{
unsigned int i;
error_t err = 0;
error_t reauth (mach_port_t *port, int isproc)
{
if (*port != MACH_PORT_NULL)
{
mach_port_t newport;
mach_port_t ref = mach_reply_port ();
error_t err =
(isproc ? proc_reauthenticate : io_reauthenticate)
(*port, ref, MACH_MSG_TYPE_MAKE_SEND);
if (!err)
err = auth_user_authenticate (auth, ref, MACH_MSG_TYPE_MAKE_SEND,
&newport);
mach_port_mod_refs (mach_task_self (), ref, MACH_PORT_RIGHT_RECEIVE, -1);
if (!err && newport == MACH_PORT_NULL)
err = KERN_INVALID_ARGUMENT;
if (err)
return must_reauth ? err : 0;
if (isproc)
{
err = proc_reauthenticate_complete (newport);
if (err)
{
mach_port_deallocate (mach_task_self (), newport);
return must_reauth ? err : 0;
}
}
mach_port_deallocate (mach_task_self (), *port);
*port = newport;
}
return 0;
}
for (i = 0; i < num_fds && !err; ++i)
err = reauth (&fds[i], 0);
if (!err)
{
if (secure)
ports[INIT_PORT_CRDIR] = MACH_PORT_NULL;
else
err = reauth (&ports[INIT_PORT_CRDIR], 0);
}
if (!err && !secure)
err = reauth (&ports[INIT_PORT_PROC], 1);
if (!err)
err = reauth (&ports[INIT_PORT_CWDIR], 0);
if (!err)
{
mach_port_deallocate (mach_task_self (), ports[INIT_PORT_AUTH]);
ports[INIT_PORT_AUTH] = auth;
}
return 0;
}