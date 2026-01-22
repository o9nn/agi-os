#include <hurd.h>
error_t
setnullauth (void)
{
error_t err;
auth_t ourauth = getauth ();
auth_t nullauth;
err = auth_makeauth (ourauth,
NULL, MACH_MSG_TYPE_COPY_SEND, 0,
NULL, 0,
NULL, 0,
NULL, 0,
NULL, 0,
&nullauth);
mach_port_deallocate (mach_task_self (), ourauth);
if (err)
return err;
err = setauth (nullauth);
return err;
}