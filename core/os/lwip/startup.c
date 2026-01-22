#include <startup.h>
#include <unistd.h>
#include <signal.h>
#include <hurd/paths.h>
#include <hurd/startup.h>
#include <lwip-hurd.h>
static void
sigterm_handler (int signo)
{
ports_class_iterate (socketport_class, ports_destroy_right);
ports_class_iterate (addrport_class, ports_destroy_right);
sleep (10);
signal (SIGTERM, SIG_DFL);
raise (SIGTERM);
}
void
arrange_shutdown_notification (void)
{
error_t err;
mach_port_t initport, notify;
struct port_info *pi;
shutdown_notify_class = ports_create_class (0, 0);
signal (SIGTERM, sigterm_handler);
err = ports_create_port (shutdown_notify_class, lwip_bucket,
sizeof (struct port_info), &pi);
if (err)
return;
initport = file_name_lookup (_SERVERS_STARTUP, 0, 0);
if (initport == MACH_PORT_NULL)
{
ports_port_deref (pi);
return;
}
notify = ports_get_send_right (pi);
ports_port_deref (pi);
startup_request_notification (initport, notify,
MACH_MSG_TYPE_MAKE_SEND,
program_invocation_short_name);
mach_port_deallocate (mach_task_self (), notify);
mach_port_deallocate (mach_task_self (), initport);
}