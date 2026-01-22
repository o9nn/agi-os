#include "pfinet.h"
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <error.h>
#include <argp.h>
#include <hurd/paths.h>
#include <hurd/startup.h>
#include <string.h>
#include <fcntl.h>
#include <version.h>
#define _HACK_ERRNO_H
#include <errno.h>
#include <linux/netdevice.h>
#include <linux/inet.h>
static void pfinet_activate_ipv6 (void);
extern error_t configure_device (struct device *dev,
uint32_t addr, uint32_t netmask,
uint32_t peer, uint32_t broadcast);
extern int addrconf_notify(struct notifier_block *this, unsigned long event,
void * data);
int trivfs_fstype = FSTYPE_MISC;
int trivfs_fsid;
int trivfs_support_read = 1;
int trivfs_support_write = 1;
int trivfs_support_exec = 0;
int trivfs_allow_open = O_READ | O_WRITE;
struct port_class *pfinet_protid_portclasses[2];
struct port_class *pfinet_cntl_portclasses[2];
int pfinet_bootstrap_portclass = PORTCLASS_INET;
struct trivfs_control *pfinetctl;
uid_t pfinet_owner;
uid_t pfinet_group;
struct port_class *shutdown_notify_class;
struct port_bucket *pfinet_bucket;
struct port_class *addrport_class;
struct port_class *socketport_class;
mach_port_t fsys_identity;
const char *argp_program_version = STANDARD_HURD_VERSION (pfinet);
extern struct argp pfinet_argp;
#include "io_S.h"
#include "socket_S.h"
#include "pfinet_S.h"
#include "iioctl_S.h"
#include "rioctl_S.h"
#include "startup_notify_S.h"
int
pfinet_demuxer (mach_msg_header_t *inp,
mach_msg_header_t *outp)
{
struct port_info *pi;
if (MACH_MSGH_BITS_LOCAL (inp->msgh_bits) ==
MACH_MSG_TYPE_PROTECTED_PAYLOAD)
pi = ports_lookup_payload (pfinet_bucket,
inp->msgh_protected_payload,
socketport_class);
else
pi = ports_lookup_port (pfinet_bucket,
inp->msgh_local_port,
socketport_class);
if (pi)
{
ports_port_deref (pi);
mig_routine_t routine;
if ((routine = io_server_routine (inp)) ||
(routine = socket_server_routine (inp)) ||
(routine = pfinet_server_routine (inp)) ||
(routine = rioctl_server_routine (inp)) ||
(routine = iioctl_server_routine (inp)) ||
(routine = NULL, trivfs_demuxer (inp, outp)) ||
(routine = startup_notify_server_routine (inp)))
{
if (routine)
(*routine) (inp, outp);
return TRUE;
}
else
return FALSE;
}
else
{
mig_routine_t routine;
if ((routine = socket_server_routine (inp)) ||
(routine = pfinet_server_routine (inp)) ||
(routine = rioctl_server_routine (inp)) ||
(routine = iioctl_server_routine (inp)) ||
(routine = NULL, trivfs_demuxer (inp, outp)) ||
(routine = startup_notify_server_routine (inp)))
{
if (routine)
(*routine) (inp, outp);
return TRUE;
}
else
return FALSE;
}
}
kern_return_t
S_startup_dosync (mach_port_t handle)
{
struct port_info *inpi = ports_lookup_port (pfinet_bucket, handle,
shutdown_notify_class);
if (!inpi)
return EOPNOTSUPP;
ports_class_iterate (socketport_class, ports_destroy_right);
return 0;
}
void
sigterm_handler (int signo)
{
ports_class_iterate (socketport_class, ports_destroy_right);
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
err = ports_create_port (shutdown_notify_class, pfinet_bucket,
sizeof (struct port_info), &pi);
if (err)
return;
initport = file_name_lookup (_SERVERS_STARTUP, 0, 0);
if (initport == MACH_PORT_NULL)
return;
notify = ports_get_send_right (pi);
ports_port_deref (pi);
startup_request_notification (initport, notify,
MACH_MSG_TYPE_MAKE_SEND,
program_invocation_short_name);
mach_port_deallocate (mach_task_self (), notify);
mach_port_deallocate (mach_task_self (), initport);
}
error_t
find_device (char *name, struct device **device)
{
struct device *dev = dev_base;
char *base_name;
assert_backtrace (dev);
dev = dev->next;
if (!name)
{
if (dev)
{
if (dev->next)
return EBUSY;
else
{
*device = dev;
return 0;
}
}
else
return ENXIO;
}
for (; dev; dev = dev->next)
if (strcmp (dev->name, name) == 0)
{
*device = dev;
return 0;
}
base_name = strrchr(name, '/');
if (base_name)
base_name++;
else
base_name = name;
if (strncmp(base_name, "tun", 3) == 0)
setup_tunnel_device (name, device);
else if (strncmp(base_name, "dummy", 5) == 0)
setup_dummy_device (name, device);
else
setup_ethernet_device (name, device);
dev_open (*device);
return 0;
}
error_t
enumerate_devices (error_t (*fun) (struct device *dev))
{
error_t err;
struct device *dev = dev_base;
assert_backtrace (dev);
dev = dev->next;
for (; dev; dev = dev->next)
{
err = (*fun) (dev);
if (err)
return err;
}
return 0;
}
extern void sk_init (void), skb_init (void);
extern int net_dev_init (void);
extern void inet6_proto_init (struct net_proto *pro);
#define ARRAY_SIZE(x)       (sizeof(x) / sizeof((x)[0]))
int
main (int argc,
char **argv)
{
error_t err;
mach_port_t bootstrap;
struct stat st;
pthread_t thread;
pfinet_bucket = ports_create_bucket ();
addrport_class = ports_create_class (clean_addrport, 0);
socketport_class = ports_create_class (clean_socketport, 0);
mach_port_allocate (mach_task_self (), MACH_PORT_RIGHT_RECEIVE,
&fsys_identity);
init_time ();
ethernet_initialize ();
err = pthread_create (&thread, NULL, net_bh_worker, NULL);
if (!err)
pthread_detach (thread);
else
{
errno = err;
perror ("pthread_create");
}
pthread_mutex_lock (&global_lock);
prepare_current (1);
sk_init ();
#ifdef SLAB_SKB
skb_init ();
#endif
inet_proto_init (0);
net_dev_init ();
configure_device (&loopback_dev,
htonl (INADDR_LOOPBACK), htonl (IN_CLASSA_NET),
htonl (INADDR_NONE), htonl (INADDR_NONE));
pthread_mutex_unlock (&global_lock);
argp_parse (&pfinet_argp, argc, argv, 0,0,0);
task_get_bootstrap_port (mach_task_self (), &bootstrap);
pfinet_owner = pfinet_group = 0;
if (bootstrap != MACH_PORT_NULL) {
if(pfinet_protid_portclasses[pfinet_bootstrap_portclass] != NULL)
error(1, 0, "No portclass left to assign to bootstrap port");
#ifdef CONFIG_IPV6
if (pfinet_bootstrap_portclass == PORTCLASS_INET6)
pfinet_activate_ipv6 ();
#endif
err = trivfs_add_protid_port_class (
&pfinet_protid_portclasses[pfinet_bootstrap_portclass]);
if (err)
error (1, 0, "error creating control port class");
err = trivfs_add_control_port_class (
&pfinet_cntl_portclasses[pfinet_bootstrap_portclass]);
if (err)
error (1, 0, "error creating control port class");
err = trivfs_startup (bootstrap, 0,
pfinet_cntl_portclasses[pfinet_bootstrap_portclass],
pfinet_bucket,
pfinet_protid_portclasses[pfinet_bootstrap_portclass],
pfinet_bucket,
&pfinetctl);
if (err)
error (1, err, "contacting parent");
err = io_stat (pfinetctl->underlying, &st);
if (! err)
{
pfinet_owner = st.st_uid;
pfinet_group = st.st_gid;
}
}
else {
int i;
for (i = 0; i < ARRAY_SIZE (pfinet_protid_portclasses); i++)
if (pfinet_protid_portclasses[i] != NULL)
break;
if (i == ARRAY_SIZE (pfinet_protid_portclasses))
error (1, 0, "should be started as a translator.\n");
}
arrange_shutdown_notification ();
ports_manage_port_operations_multithread (pfinet_bucket,
pfinet_demuxer,
30 * 1000, 2 * 60 * 1000, 0);
return 0;
}
#ifdef CONFIG_IPV6
static void
pfinet_activate_ipv6 (void)
{
inet6_proto_init (0);
struct device *dev = dev_base;
if (dev)
do
{
if (!(dev->flags & IFF_UP))
continue;
addrconf_notify (NULL, NETDEV_REGISTER, dev);
addrconf_notify (NULL, NETDEV_UP, dev);
}
while ((dev = dev->next));
}
#endif
void
pfinet_bind (int portclass, const char *name)
{
struct trivfs_control *cntl;
error_t err = 0;
mach_port_t right;
file_t file = file_name_lookup (name, O_CREAT|O_NOTRANS, 0666);
if (file == MACH_PORT_NULL)
err = errno;
if (! err) {
if (pfinet_protid_portclasses[portclass] != NULL)
error (1, 0, "Cannot bind one protocol to multiple nodes.\n");
#ifdef CONFIG_IPV6
if (portclass == PORTCLASS_INET6)
pfinet_activate_ipv6 ();
#endif
err = trivfs_add_protid_port_class (&pfinet_protid_portclasses[portclass]);
if (err)
error (1, 0, "error creating control port class");
err = trivfs_add_control_port_class (&pfinet_cntl_portclasses[portclass]);
if (err)
error (1, 0, "error creating control port class");
err = trivfs_create_control (file, pfinet_cntl_portclasses[portclass],
pfinet_bucket,
pfinet_protid_portclasses[portclass],
pfinet_bucket, &cntl);
}
if (! err)
{
right = ports_get_send_right (cntl);
err = file_set_translator (file, 0, FS_TRANS_EXCL | FS_TRANS_SET,
0, 0, 0, right, MACH_MSG_TYPE_COPY_SEND);
mach_port_deallocate (mach_task_self (), right);
}
if (err)
error (1, err, "%s", name);
ports_port_deref (cntl);
}
void
trivfs_modify_stat (struct trivfs_protid *cred,
struct stat *st)
{
}
error_t
trivfs_goaway (struct trivfs_control *cntl, int flags)
{
if (flags & FSYS_GOAWAY_FORCE)
exit (0);
else
{
ports_inhibit_class_rpcs (pfinet_cntl_portclasses[0]);
ports_inhibit_class_rpcs (pfinet_protid_portclasses[0]);
ports_inhibit_class_rpcs (socketport_class);
if (ports_count_class (socketport_class) != 0)
{
ports_enable_class (socketport_class);
ports_resume_class_rpcs (pfinet_cntl_portclasses[0]);
ports_resume_class_rpcs (pfinet_protid_portclasses[0]);
return EBUSY;
}
exit (0);
}
}