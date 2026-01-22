#include <hurd/netfs.h>
#include <stdlib.h>
#include <error.h>
#include <string.h>
#include <fcntl.h>
#include <pthread.h>
#include <sys/mman.h>
#include "kdioctl_S.h"
#include "mach-inputdev.h"
#include "input.h"
#define KBDEVTBUFSZ	20
#define KBDBUFSZ	(KBDEVTBUFSZ * sizeof (kd_event))
#define KBDBUF_POS(x)	((x) % KBDBUFSZ)
static struct kbdbuf
{
char keybuffer[KBDBUFSZ];
int pos;
size_t size;
pthread_cond_t readcond;
pthread_cond_t writecond;
} kbdbuf;
static pthread_cond_t select_alert;
static pthread_mutex_t global_lock;
int kbd_repeater_opened;
void
kbd_repeat_key (kd_event *key)
{
kd_event *ev;
pthread_mutex_lock (&global_lock);
while (kbdbuf.size + sizeof (kd_event) > KBDBUFSZ)
{
(void) pthread_hurd_cond_wait_np (&kbdbuf.writecond, &global_lock);
}
ev = (kd_event *) &kbdbuf.keybuffer[KBDBUF_POS (kbdbuf.pos
+ kbdbuf.size)];
kbdbuf.size += sizeof (kd_event);
memcpy (ev, key, sizeof (kd_event));
pthread_cond_broadcast (&kbdbuf.readcond);
pthread_cond_broadcast (&select_alert);
pthread_mutex_unlock (&global_lock);
}
static error_t
repeater_select (struct protid *cred, mach_port_t reply,
mach_msg_type_name_t replytype,
struct timespec *tsp, int *type)
{
error_t err;
if (!cred)
return EOPNOTSUPP;
*type &= ~SELECT_URG;
if (*type & ~SELECT_READ)
return 0;
if (*type == 0)
return 0;
pthread_mutex_lock (&global_lock);
while (1)
{
if (kbdbuf.size > 0)
{
*type = SELECT_READ;
pthread_mutex_unlock (&global_lock);
return 0;
}
ports_interrupt_self_on_port_death (cred, reply);
err = pthread_hurd_cond_timedwait_np (&select_alert, &global_lock, tsp);
if (err)
{
*type = 0;
pthread_mutex_unlock (&global_lock);
if (err == ETIMEDOUT)
err = 0;
return err;
}
}
}
static error_t
repeater_read (struct protid *cred, char **data,
mach_msg_type_number_t *datalen, off_t offset,
mach_msg_type_number_t amount)
{
if (! cred)
return EOPNOTSUPP;
else if (! (cred->po->openstat & O_READ))
return EBADF;
pthread_mutex_lock (&global_lock);
while (amount > kbdbuf.size)
{
if (cred->po->openstat & O_NONBLOCK && amount > kbdbuf.size)
{
pthread_mutex_unlock (&global_lock);
return EWOULDBLOCK;
}
if (pthread_hurd_cond_wait_np (&kbdbuf.readcond, &global_lock))
{
pthread_mutex_unlock (&global_lock);
return EINTR;
}
}
if (amount > 0)
{
char *keydata;
unsigned int i = 0;
if (*datalen < amount)
{
*data = mmap (0, amount, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (*data == MAP_FAILED)
{
pthread_mutex_unlock (&global_lock);
return ENOMEM;
}
}
keydata = *data;
while (i != amount)
{
keydata[i++] = kbdbuf.keybuffer[kbdbuf.pos++];
kbdbuf.pos = KBDBUF_POS (kbdbuf.pos);
}
kbdbuf.size -= amount;
pthread_cond_broadcast (&kbdbuf.writecond);
}
*datalen = amount;
pthread_mutex_unlock (&global_lock);
return 0;
}
static void
repeater_open (void)
{
if (! kbd_repeater_opened)
console_switch_away ();
kbd_repeater_opened++;
}
static void
repeater_close (void)
{
kbd_repeater_opened--;
if (! kbd_repeater_opened)
{
console_switch_back ();
kbdbuf.pos = 0;
kbdbuf.size = 0;
}
}
error_t
kbd_setrepeater (const char *nodename, consnode_t *cn)
{
extern int kdioctl_server (mach_msg_header_t *inp, mach_msg_header_t *outp);
error_t err;
err = console_create_consnode (nodename, cn);
if (err)
return err;
(*cn)->read = repeater_read;
(*cn)->write = 0;
(*cn)->select = repeater_select;
(*cn)->open = repeater_open;
(*cn)->close = repeater_close;
(*cn)->demuxer = kdioctl_server;
pthread_mutex_init (&global_lock, NULL);
pthread_cond_init (&kbdbuf.readcond, NULL);
pthread_cond_init (&kbdbuf.writecond, NULL);
pthread_cond_init (&select_alert, NULL);
console_register_consnode (*cn);
return 0;
}
kern_return_t
S_kdioctl_kdskbdmode (io_t port, int mode)
{
return 0;
}
kern_return_t
S_kdioctl_kdgkbdmode (io_t port, int *mode)
{
return 0;
}