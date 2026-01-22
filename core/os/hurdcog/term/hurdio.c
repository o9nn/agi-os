#include <termios.h>
#include <assert-backtrace.h>
#include <errno.h>
#include <error.h>
#include <string.h>
#include <stdio.h>
#include <pthread.h>
#include <hurd.h>
#include <hurd/ports.h>
#include <hurd/io.h>
#include <hurd/tioctl.h>
#include "term.h"
thread_t reader_thread = MACH_PORT_NULL;
static file_t ioport = MACH_PORT_NULL;
#define TIOC_CAP_OUTQ 0x001
#define TIOC_CAP_START 0x002
#define TIOC_CAP_STOP 0x004
#define TIOC_CAP_FLUSH 0x008
#define TIOC_CAP_CBRK 0x010
#define TIOC_CAP_SBRK 0x020
#define TIOC_CAP_MODG 0x040
#define TIOC_CAP_MODS 0x080
#define TIOC_CAP_GETA 0x100
#define TIOC_CAP_SETA 0x200
#define TIOC_CAP_GWINSZ 0x400
unsigned int tioc_caps;
thread_t writer_thread = MACH_PORT_NULL;
static int output_stopped;
static pthread_cond_t hurdio_writer_condition;
size_t npending_output;
int assert_dtr;
static pthread_cond_t hurdio_assert_dtr_condition;
static error_t hurdio_desert_dtr (void);
static void *hurdio_reader_loop (void *arg);
static void *hurdio_writer_loop (void *arg);
static error_t hurdio_set_bits (struct termios *state);
static error_t
hurdio_init (void)
{
pthread_t thread;
error_t err;
pthread_cond_init (&hurdio_writer_condition, NULL);
pthread_cond_init (&hurdio_assert_dtr_condition, NULL);
err = pthread_create (&thread, NULL, hurdio_reader_loop, NULL);
if (!err)
pthread_detach (thread);
else
{
errno = err;
perror ("pthread_create");
}
err = pthread_create (&thread, NULL, hurdio_writer_loop, NULL);
if (!err)
pthread_detach (thread);
else
{
errno = err;
perror ("pthread_create");
}
return 0;
}
static error_t
hurdio_fini (void)
{
hurdio_desert_dtr ();
writer_thread = MACH_PORT_NULL;
return 0;
}
static error_t
hurdio_gwinsz (struct winsize *size)
{
if (tioc_caps & TIOC_CAP_GWINSZ)
{
error_t err = tioctl_tiocgwinsz (ioport, size);
if (err && (err == EMIG_BAD_ID || err == EOPNOTSUPP))
{
tioc_caps &= ~TIOC_CAP_GWINSZ;
err = EOPNOTSUPP;
}
return err;
}
return EOPNOTSUPP;
}
static void
wait_for_dtr (void)
{
while (!assert_dtr)
pthread_hurd_cond_wait_np (&hurdio_assert_dtr_condition, &global_lock);
assert_dtr = 0;
if (tty_arg == 0)
ioport = termctl->underlying;
else
{
ioport = file_name_lookup (tty_arg, O_READ|O_WRITE, 0);
if (ioport == MACH_PORT_NULL)
{
report_carrier_error (errno);
return;
}
}
error_t err;
struct termios state = termstate;
tioc_caps = ~0;
err = hurdio_set_bits (&state);
if (err)
report_carrier_error (err);
else
{
termstate = state;
report_carrier_on ();
pthread_cond_broadcast (&hurdio_writer_condition);
}
}
static void *
hurdio_reader_loop (void *arg)
{
#define BUFFER_SIZE 256
char buffer[BUFFER_SIZE];
char *data;
mach_msg_type_number_t datalen;
error_t err;
pthread_setname_np (pthread_self (), "reader");
pthread_mutex_lock (&global_lock);
reader_thread = mach_thread_self ();
while (1)
{
while (ioport == MACH_PORT_NULL)
wait_for_dtr ();
pthread_mutex_unlock (&global_lock);
data = buffer;
datalen = BUFFER_SIZE;
err = io_read (ioport, &data, &datalen, -1, BUFFER_SIZE);
pthread_mutex_lock (&global_lock);
if (err || !datalen)
hurdio_desert_dtr ();
else
{
if (termstate.c_cflag & CREAD)
{
int i;
for (i = 0; i < datalen; i++)
if (input_character (data[i]))
break;
}
if (data != buffer)
vm_deallocate (mach_task_self(), (vm_address_t) data, datalen);
}
}
#undef BUFFER_SIZE
return 0;
}
static void *
hurdio_writer_loop (void *arg)
{
#define BUFFER_SIZE 256
char *bufp;
char pending_output[BUFFER_SIZE];
size_t amount;
error_t err;
int size;
int npending_output_copy;
mach_port_t ioport_copy;
pthread_setname_np (pthread_self (), "writer");
pthread_mutex_lock (&global_lock);
writer_thread = mach_thread_self ();
while (1)
{
while (writer_thread != MACH_PORT_NULL
&& (ioport == MACH_PORT_NULL || !qsize (outputq)
|| output_stopped))
pthread_hurd_cond_wait_np (&hurdio_writer_condition, &global_lock);
if (writer_thread == MACH_PORT_NULL)
return 0;
size = qsize (outputq);
if (size + npending_output > BUFFER_SIZE)
size = BUFFER_SIZE - npending_output;
bufp = pending_output + npending_output;
npending_output += size;
npending_output_copy = npending_output;
ioport_copy = ioport;
mach_port_mod_refs (mach_task_self (), ioport_copy,
MACH_PORT_RIGHT_SEND, 1);
while (size--)
*bufp++ = dequeue (outputq);
pthread_mutex_unlock (&global_lock);
err = io_write (ioport_copy, pending_output, npending_output_copy,
-1, &amount);
pthread_mutex_lock (&global_lock);
mach_port_mod_refs (mach_task_self (), ioport_copy,
MACH_PORT_RIGHT_SEND, -1);
if (err)
hurdio_desert_dtr ();
else
{
if (amount >= npending_output)
{
npending_output = 0;
pthread_cond_broadcast (outputq->wait);
pthread_cond_broadcast (&select_alert);
}
else
{
npending_output -= amount;
memmove (pending_output, pending_output + amount,
npending_output);
}
}
}
#undef BUFFER_SIZE
return 0;
}
static error_t
hurdio_start_output (void)
{
if (output_stopped && !(termflags & USER_OUTPUT_SUSP))
{
if (tioc_caps & TIOC_CAP_START)
{
error_t err = tioctl_tiocstart (ioport);
if (err && (err == EMIG_BAD_ID || err == EOPNOTSUPP))
tioc_caps &= ~TIOC_CAP_START;
}
output_stopped = 0;
}
pthread_cond_broadcast (&hurdio_writer_condition);
return 0;
}
static error_t
hurdio_set_break (void)
{
if (tioc_caps & TIOC_CAP_SBRK)
{
error_t err = tioctl_tiocsbrk (ioport);
if (err && (err == EMIG_BAD_ID || err == EOPNOTSUPP))
tioc_caps &= ~TIOC_CAP_SBRK;
else if (err)
return err;
}
return 0;
}
static error_t
hurdio_clear_break (void)
{
if (tioc_caps & TIOC_CAP_CBRK)
{
error_t err = tioctl_tioccbrk (ioport);
if (err && (err == EMIG_BAD_ID || err == EOPNOTSUPP))
tioc_caps &= ~TIOC_CAP_CBRK;
else if (err)
return err;
}
return 0;
}
static error_t
hurdio_abandon_physical_output (void)
{
if (tioc_caps & TIOC_CAP_FLUSH)
{
error_t err = tioctl_tiocflush (ioport, O_WRITE);
if (err && (err == EMIG_BAD_ID || err == EOPNOTSUPP))
tioc_caps &= ~TIOC_CAP_FLUSH;
else if (err)
return err;
}
npending_output = 0;
return 0;
}
static error_t
hurdio_suspend_physical_output (void)
{
if (!output_stopped)
{
if (tioc_caps & TIOC_CAP_STOP)
{
error_t err = tioctl_tiocstop (ioport);
if (err && (err == EMIG_BAD_ID || err == EOPNOTSUPP))
tioc_caps &= ~TIOC_CAP_STOP;
else if (err)
return err;
}
output_stopped = 1;
}
return 0;
}
static error_t
hurdio_notice_input_flushed (void)
{
if (tioc_caps & TIOC_CAP_FLUSH)
{
error_t err = tioctl_tiocflush (ioport, O_READ);
if (err && (err == EMIG_BAD_ID || err == EOPNOTSUPP))
tioc_caps &= ~TIOC_CAP_FLUSH;
else if (err)
return err;
}
return 0;
}
static int
hurdio_pending_output_size (void)
{
int queue_size = 0;
if (tioc_caps & TIOC_CAP_OUTQ)
{
error_t err = tioctl_tiocoutq (ioport, &queue_size);
if (err && (err == EMIG_BAD_ID || err == EOPNOTSUPP))
tioc_caps &= ~TIOC_CAP_OUTQ;
else if (err)
queue_size = 0;
}
return queue_size + npending_output;
}
static error_t
hurdio_desert_dtr (void)
{
if (writer_thread != MACH_PORT_NULL)
hurd_thread_cancel (writer_thread);
if (reader_thread != MACH_PORT_NULL)
hurd_thread_cancel (reader_thread);
if (ioport != MACH_PORT_NULL && tty_arg)
{
mach_port_deallocate (mach_task_self (), ioport);
ioport = MACH_PORT_NULL;
}
assert_dtr = 0;
report_carrier_off ();
return 0;
}
static error_t
hurdio_assert_dtr (void)
{
if (ioport == MACH_PORT_NULL)
{
assert_dtr = 1;
pthread_cond_signal (&hurdio_assert_dtr_condition);
}
return 0;
}
static error_t
hurdio_set_bits (struct termios *state)
{
error_t err;
struct termios ttystat;
struct hurd_termios
{
modes_t modes;
ccs_t ccs;
speeds_t speeds;
} *hurd_ttystat = (struct hurd_termios *) &ttystat;
if (!(state->c_cflag & CIGNORE) && ioport != MACH_PORT_NULL)
{
if (!(tioc_caps & TIOC_CAP_GETA))
return 0;
err = tioctl_tiocgeta (ioport, hurd_ttystat->modes,
hurd_ttystat->ccs, hurd_ttystat->speeds);
if (err && (err == EMIG_BAD_ID || err == EOPNOTSUPP))
{
tioc_caps &= ~TIOC_CAP_GETA;
return 0;
}
else if (err)
return err;
if (tioc_caps & TIOC_CAP_SETA)
{
if (state->__ispeed)
hurd_ttystat->speeds[0] = state->__ispeed;
if (state->__ospeed)
hurd_ttystat->speeds[1] = state->__ospeed;
cfmakeraw (&ttystat);
ttystat.c_cflag = state->c_cflag &~ HUPCL;
err = tioctl_tiocseta (ioport, hurd_ttystat->modes,
hurd_ttystat->ccs, hurd_ttystat->speeds);
if (err && (err == EMIG_BAD_ID || err == EOPNOTSUPP))
tioc_caps &= ~TIOC_CAP_SETA;
else if (err)
return err;
err = tioctl_tiocgeta (ioport, hurd_ttystat->modes,
hurd_ttystat->ccs, hurd_ttystat->speeds);
if (err && (err == EMIG_BAD_ID || err == EOPNOTSUPP))
tioc_caps &= ~TIOC_CAP_GETA;
else if (err)
return err;
}
*state = ttystat;
}
return 0;
}
static error_t
hurdio_mdmctl (int how, int bits)
{
error_t err;
int oldbits, newbits;
if (tioc_caps & TIOC_CAP_MODS)
{
if ((how == MDMCTL_BIS) || (how == MDMCTL_BIC))
{
if (tioc_caps & TIOC_CAP_MODG)
{
error_t err = tioctl_tiocmodg (ioport, &oldbits);
if (err && (err == EMIG_BAD_ID || err == EOPNOTSUPP))
{
tioc_caps &= ~TIOC_CAP_MODG;
return EOPNOTSUPP;
}
else if (err)
return err;
}
else
return EOPNOTSUPP;
}
if (how == MDMCTL_BIS)
newbits = (oldbits | bits);
else if (how == MDMCTL_BIC)
newbits = (oldbits &= ~bits);
else
newbits = bits;
err = tioctl_tiocmods (ioport, newbits);
if (err && (err == EMIG_BAD_ID || err == EOPNOTSUPP))
tioc_caps &= ~TIOC_CAP_MODS;
else if (err)
return err;
}
return 0;
}
static int
hurdio_mdmstate (int *state)
{
int oldbits;
if (tioc_caps & TIOC_CAP_MODG)
{
error_t err = tioctl_tiocmodg (ioport, &oldbits);
if (err && (err == EMIG_BAD_ID || err == EOPNOTSUPP))
tioc_caps &= ~TIOC_CAP_MODG;
else if (err)
return 0;
}
return 0;
}
const struct bottomhalf hurdio_bottom =
{
TERM_ON_HURDIO,
hurdio_init,
hurdio_fini,
hurdio_gwinsz,
hurdio_start_output,
hurdio_set_break,
hurdio_clear_break,
hurdio_abandon_physical_output,
hurdio_suspend_physical_output,
hurdio_pending_output_size,
hurdio_notice_input_flushed,
hurdio_assert_dtr,
hurdio_desert_dtr,
hurdio_set_bits,
hurdio_mdmctl,
hurdio_mdmstate,
};