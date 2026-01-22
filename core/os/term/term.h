#ifndef __HURD_TERM_H__
#define __HURD_TERM_H__
#include <pthread.h>
#include <assert-backtrace.h>
#include <errno.h>
#include <hurd/trivfs.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <features.h>
#include <hurd/hurd_types.h>
extern int nperopens;
#ifdef TERM_DEFINE_EI
#define TERM_EI
#else
#define TERM_EI __extern_inline
#endif
#undef MDMBUF
#undef ECHO
#undef TOSTOP
#undef FLUSHO
#undef PENDIN
#undef NOFLSH
#include <unistd.h>
#include <termios.h>
#define CHAR_EOT '\004'
#define CHAR_DC1 '\021'
#define CHAR_DC2 '\022'
#define CHAR_DC3 '\023'
#define CHAR_USER_QUOTE '\377'
#define CTRL_BIT 0x40
#ifdef	IUCLC
#define ILCASE	IUCLC
#else
#define ILCASE (1 << 14)
#endif
#ifdef	OLCUC
#define OLCASE	OLCUC
#else
#define OLCASE (1 << 9)
#endif
#define MDMCTL_BIS 0
#define MDMCTL_BIC 1
#define MDMCTL_SET 2
extern struct termios termstate;
extern long termflags;
#define USER_OUTPUT_SUSP  0x00000001
#define TTY_OPEN	  0x00000002
#define LAST_SLASH	  0x00000004
#define LAST_LNEXT        0x00000008
#define INSIDE_HDERASE    0x00000010
#define SENT_VSTOP        0x00000020
#define FLUSH_OUTPUT      0x00000040
#define NO_CARRIER        0x00000080
#define EXCL_USE          0x00000100
#define NO_OWNER          0x00000200
#define ICKY_ASYNC	  0x00000400
#define QUEUE_LOWAT 200
#define QUEUE_HIWAT 8100
extern pthread_mutex_t global_lock;
extern pthread_cond_t carrier_alert;
extern pthread_cond_t select_alert;
extern pthread_cond_t *pty_select_alert;
extern struct port_bucket *term_bucket;
extern struct port_class *tty_cntl_class;
extern struct port_class *tty_class;
extern struct port_class *cttyid_class;
extern struct port_class *pty_class;
extern struct port_class *pty_cntl_class;
extern struct trivfs_control *termctl;
extern struct trivfs_control *ptyctl;
extern struct queue *inputq, *rawq, *outputq;
extern int remote_input_mode;
extern int external_processing;
extern uid_t term_owner;
extern uid_t term_group;
extern mode_t term_mode;
struct winsize;
struct bottomhalf
{
enum term_bottom_type type;
error_t (*init) (void);
error_t (*fini) (void);
error_t (*gwinsz) (struct winsize *size);
error_t (*start_output) (void);
error_t (*set_break) (void);
error_t (*clear_break) (void);
error_t (*abandon_physical_output) (void);
error_t (*suspend_physical_output) (void);
int (*pending_output_size) (void);
error_t (*notice_input_flushed) (void);
error_t (*assert_dtr) (void);
error_t (*desert_dtr) (void);
error_t (*set_bits) (struct termios *state);
error_t (*mdmctl) (int how, int bits);
error_t (*mdmstate) (int *state);
};
extern const struct bottomhalf *bottom;
extern const struct bottomhalf devio_bottom, hurdio_bottom, ptyio_bottom;
#define QUEUE_QUOTE_MARK 0xf000
typedef short quoted_char;
struct queue
{
int susp;
int lowat;
int hiwat;
short *cs, *ce;
int arraylen;
pthread_cond_t *wait;
quoted_char array[0];
};
struct queue *create_queue (int size, int lowat, int hiwat);
extern int qsize (struct queue *q);
extern int qavail (struct queue *q);
extern void clear_queue (struct queue *q);
extern quoted_char dequeue_quote (struct queue *q);
extern char dequeue (struct queue *q);
extern void enqueue_internal (struct queue **qp, quoted_char c);
extern void enqueue (struct queue **qp, char c);
extern void enqueue_quote (struct queue **qp, char c);
extern char unquote_char (quoted_char c);
extern int char_quoted_p (quoted_char c);
extern short queue_erase (struct queue *q);
#if defined(__USE_EXTERN_INLINES) || defined(TERM_DEFINE_EI)
TERM_EI int
qsize (struct queue *q)
{
return q->ce - q->cs;
}
TERM_EI int
qavail (struct queue *q)
{
return !q->susp;
}
TERM_EI void
clear_queue (struct queue *q)
{
q->susp = 0;
q->cs = q->ce = q->array;
pthread_cond_broadcast (q->wait);
pthread_cond_broadcast (&select_alert);
if (q == inputq && pty_select_alert != NULL)
pthread_cond_broadcast (pty_select_alert);
}
#endif
void call_asyncs (int dir);
#if defined(__USE_EXTERN_INLINES) || defined(TERM_DEFINE_EI)
TERM_EI quoted_char
dequeue_quote (struct queue *q)
{
int beep = 0;
assert_backtrace (qsize (q));
if (q->susp && (qsize (q) < q->lowat))
{
q->susp = 0;
beep = 1;
}
if (qsize (q) == 1)
beep = 1;
if (beep)
{
pthread_cond_broadcast (q->wait);
pthread_cond_broadcast (&select_alert);
if (q == inputq && pty_select_alert != NULL)
pthread_cond_broadcast (pty_select_alert);
else if (q == outputq)
call_asyncs (O_WRITE);
}
return *q->cs++;
}
TERM_EI char
dequeue (struct queue *q)
{
return dequeue_quote (q) & ~QUEUE_QUOTE_MARK;
}
#endif
struct queue *reallocate_queue (struct queue *);
#if defined(__USE_EXTERN_INLINES) || defined(TERM_DEFINE_EI)
TERM_EI void
enqueue_internal (struct queue **qp, quoted_char c)
{
struct queue *q = *qp;
if (q->ce - q->array == q->arraylen)
q = *qp = reallocate_queue (q);
*q->ce++ = c;
if (qsize (q) == 1)
{
pthread_cond_broadcast (q->wait);
pthread_cond_broadcast (&select_alert);
if (q == inputq)
{
if (pty_select_alert != NULL)
pthread_cond_broadcast (pty_select_alert);
call_asyncs (O_READ);
}
}
if (!q->susp && (qsize (q) > q->hiwat))
q->susp = 1;
}
TERM_EI void
enqueue (struct queue **qp, char c)
{
enqueue_internal (qp, c);
}
TERM_EI void
enqueue_quote (struct queue **qp, char c)
{
enqueue_internal (qp, c | QUEUE_QUOTE_MARK);
}
TERM_EI char
unquote_char (quoted_char c)
{
return c & ~QUEUE_QUOTE_MARK;
}
TERM_EI int
char_quoted_p (quoted_char c)
{
return c & QUEUE_QUOTE_MARK;
}
TERM_EI short
queue_erase (struct queue *q)
{
short answer;
int beep = 0;
assert_backtrace (qsize (q));
answer = *--q->ce;
if (q->susp && (qsize (q) < q->lowat))
{
q->susp = 0;
beep = 1;
}
if (qsize (q) == 0)
beep = 1;
if (beep)
{
pthread_cond_broadcast (q->wait);
pthread_cond_broadcast (&select_alert);
if (q == inputq && pty_select_alert != NULL)
pthread_cond_broadcast (pty_select_alert);
}
return answer;
}
#endif
int input_character (int);
void report_carrier_on (void);
void report_carrier_off (void);
void report_carrier_error (error_t);
error_t drop_output (void);
void send_signal (int);
error_t drain_output (void);
void output_character (int);
void copy_rawq (void);
void rescan_inputq (void);
void write_character (int);
void init_users (void);
extern char *tty_arg;
extern dev_t rdev;
error_t pty_io_write (struct trivfs_protid *, const char *,
mach_msg_type_number_t, vm_size_t *);
error_t pty_io_read (struct trivfs_protid *, char **,
mach_msg_type_number_t *, vm_size_t);
error_t pty_io_readable (size_t *);
error_t pty_io_select (struct trivfs_protid *, mach_port_t,
struct timespec *, int *);
error_t pty_open_hook (struct trivfs_control *, struct iouser *, int);
error_t pty_po_create_hook (struct trivfs_peropen *);
error_t pty_po_destroy_hook (struct trivfs_peropen *);
#endif