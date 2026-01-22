#ifndef __PIPE_H__
#define __PIPE_H__
#define EWOULDBLOCK EAGAIN
#include <pthread.h>
#include <features.h>
#ifdef PIPE_DEFINE_EI
#define PIPE_EI
#else
#define PIPE_EI __extern_inline
#endif
#include "pq.h"
struct pipe_class
{
int sock_type;
unsigned flags;
error_t (*read)(struct packet *packet, int *dequeue, unsigned *flags,
char **data, size_t *data_len, size_t amount);
error_t (*write)(struct pq *pq, void *source,
const char *data, size_t data_len, size_t *amount);
};
#define PIPE_CLASS_CONNECTIONLESS	0x1
extern struct pipe_class *stream_pipe_class;
extern struct pipe_class *dgram_pipe_class;
extern struct pipe_class *seqpack_pipe_class;
struct pipe_select_cond
{
struct pipe_select_cond *next;
struct pipe_select_cond *prev;
pthread_cond_t cond;
};
struct pipe
{
struct pipe_class *class;
unsigned readers, writers;
unsigned flags;
time_value_t read_time;
time_value_t write_time;
pthread_cond_t pending_reads;
pthread_cond_t pending_read_selects;
pthread_cond_t pending_writes;
pthread_cond_t pending_write_selects;
struct pipe_select_cond *pending_selects;
size_t write_limit;
size_t write_atomic;
pthread_mutex_t lock;
struct pq *queue;
};
#define PIPE_BROKEN	0x1
extern size_t pipe_readable (struct pipe *pipe, int data_only);
extern int pipe_is_readable (struct pipe *pipe, int data_only);
extern error_t pipe_wait_readable (struct pipe *pipe, int noblock, int data_only);
extern error_t pipe_select_readable (struct pipe *pipe, struct timespec *tsp,
int data_only);
extern error_t pipe_wait_writable_amount (struct pipe *pipe, int noblock, size_t amount);
extern error_t pipe_wait_writable (struct pipe *pipe, int noblock);
extern error_t pipe_select_writable (struct pipe *pipe, struct timespec *tsp);
#if defined(__USE_EXTERN_INLINES) || defined(PIPE_DEFINE_EI)
PIPE_EI size_t
pipe_readable (struct pipe *pipe, int data_only)
{
size_t readable = 0;
struct pq *pq = pipe->queue;
struct packet *packet = pq_head (pq, PACKET_TYPE_ANY, NULL);
while (packet)
{
if (packet->type == PACKET_TYPE_DATA)
readable += packet_readable (packet);
packet = packet->next;
}
return readable;
}
PIPE_EI int
pipe_is_readable (struct pipe *pipe, int data_only)
{
struct pq *pq = pipe->queue;
struct packet *packet = pq_head (pq, PACKET_TYPE_ANY, NULL);
if (data_only)
while (packet && packet->type == PACKET_TYPE_CONTROL)
packet = packet->next;
return (packet != NULL);
}
PIPE_EI error_t
pipe_wait_readable (struct pipe *pipe, int noblock, int data_only)
{
while (! pipe_is_readable (pipe, data_only) && ! (pipe->flags & PIPE_BROKEN))
{
if (noblock)
return EWOULDBLOCK;
if (pthread_hurd_cond_wait_np (&pipe->pending_reads, &pipe->lock))
return EINTR;
}
return 0;
}
PIPE_EI error_t
pipe_select_readable (struct pipe *pipe, struct timespec *tsp, int data_only)
{
error_t err = 0;
while (! pipe_is_readable (pipe, data_only) && ! (pipe->flags & PIPE_BROKEN))
{
err = pthread_hurd_cond_timedwait_np (&pipe->pending_read_selects,
&pipe->lock, tsp);
if (err)
break;
}
return err;
}
PIPE_EI error_t
pipe_wait_writable_amount (struct pipe *pipe, int noblock, size_t amount)
{
if (pipe->flags & PIPE_BROKEN)
return EPIPE;
while (pipe_readable (pipe, 1) + amount >= pipe->write_limit)
{
if (noblock)
return EWOULDBLOCK;
if (pthread_hurd_cond_wait_np (&pipe->pending_writes, &pipe->lock))
return EINTR;
if (pipe->flags & PIPE_BROKEN)
return EPIPE;
}
return 0;
}
PIPE_EI error_t
pipe_wait_writable (struct pipe *pipe, int noblock)
{
return pipe_wait_writable_amount (pipe, noblock, 1);
}
PIPE_EI error_t
pipe_select_writable (struct pipe *pipe, struct timespec *tsp)
{
error_t err = 0;
while (! (pipe->flags & PIPE_BROKEN)
&& pipe_readable (pipe, 1) >= pipe->write_limit)
{
err = pthread_hurd_cond_timedwait_np (&pipe->pending_writes,
&pipe->lock, tsp);
if (err)
break;
}
return err;
}
#endif
error_t pipe_create (struct pipe_class *class, struct pipe **pipe);
void pipe_free (struct pipe *pipe);
void _pipe_first_reader (struct pipe *pipe);
void _pipe_first_writer (struct pipe *pipe);
void _pipe_no_readers (struct pipe *pipe);
void _pipe_no_writers (struct pipe *pipe);
void _pipe_wake_writers (struct pipe *pipe);
extern void pipe_acquire_reader (struct pipe *pipe);
extern void pipe_acquire_writer (struct pipe *pipe);
extern void pipe_release_reader (struct pipe *pipe);
extern void pipe_release_writer (struct pipe *pipe);
extern void pipe_add_reader (struct pipe *pipe);
extern void pipe_add_writer (struct pipe *pipe);
extern void pipe_remove_reader (struct pipe *pipe);
extern void pipe_remove_writer (struct pipe *pipe);
extern void pipe_drain (struct pipe *pipe);
#if defined(__USE_EXTERN_INLINES) || defined(PIPE_DEFINE_EI)
PIPE_EI void
pipe_acquire_reader (struct pipe *pipe)
{
pthread_mutex_lock (&pipe->lock);
if (pipe->readers++ == 0)
_pipe_first_reader (pipe);
}
PIPE_EI void
pipe_acquire_writer (struct pipe *pipe)
{
pthread_mutex_lock (&pipe->lock);
if (pipe->writers++ == 0)
_pipe_first_writer (pipe);
}
PIPE_EI void
pipe_release_reader (struct pipe *pipe)
{
if (--pipe->readers == 0)
_pipe_no_readers (pipe);
else
pthread_mutex_unlock (&pipe->lock);
}
PIPE_EI void
pipe_release_writer (struct pipe *pipe)
{
if (--pipe->writers == 0)
_pipe_no_writers (pipe);
else
pthread_mutex_unlock (&pipe->lock);
}
PIPE_EI void
pipe_add_reader (struct pipe *pipe)
{
pipe_acquire_reader (pipe);
pthread_mutex_unlock (&pipe->lock);
}
PIPE_EI void
pipe_add_writer (struct pipe *pipe)
{
pipe_acquire_writer (pipe);
pthread_mutex_unlock (&pipe->lock);
}
PIPE_EI void
pipe_remove_reader (struct pipe *pipe)
{
pthread_mutex_lock (&pipe->lock);
pipe_release_reader (pipe);
}
PIPE_EI void
pipe_remove_writer (struct pipe *pipe)
{
pthread_mutex_lock (&pipe->lock);
pipe_release_writer (pipe);
}
PIPE_EI void
pipe_drain (struct pipe *pipe)
{
pq_drain (pipe->queue);
}
#endif
error_t pipe_send (struct pipe *pipe, int noblock, void *source,
const char *data, size_t data_len,
const char *control, size_t control_len,
const mach_port_t *ports, size_t num_ports,
size_t *amount);
#define pipe_write(pipe, noblock, source, data, data_len, amount) \
pipe_send (pipe, noblock, source, data, data_len, 0, 0, 0, 0, amount)
error_t pipe_recv (struct pipe *pipe, int noblock, unsigned *flags,
void **source,
char **data, size_t *data_len, size_t amount,
char **control, size_t *control_len,
mach_port_t **ports, size_t *num_ports);
#define pipe_read(pipe, noblock, source, data, data_len, amount) \
pipe_recv (pipe, noblock, 0, source, data, data_len, amount, 0,0,0,0)
extern pthread_mutex_t pipe_multiple_lock;
error_t pipe_pair_select (struct pipe *rpipe, struct pipe *wpipe,
struct timespec *tsp, int *select_type,
int data_only);
void pipe_dealloc_addr (void *addr);
#endif