#ifndef __SOCK_H__
#define __SOCK_H__
#include <assert-backtrace.h>
#include <pthread.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <hurd/ports.h>
struct pipe;
struct pipe_class;
struct sock_user
{
struct port_info pi;
struct sock *sock;
};
struct sock
{
int refs;
pthread_mutex_t lock;
struct pipe_class *pipe_class;
struct pipe *read_pipe, *write_pipe;
size_t req_write_limit;
unsigned flags;
mach_port_t id;
time_value_t change_time;
mode_t mode;
struct addr *addr;
struct connq *listen_queue;
struct connq *connect_queue;
uid_t uid;
gid_t gid;
};
#define PFLOCAL_SOCK_CONNECTED		0x1
#define PFLOCAL_SOCK_NONBLOCK		0x2
#define PFLOCAL_SOCK_SHUTDOWN_READ	0x4
#define PFLOCAL_SOCK_SHUTDOWN_WRITE	0x8
error_t sock_acquire_read_pipe (struct sock *sock, struct pipe **pipe);
error_t sock_acquire_write_pipe (struct sock *sock, struct pipe **pipe);
error_t sock_connect (struct sock *sock1, struct sock *sock2);
error_t sock_create (struct pipe_class *pipe_class, mode_t mode,
struct sock **sock);
void sock_free (struct sock *sock);
void _sock_norefs (struct sock *sock);
error_t sock_bind (struct sock *sock, struct addr *addr);
static inline void __attribute__ ((unused))
sock_deref (struct sock *sock)
{
error_t err;
pthread_mutex_lock (&sock->lock);
sock->refs--;
if (sock->refs == 0)
_sock_norefs (sock);
else if (sock->refs == 1 && sock->addr)
{
sock->refs++;
pthread_mutex_unlock (&sock->lock);
err = sock_bind (sock, NULL);
assert_backtrace (!err);
pthread_mutex_lock (&sock->lock);
sock->refs--;
assert_backtrace (sock->refs == 0);
_sock_norefs (sock);
}
else
pthread_mutex_unlock (&sock->lock);
}
error_t sock_clone (struct sock *template, struct sock **sock);
error_t sock_create_port (struct sock *sock, mach_port_t *port);
error_t sock_get_addr (struct sock *sock, struct addr **addr);
error_t sock_get_write_addr_port (struct sock *sock, mach_port_t *addr_port);
void sock_shutdown (struct sock *sock, unsigned flags);
error_t addr_create (struct addr **addr);
error_t addr_get_sock (struct addr *addr, struct sock **sock);
error_t sock_global_init (void);
error_t sock_global_shutdown (void);
extern struct port_class *sock_user_port_class;
extern struct port_class *addr_port_class;
#define PFLOCAL_WRITE_LIMIT_MAX (1024*1024)
#endif