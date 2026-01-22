#ifndef _LINUX_RPCSOCK_H
#define _LINUX_RPCSOCK_H
#define RPC_MAXREQS 32
#define RPC_CWNDSCALE 256
#define RPC_MAXCWND (RPC_MAXREQS * RPC_CWNDSCALE)
#define RPC_INITCWND RPC_CWNDSCALE
#define RPC_CONGESTED(rsock) ((rsock)->cong >= (rsock)->cwnd)
#define RPC_HDRSIZE (4 * 4)
struct rpc_timeout {
unsigned long to_initval,
to_maxval,
to_increment;
int to_retries;
char to_exponential;
};
struct rpc_ioreq {
struct rpc_wait * rq_slot;
struct sockaddr * rq_addr;
int rq_alen;
struct iovec rq_svec[UIO_FASTIOV];
unsigned int rq_snr;
unsigned long rq_slen;
struct iovec rq_rvec[UIO_FASTIOV];
unsigned int rq_rnr;
unsigned long rq_rlen;
};
struct rpc_wait;
typedef void (*rpc_callback_fn_t)(int, struct rpc_wait *, void *);
struct rpc_wait {
struct rpc_sock * w_sock;
struct rpc_wait * w_prev;
struct rpc_wait * w_next;
struct rpc_ioreq * w_req;
int w_result;
struct wait_queue * w_wait;
rpc_callback_fn_t w_handler;
void * w_cdata;
char w_queued;
char w_gotit;
__u32 w_xid;
};
struct rpc_sock {
struct file * file;
struct socket * sock;
struct sock * inet;
struct rpc_wait waiting[RPC_MAXREQS];
unsigned long cong;
unsigned long cwnd;
struct rpc_wait * pending;
struct rpc_wait * free;
struct wait_queue * backlog;
struct wait_queue * shutwait;
int shutdown;
};
#ifdef __KERNEL__
int rpc_call(struct rpc_sock *, struct rpc_ioreq *,
struct rpc_timeout *);
int rpc_reserve(struct rpc_sock *, struct rpc_ioreq *, int);
void rpc_release(struct rpc_sock *, struct rpc_ioreq *);
int rpc_transmit(struct rpc_sock *, struct rpc_ioreq *);
int rpc_doio(struct rpc_sock *, struct rpc_ioreq *,
struct rpc_timeout *, int);
struct rpc_sock * rpc_makesock(struct file *);
int rpc_closesock(struct rpc_sock *);
#endif
#endif