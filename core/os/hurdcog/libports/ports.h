#ifndef _HURD_PORTS_
#define _HURD_PORTS_
#include <mach.h>
#include <stdlib.h>
#include <hurd.h>
#include <hurd/ihash.h>
#include <mach/notify.h>
#include <pthread.h>
#include <refcount.h>
#include "port-deref-deferred.h"
#ifdef PORTS_DEFINE_EI
#define PORTS_EI
#else
#define PORTS_EI __extern_inline
#endif
#define PORTS_INHIBITED 0x0100
#define PORTS_BLOCKED 0x0200
#define PORTS_INHIBIT_WAIT 0x0400
#define PORTS_NO_ALLOC 0x0800
#define PORTS_ALLOC_WAIT 0x1000
struct port_info
{
#ifdef __cplusplus
struct port_class *port_class;
#else
struct port_class *class;
#endif
refcounts_t refcounts;
mach_port_mscount_t mscount;
mach_msg_seqno_t cancel_threshold;
int flags;
mach_port_t port_right;
struct rpc_info *current_rpcs;
struct port_bucket *bucket;
hurd_ihash_locp_t hentry;
hurd_ihash_locp_t ports_htable_entry;
};
typedef struct port_info *port_info_t;
#define PORT_HAS_SENDRIGHTS 0x0001
#define PORT_INHIBITED PORTS_INHIBITED
#define PORT_BLOCKED PORTS_BLOCKED
#define PORT_INHIBIT_WAIT PORTS_INHIBIT_WAIT
struct port_bucket
{
mach_port_t portset;
struct hurd_ihash htable;
int rpcs;
int flags;
int count;
struct ports_threadpool threadpool;
struct port_info *notify_port;
};
#define PORT_BUCKET_INHIBITED PORTS_INHIBITED
#define PORT_BUCKET_BLOCKED PORTS_BLOCKED
#define PORT_BUCKET_INHIBIT_WAIT PORTS_INHIBIT_WAIT
#define PORT_BUCKET_NO_ALLOC PORTS_NO_ALLOC
#define PORT_BUCKET_ALLOC_WAIT PORTS_ALLOC_WAIT
struct port_class
{
int flags;
int rpcs;
int count;
void (*clean_routine) (void *);
void (*dropweak_routine) (void *);
struct ports_msg_id_range *uninhibitable_rpcs;
};
#define PORT_CLASS_INHIBITED PORTS_INHIBITED
#define PORT_CLASS_BLOCKED PORTS_BLOCKED
#define PORT_CLASS_INHIBIT_WAIT PORTS_INHIBIT_WAIT
#define PORT_CLASS_NO_ALLOC PORTS_NO_ALLOC
#define PORT_CLASS_ALLOC_WAIT PORTS_ALLOC_WAIT
struct rpc_info
{
thread_t thread;
struct rpc_info *next, **prevp;
struct rpc_notify *notifies;
struct rpc_info *interrupted_next;
};
struct rpc_notify
{
struct rpc_info *rpc;
struct ports_notify *notify;
struct rpc_notify *next;
unsigned pending;
struct rpc_notify *next_req;
struct rpc_notify **prev_req_p;
};
struct ports_notify
{
mach_port_t port;
mach_msg_id_t what;
unsigned pending : 1;
pthread_mutex_t lock;
struct rpc_notify *reqs;
struct ports_notify *next, **prevp;
};
extern struct ports_notify *_ports_notifications;
extern struct ports_notify *_ports_free_ports_notifies;
extern struct rpc_notify *_ports_free_rpc_notifies;
void _ports_remove_notified_rpc (struct rpc_info *rpc);
struct ports_msg_id_range
{
mach_msg_id_t start, end;
struct ports_msg_id_range *next;
};
extern struct ports_msg_id_range *ports_default_uninhibitable_rpcs;
struct port_bucket *ports_create_bucket (void);
struct port_class *ports_create_class (void (*clean_routine)(void *),
void (*dropweak_routine)(void *));
error_t ports_create_port (struct port_class *port_class,
struct port_bucket *bucket,
size_t size,
void *result);
error_t
ports_create_port_noinstall (struct port_class *port_class,
struct port_bucket *bucket,
size_t size,
void *result);
error_t ports_import_port (struct port_class *port_class,
struct port_bucket *bucket,
mach_port_t port, size_t size,
void *result);
void ports_reallocate_port (void *port);
void ports_reallocate_from_external (void *port, mach_port_t receive);
error_t ports_destroy_right (void *port);
mach_port_t ports_claim_right (void *port);
error_t ports_transfer_right (void *topt, void *frompt);
mach_port_t ports_get_right (void *port);
mach_port_t ports_get_send_right (void *port);
void *ports_lookup_port (struct port_bucket *bucket,
mach_port_t port, struct port_class *port_class);
extern void *ports_lookup_payload (struct port_bucket *bucket,
uintptr_t payload,
struct port_class *port_class);
extern mach_port_t ports_payload_get_name (uintptr_t payload);
#if (defined(__USE_EXTERN_INLINES) || defined(PORTS_DEFINE_EI)) && !defined(__cplusplus)
PORTS_EI void *
ports_lookup_payload (struct port_bucket *bucket,
uintptr_t payload,
struct port_class *class)
{
struct port_info *pi = (struct port_info *) payload;
if (pi && ! MACH_PORT_VALID (pi->port_right))
pi = NULL;
if (pi && bucket && pi->bucket != bucket)
pi = NULL;
if (pi && class && pi->class != class)
pi = NULL;
if (pi)
refcounts_unsafe_ref (&pi->refcounts, NULL);
return pi;
}
PORTS_EI mach_port_t
ports_payload_get_name (uintptr_t payload)
{
struct port_info *pi = (struct port_info *) payload;
if (pi)
return pi->port_right;
return MACH_PORT_NULL;
}
#endif
void ports_port_ref (void *port);
void ports_port_ref_weak (void *port);
void ports_port_deref (void *port);
void ports_port_deref_weak (void *port);
#define ports_port_notify_right(port) \
((struct port_info *) (port))->bucket->notify_port->port_right
#define ports_port_is_notify(port) \
({ \
struct port_info *__pi = (port); \
__pi ? (__pi->bucket->notify_port == __pi) : 0; \
})
error_t ports_request_dead_name_notification (void *object,
mach_port_t name,
mach_port_t *previous);
void ports_no_senders (void *port, mach_port_mscount_t mscount);
void ports_dead_name (void *notify, mach_port_t dead_name);
int ports_count_class (struct port_class *port_class);
int ports_count_bucket (struct port_bucket *bucket);
void ports_enable_class (struct port_class *port_class);
void ports_enable_bucket (struct port_bucket *bucket);
error_t ports_bucket_iterate (struct port_bucket *bucket,
error_t (*fun)(void *port));
error_t ports_class_iterate (struct port_class *port_class,
error_t (*fun)(void *port));
error_t _ports_bucket_class_iterate (struct hurd_ihash *ht,
struct port_class *port_class,
error_t (*fun)(void *port));
typedef int (*ports_demuxer_type)(mach_msg_header_t *inp,
mach_msg_header_t *outp);
error_t ports_begin_rpc (void *port, mach_msg_id_t msg_id,
struct rpc_info *info);
void ports_end_rpc (void *port, struct rpc_info *info);
void ports_manage_port_operations_one_thread(struct port_bucket *bucket,
ports_demuxer_type demuxer,
int timeout);
void ports_manage_port_operations_multithread (struct port_bucket *bucket,
ports_demuxer_type demuxer,
int thread_timeout,
int global_timeout,
void (*hook)(void));
error_t ports_inhibit_port_rpcs (void *port);
error_t ports_inhibit_class_rpcs (struct port_class *port_class);
error_t ports_inhibit_bucket_rpcs (struct port_bucket *bucket);
error_t ports_inhibit_all_rpcs (void);
void ports_resume_port_rpcs (void *port);
void ports_resume_class_rpcs (struct port_class *port_class);
void ports_resume_bucket_rpcs (struct port_bucket *bucket);
void ports_resume_all_rpcs (void);
void ports_interrupt_rpcs (void *port);
int ports_self_interrupted (void);
void _ports_record_interruption (struct rpc_info *rpc);
error_t
ports_interrupt_rpc_on_notification (void *object,
struct rpc_info *rpc,
mach_port_t port, mach_msg_id_t what);
error_t
ports_interrupt_self_on_notification (void *object,
mach_port_t port, mach_msg_id_t what);
#define ports_interrupt_self_on_port_death(obj, port) \
ports_interrupt_self_on_notification (obj, port, MACH_NOTIFY_DEAD_NAME)
void ports_interrupt_notified_rpcs (void *object, mach_port_t port,
mach_msg_id_t what);
int ports_notify_server (mach_msg_header_t *, mach_msg_header_t *);
extern kern_return_t
ports_do_mach_notify_dead_name (struct port_info *pi, mach_port_t deadport);
extern kern_return_t
ports_do_mach_notify_msg_accepted (struct port_info *pi, mach_port_t name);
extern kern_return_t
ports_do_mach_notify_no_senders (struct port_info *pi,
mach_port_mscount_t count);
extern kern_return_t
ports_do_mach_notify_port_deleted (struct port_info *pi, mach_port_t name);
extern kern_return_t
ports_do_mach_notify_port_destroyed (struct port_info *pi, mach_port_t name);
extern kern_return_t
ports_do_mach_notify_send_once (struct port_info *pi);
extern boolean_t ports_interrupt_server (mach_msg_header_t *, mach_msg_header_t *);
extern pthread_mutex_t _ports_lock;
extern pthread_cond_t _ports_block;
extern struct hurd_ihash _ports_htable;
extern pthread_rwlock_t _ports_htable_lock;
extern int _ports_total_rpcs;
extern int _ports_flags;
#define _PORTS_INHIBITED PORTS_INHIBITED
#define _PORTS_BLOCKED PORTS_BLOCKED
#define _PORTS_INHIBIT_WAIT PORTS_INHIBIT_WAIT
void _ports_complete_deallocate (struct port_info *);
error_t _ports_create_port_internal (struct port_class *, struct port_bucket *,
size_t, void *, int);
#endif