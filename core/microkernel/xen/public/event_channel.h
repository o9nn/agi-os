#ifndef __XEN_PUBLIC_EVENT_CHANNEL_H__
#define __XEN_PUBLIC_EVENT_CHANNEL_H__
typedef uint32_t evtchn_port_t;
DEFINE_XEN_GUEST_HANDLE(evtchn_port_t);
#define EVTCHNOP_alloc_unbound 6
struct evtchn_alloc_unbound {
domid_t dom, remote_dom;
evtchn_port_t port;
};
typedef struct evtchn_alloc_unbound evtchn_alloc_unbound_t;
#define EVTCHNOP_bind_interdomain 0
struct evtchn_bind_interdomain {
domid_t remote_dom;
evtchn_port_t remote_port;
evtchn_port_t local_port;
};
typedef struct evtchn_bind_interdomain evtchn_bind_interdomain_t;
#define EVTCHNOP_bind_virq 1
struct evtchn_bind_virq {
uint32_t virq;
uint32_t vcpu;
evtchn_port_t port;
};
typedef struct evtchn_bind_virq evtchn_bind_virq_t;
#define EVTCHNOP_bind_pirq 2
struct evtchn_bind_pirq {
uint32_t pirq;
#define BIND_PIRQ__WILL_SHARE 1
uint32_t flags;
evtchn_port_t port;
};
typedef struct evtchn_bind_pirq evtchn_bind_pirq_t;
#define EVTCHNOP_bind_ipi 7
struct evtchn_bind_ipi {
uint32_t vcpu;
evtchn_port_t port;
};
typedef struct evtchn_bind_ipi evtchn_bind_ipi_t;
#define EVTCHNOP_close 3
struct evtchn_close {
evtchn_port_t port;
};
typedef struct evtchn_close evtchn_close_t;
#define EVTCHNOP_send 4
struct evtchn_send {
evtchn_port_t port;
};
typedef struct evtchn_send evtchn_send_t;
#define EVTCHNOP_status 5
struct evtchn_status {
domid_t dom;
evtchn_port_t port;
#define EVTCHNSTAT_closed 0
#define EVTCHNSTAT_unbound 1
#define EVTCHNSTAT_interdomain 2
#define EVTCHNSTAT_pirq 3
#define EVTCHNSTAT_virq 4
#define EVTCHNSTAT_ipi 5
uint32_t status;
uint32_t vcpu;
union {
struct {
domid_t dom;
} unbound;
struct {
domid_t dom;
evtchn_port_t port;
} interdomain;
uint32_t pirq;
uint32_t virq;
} u;
};
typedef struct evtchn_status evtchn_status_t;
#define EVTCHNOP_bind_vcpu 8
struct evtchn_bind_vcpu {
evtchn_port_t port;
uint32_t vcpu;
};
typedef struct evtchn_bind_vcpu evtchn_bind_vcpu_t;
#define EVTCHNOP_unmask 9
struct evtchn_unmask {
evtchn_port_t port;
};
typedef struct evtchn_unmask evtchn_unmask_t;
#define EVTCHNOP_reset 10
struct evtchn_reset {
domid_t dom;
};
typedef struct evtchn_reset evtchn_reset_t;
struct evtchn_op {
uint32_t cmd;
union {
struct evtchn_alloc_unbound alloc_unbound;
struct evtchn_bind_interdomain bind_interdomain;
struct evtchn_bind_virq bind_virq;
struct evtchn_bind_pirq bind_pirq;
struct evtchn_bind_ipi bind_ipi;
struct evtchn_close close;
struct evtchn_send send;
struct evtchn_status status;
struct evtchn_bind_vcpu bind_vcpu;
struct evtchn_unmask unmask;
} u;
};
typedef struct evtchn_op evtchn_op_t;
DEFINE_XEN_GUEST_HANDLE(evtchn_op_t);
#endif