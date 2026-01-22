#ifndef _MACH_PORT_H_
#define _MACH_PORT_H_
#include <mach/boolean.h>
#include <mach/machine/vm_types.h>
typedef unsigned int mach_port_name_t;
typedef mach_port_name_t *mach_port_name_array_t;
typedef const mach_port_name_t *const_mach_port_name_array_t;
#ifdef KERNEL
typedef vm_offset_t mach_port_t;
#else
typedef mach_port_name_t mach_port_t;
#endif
typedef mach_port_t *mach_port_array_t;
typedef const mach_port_t *const_mach_port_array_t;
typedef int *rpc_signature_info_t;
#define MACH_PORT_NULL 0
#define MACH_PORT_DEAD ((mach_port_t) ~0)
#define MACH_PORT_NAME_NULL ((mach_port_name_t) 0)
#define MACH_PORT_NAME_DEAD ((mach_port_name_t) ~0)
#define MACH_PORT_VALID(port) \
(((port) != MACH_PORT_NULL) && ((port) != MACH_PORT_DEAD))
#define MACH_PORT_NAME_VALID(name) \
(((name) != MACH_PORT_NAME_NULL) && ((name) != MACH_PORT_NAME_DEAD))
typedef natural_t mach_port_right_t;
#define MACH_PORT_RIGHT_SEND ((mach_port_right_t) 0)
#define MACH_PORT_RIGHT_RECEIVE ((mach_port_right_t) 1)
#define MACH_PORT_RIGHT_SEND_ONCE ((mach_port_right_t) 2)
#define MACH_PORT_RIGHT_PORT_SET ((mach_port_right_t) 3)
#define MACH_PORT_RIGHT_DEAD_NAME ((mach_port_right_t) 4)
#define MACH_PORT_RIGHT_NUMBER ((mach_port_right_t) 5)
typedef natural_t mach_port_type_t;
typedef mach_port_type_t *mach_port_type_array_t;
#define MACH_PORT_TYPE(right) ((mach_port_type_t)(1 << ((right)+16)))
#define MACH_PORT_TYPE_NONE ((mach_port_type_t) 0)
#define MACH_PORT_TYPE_SEND MACH_PORT_TYPE(MACH_PORT_RIGHT_SEND)
#define MACH_PORT_TYPE_RECEIVE MACH_PORT_TYPE(MACH_PORT_RIGHT_RECEIVE)
#define MACH_PORT_TYPE_SEND_ONCE MACH_PORT_TYPE(MACH_PORT_RIGHT_SEND_ONCE)
#define MACH_PORT_TYPE_PORT_SET MACH_PORT_TYPE(MACH_PORT_RIGHT_PORT_SET)
#define MACH_PORT_TYPE_DEAD_NAME MACH_PORT_TYPE(MACH_PORT_RIGHT_DEAD_NAME)
#define MACH_PORT_TYPE_SEND_RECEIVE \
(MACH_PORT_TYPE_SEND|MACH_PORT_TYPE_RECEIVE)
#define MACH_PORT_TYPE_SEND_RIGHTS \
(MACH_PORT_TYPE_SEND|MACH_PORT_TYPE_SEND_ONCE)
#define MACH_PORT_TYPE_PORT_RIGHTS \
(MACH_PORT_TYPE_SEND_RIGHTS|MACH_PORT_TYPE_RECEIVE)
#define MACH_PORT_TYPE_PORT_OR_DEAD \
(MACH_PORT_TYPE_PORT_RIGHTS|MACH_PORT_TYPE_DEAD_NAME)
#define MACH_PORT_TYPE_ALL_RIGHTS \
(MACH_PORT_TYPE_PORT_OR_DEAD|MACH_PORT_TYPE_PORT_SET)
#define MACH_PORT_TYPE_DNREQUEST 0x80000000U
#define MACH_PORT_TYPE_MAREQUEST 0x40000000
#define MACH_PORT_TYPE_COMPAT 0x20000000
typedef natural_t mach_port_urefs_t;
typedef integer_t mach_port_delta_t;
typedef natural_t mach_port_seqno_t;
typedef unsigned int mach_port_mscount_t;
typedef unsigned int mach_port_msgcount_t;
typedef unsigned int mach_port_rights_t;
typedef struct mach_port_status {
mach_port_name_t mps_pset;
mach_port_seqno_t mps_seqno;
mach_port_mscount_t mps_mscount;
mach_port_msgcount_t mps_qlimit;
mach_port_msgcount_t mps_msgcount;
mach_port_rights_t mps_sorights;
boolean_t mps_srights;
boolean_t mps_pdrequest;
boolean_t mps_nsrequest;
} mach_port_status_t;
#define MACH_PORT_QLIMIT_DEFAULT ((mach_port_msgcount_t) 5)
#define MACH_PORT_QLIMIT_MAX ((mach_port_msgcount_t) 16)
#endif