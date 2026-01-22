#ifndef	_MACH_MESSAGE_H_
#define _MACH_MESSAGE_H_
#include <mach/kern_return.h>
#include <mach/port.h>
typedef natural_t mach_msg_timeout_t;
#define MACH_MSG_TIMEOUT_NONE		((mach_msg_timeout_t) 0)
#define MACH_MSGH_BITS_ZERO		0x00000000
#define MACH_MSGH_BITS_REMOTE_MASK	0x000000ff
#define MACH_MSGH_BITS_LOCAL_MASK	0x0000ff00
#define MACH_MSGH_BITS_COMPLEX		0x80000000U
#define	MACH_MSGH_BITS_CIRCULAR		0x40000000
#define	MACH_MSGH_BITS_COMPLEX_PORTS	0x20000000
#define	MACH_MSGH_BITS_COMPLEX_DATA	0x10000000
#define	MACH_MSGH_BITS_MIGRATED		0x08000000
#define	MACH_MSGH_BITS_UNUSED		0x07ff0000
#define	MACH_MSGH_BITS_PORTS_MASK				\
(MACH_MSGH_BITS_REMOTE_MASK|MACH_MSGH_BITS_LOCAL_MASK)
#define MACH_MSGH_BITS(remote, local)				\
((remote) | ((local) << 8))
#define	MACH_MSGH_BITS_REMOTE(bits)				\
((bits) & MACH_MSGH_BITS_REMOTE_MASK)
#define	MACH_MSGH_BITS_LOCAL(bits)				\
(((bits) & MACH_MSGH_BITS_LOCAL_MASK) >> 8)
#define	MACH_MSGH_BITS_PORTS(bits)				\
((bits) & MACH_MSGH_BITS_PORTS_MASK)
#define	MACH_MSGH_BITS_OTHER(bits)				\
((bits) &~ MACH_MSGH_BITS_PORTS_MASK)
typedef unsigned int mach_msg_bits_t;
typedef	unsigned int mach_msg_size_t;
typedef natural_t mach_msg_seqno_t;
typedef integer_t mach_msg_id_t;
typedef	struct mach_msg_header {
mach_msg_bits_t	msgh_bits;
mach_msg_size_t	msgh_size;
union {
mach_port_t		msgh_remote_port;
rpc_uintptr_t   msgh_remote_port_do_not_use;
};
union {
mach_port_t	msgh_local_port;
rpc_uintptr_t	msgh_protected_payload;
};
mach_port_seqno_t	msgh_seqno;
mach_msg_id_t	msgh_id;
} mach_msg_header_t;
#ifdef KERNEL
typedef	struct {
mach_msg_bits_t	msgh_bits;
mach_msg_size_t	msgh_size;
union {
mach_port_name_t	msgh_remote_port;
rpc_uintptr_t       msgh_remote_port_do_not_use;
};
union {
mach_port_name_t	msgh_local_port;
rpc_uintptr_t msgh_protected_payload;
};
mach_port_seqno_t	msgh_seqno;
mach_msg_id_t	msgh_id;
} mach_msg_user_header_t;
#else
typedef mach_msg_header_t mach_msg_user_header_t;
#endif
#define	MACH_MSG_SIZE_MAX	((mach_msg_size_t) ~0)
#define MACH_MSGH_KIND_NORMAL		0x00000000
#if	0
#define MACH_MSGH_KIND_NOTIFICATION	0x00000001
#endif
#define	msgh_kind			msgh_seqno
#define mach_msg_kind_t			mach_port_seqno_t
typedef unsigned int mach_msg_type_name_t;
typedef unsigned int mach_msg_type_size_t;
typedef natural_t  mach_msg_type_number_t;
typedef struct {
union {
mach_port_name_t name;
#ifdef KERNEL
mach_port_t kernel_port;
#else
uintptr_t kernel_port_do_not_use;
#endif
};
} mach_port_name_inlined_t;
typedef struct  {
#ifdef __LP64__
unsigned int	msgt_name : 8,
msgt_size : 16,
msgt_unused : 5,
msgt_inline : 1,
msgt_longform : 1,
msgt_deallocate : 1;
mach_msg_type_number_t   msgt_number;
#else
unsigned int	msgt_name : 8,
msgt_size : 8,
msgt_number : 12,
msgt_inline : 1,
msgt_longform : 1,
msgt_deallocate : 1,
msgt_unused : 1;
#endif
} __attribute__ ((aligned (__alignof__ (uintptr_t)))) mach_msg_type_t;
typedef struct {
#ifdef __LP64__
union {
mach_msg_type_t	msgtl_header;
struct {
unsigned int	msgtl_name : 8,
msgtl_size : 16,
msgtl_unused : 5,
msgtl_inline : 1,
msgtl_longform : 1,
msgtl_deallocate : 1;
mach_msg_type_number_t   msgtl_number;
};
};
#else
mach_msg_type_t	msgtl_header;
unsigned short	msgtl_name;
unsigned short	msgtl_size;
natural_t		msgtl_number;
#endif
} __attribute__ ((aligned (__alignof__ (uintptr_t)))) mach_msg_type_long_t;
#ifdef __LP64__
#ifdef __cplusplus
#if __cplusplus >= 201103L
static_assert (sizeof (mach_msg_type_t) == sizeof (mach_msg_type_long_t),
"mach_msg_type_t and mach_msg_type_long_t need to have the same size.");
#endif
#else
_Static_assert (sizeof (mach_msg_type_t) == sizeof (mach_msg_type_long_t),
"mach_msg_type_t and mach_msg_type_long_t need to have the same size.");
#endif
#endif
#define MACH_MSG_TYPE_UNSTRUCTURED	0
#define MACH_MSG_TYPE_BIT		0
#define MACH_MSG_TYPE_BOOLEAN		0
#define MACH_MSG_TYPE_INTEGER_16	1
#define MACH_MSG_TYPE_INTEGER_32	2
#define MACH_MSG_TYPE_CHAR		8
#define MACH_MSG_TYPE_BYTE		9
#define MACH_MSG_TYPE_INTEGER_8		9
#define MACH_MSG_TYPE_REAL		10
#define MACH_MSG_TYPE_INTEGER_64	11
#define MACH_MSG_TYPE_STRING		12
#define MACH_MSG_TYPE_STRING_C		12
#define MACH_MSG_TYPE_MOVE_RECEIVE	16
#define MACH_MSG_TYPE_MOVE_SEND		17
#define MACH_MSG_TYPE_MOVE_SEND_ONCE	18
#define MACH_MSG_TYPE_COPY_SEND		19
#define MACH_MSG_TYPE_MAKE_SEND		20
#define MACH_MSG_TYPE_MAKE_SEND_ONCE	21
#define MACH_MSG_TYPE_PORT_NAME		15
#define MACH_MSG_TYPE_PORT_RECEIVE	MACH_MSG_TYPE_MOVE_RECEIVE
#define MACH_MSG_TYPE_PORT_SEND		MACH_MSG_TYPE_MOVE_SEND
#define MACH_MSG_TYPE_PORT_SEND_ONCE	MACH_MSG_TYPE_MOVE_SEND_ONCE
#define MACH_MSG_TYPE_PROTECTED_PAYLOAD	23
#define MACH_MSG_TYPE_LAST		23
#define MACH_MSG_TYPE_POLYMORPHIC	((mach_msg_type_name_t) -1)
#define MACH_MSG_TYPE_PORT_ANY(x)			\
(((x) >= MACH_MSG_TYPE_MOVE_RECEIVE) &&		\
((x) <= MACH_MSG_TYPE_MAKE_SEND_ONCE))
#define	MACH_MSG_TYPE_PORT_ANY_SEND(x)			\
(((x) >= MACH_MSG_TYPE_MOVE_SEND) &&		\
((x) <= MACH_MSG_TYPE_MAKE_SEND_ONCE))
#define	MACH_MSG_TYPE_PORT_ANY_RIGHT(x)			\
(((x) >= MACH_MSG_TYPE_MOVE_RECEIVE) &&		\
((x) <= MACH_MSG_TYPE_MOVE_SEND_ONCE))
typedef integer_t mach_msg_option_t;
#define MACH_MSG_OPTION_NONE	0x00000000
#define	MACH_SEND_MSG		0x00000001
#define	MACH_RCV_MSG		0x00000002
#define MACH_SEND_TIMEOUT	0x00000010
#define MACH_SEND_NOTIFY	0x00000020
#define MACH_SEND_INTERRUPT	0x00000040
#define MACH_SEND_CANCEL	0x00000080
#define MACH_RCV_TIMEOUT	0x00000100
#define MACH_RCV_NOTIFY		0x00000200
#define MACH_RCV_INTERRUPT	0x00000400
#define MACH_RCV_LARGE		0x00000800
#define MACH_SEND_ALWAYS	0x00010000
#ifdef __LP64__
#if defined(KERNEL) && defined(USER32)
#define MACH_MSG_USER_ALIGNMENT 4
#else
#define MACH_MSG_USER_ALIGNMENT 8
#endif
#else
#define MACH_MSG_USER_ALIGNMENT 4
#endif
#ifdef KERNEL
#define MACH_MSG_KERNEL_ALIGNMENT sizeof(uintptr_t)
#define mach_msg_align(x, alignment)	\
( ( ((vm_offset_t)(x)) + ((alignment)-1) ) & ~((alignment)-1) )
#define mach_msg_user_align(x) mach_msg_align(x, MACH_MSG_USER_ALIGNMENT)
#define mach_msg_kernel_align(x) mach_msg_align(x, MACH_MSG_KERNEL_ALIGNMENT)
#define mach_msg_user_is_misaligned(x) ((x) & ((MACH_MSG_USER_ALIGNMENT)-1))
#define mach_msg_kernel_is_misaligned(x) ((x) & ((MACH_MSG_KERNEL_ALIGNMENT)-1))
#endif
typedef kern_return_t mach_msg_return_t;
#define MACH_MSG_SUCCESS		0x00000000
#define	MACH_MSG_MASK			0x00003c00
#define	MACH_MSG_IPC_SPACE		0x00002000
#define	MACH_MSG_VM_SPACE		0x00001000
#define	MACH_MSG_IPC_KERNEL		0x00000800
#define	MACH_MSG_VM_KERNEL		0x00000400
#define MACH_SEND_IN_PROGRESS		0x10000001
#define MACH_SEND_INVALID_DATA		0x10000002
#define MACH_SEND_INVALID_DEST		0x10000003
#define MACH_SEND_TIMED_OUT		0x10000004
#define MACH_SEND_WILL_NOTIFY		0x10000005
#define MACH_SEND_NOTIFY_IN_PROGRESS	0x10000006
#define MACH_SEND_INTERRUPTED		0x10000007
#define MACH_SEND_MSG_TOO_SMALL		0x10000008
#define MACH_SEND_INVALID_REPLY		0x10000009
#define MACH_SEND_INVALID_RIGHT		0x1000000a
#define MACH_SEND_INVALID_NOTIFY	0x1000000b
#define MACH_SEND_INVALID_MEMORY	0x1000000c
#define MACH_SEND_NO_BUFFER		0x1000000d
#define MACH_SEND_NO_NOTIFY		0x1000000e
#define MACH_SEND_INVALID_TYPE		0x1000000f
#define MACH_SEND_INVALID_HEADER	0x10000010
#define MACH_RCV_IN_PROGRESS		0x10004001
#define MACH_RCV_INVALID_NAME		0x10004002
#define MACH_RCV_TIMED_OUT		0x10004003
#define MACH_RCV_TOO_LARGE		0x10004004
#define MACH_RCV_INTERRUPTED		0x10004005
#define MACH_RCV_PORT_CHANGED		0x10004006
#define MACH_RCV_INVALID_NOTIFY		0x10004007
#define MACH_RCV_INVALID_DATA		0x10004008
#define MACH_RCV_PORT_DIED		0x10004009
#define	MACH_RCV_IN_SET			0x1000400a
#define	MACH_RCV_HEADER_ERROR		0x1000400b
#define	MACH_RCV_BODY_ERROR		0x1000400c
extern mach_msg_return_t
mach_msg_trap
(mach_msg_user_header_t *msg,
mach_msg_option_t option,
mach_msg_size_t send_size,
mach_msg_size_t rcv_size,
mach_port_name_t rcv_name,
mach_msg_timeout_t timeout,
mach_port_name_t notify);
extern mach_msg_return_t
mach_msg
(mach_msg_header_t *msg,
mach_msg_option_t option,
mach_msg_size_t send_size,
mach_msg_size_t rcv_size,
mach_port_name_t rcv_name,
mach_msg_timeout_t timeout,
mach_port_name_t notify);
extern __typeof (mach_msg) __mach_msg;
extern __typeof (mach_msg_trap) __mach_msg_trap;
#endif