#ifndef	_MIG_MESSAGE_H_
#define _MIG_MESSAGE_H_
#define MACH_MSG_TYPE_PORT_ANY(x)			\
(((x) >= MACH_MSG_TYPE_MOVE_RECEIVE) &&		\
((x) <= MACH_MSG_TYPE_MAKE_SEND_ONCE))
#endif