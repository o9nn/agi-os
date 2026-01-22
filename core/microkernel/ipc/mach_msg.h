#ifndef	_IPC_MACH_MSG_H_
#define _IPC_MACH_MSG_H_
#include <mach/boolean.h>
#include <mach/message.h>
extern mach_msg_return_t
mach_msg_send(mach_msg_user_header_t *, mach_msg_option_t,
mach_msg_size_t, mach_msg_timeout_t, mach_port_name_t);
extern mach_msg_return_t
mach_msg_receive(mach_msg_user_header_t *, mach_msg_option_t,
mach_msg_size_t, mach_port_name_t,
mach_msg_timeout_t, mach_port_name_t);
extern void
mach_msg_receive_continue(void);
extern void
mach_msg_continue(void);
extern boolean_t
mach_msg_interrupt(thread_t);
#endif