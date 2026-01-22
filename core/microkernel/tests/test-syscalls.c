#include <syscalls.h>
#include <testlib.h>
#include <mach/exception.h>
#include <mach/mig_errors.h>
#include <mach/vm_param.h>
#include <mach.user.h>
#include <mach_port.user.h>
#include <exc.server.h>
static struct {
mach_port_t exception_port;
mach_port_t thread;
mach_port_t task;
integer_t exception;
integer_t code;
long_integer_t subcode;
} last_exc;
kern_return_t catch_exception_raise(mach_port_t exception_port,
mach_port_t thread, mach_port_t task,
integer_t exception, integer_t code,
long_integer_t subcode)
{
printf("received catch_exception_raise(%u %u %u %d %d %d)\n",
exception_port, thread, task, exception, code, subcode);
last_exc.exception_port = exception_port;
last_exc.thread = thread;
last_exc.task = task;
last_exc.exception = exception;
last_exc.code = code;
last_exc.subcode = subcode;
thread_terminate(thread);
return KERN_SUCCESS;
}
void test_syscall_bad_arg_on_stack(void *arg)
{
#ifdef __x86_64__
asm volatile("movq	$0x123,%rsp;" \
"movq	$-25,%rax;" \
"syscall;" \
);
#else
asm volatile("mov	$0x123,%esp;" \
"mov	$-25,%eax;" \
"lcall	$0x7,$0x0;" \
);
#endif
FAILURE("we shouldn't be here!");
}
void test_bad_syscall_num(void *arg)
{
#ifdef __x86_64__
asm volatile("movq	$0x123456,%rax;" \
"syscall;" \
);
#else
asm volatile("mov	$0x123456,%eax;" \
"lcall	$0x7,$0x0;" \
);
#endif
FAILURE("we shouldn't be here!");
}
int main(int argc, char *argv[], int envc, char *envp[])
{
int err;
mach_port_t excp;
err = mach_port_allocate(mach_task_self (), MACH_PORT_RIGHT_RECEIVE, &excp);
ASSERT_RET(err, "creating exception port");
err = mach_port_insert_right(mach_task_self(), excp, excp,
MACH_MSG_TYPE_MAKE_SEND);
ASSERT_RET(err, "inserting send right into exception port");
err = task_set_special_port(mach_task_self(), TASK_EXCEPTION_PORT, excp);
ASSERT_RET(err, "setting task exception port");
boolean_t exc_server
(mach_msg_header_t *InHeadP, mach_msg_header_t *OutHeadP);
memset(&last_exc, 0, sizeof(last_exc));
test_thread_start(mach_task_self(), test_bad_syscall_num, NULL);
ASSERT_RET(mach_msg_server_once(exc_server, 4096, excp, MACH_MSG_OPTION_NONE), "error in exc server");
ASSERT((last_exc.exception == EXC_BAD_INSTRUCTION) && (last_exc.code == EXC_I386_INVOP),
"bad exception for test_bad_syscall_num()");
memset(&last_exc, 0, sizeof(last_exc));
test_thread_start(mach_task_self(), test_syscall_bad_arg_on_stack, NULL);
ASSERT_RET(mach_msg_server_once(exc_server, 4096, excp, MACH_MSG_OPTION_NONE), "error in exc server");
ASSERT((last_exc.exception == EXC_BAD_ACCESS) && (last_exc.code == KERN_INVALID_ADDRESS),
"bad exception for test_syscall_bad_arg_on_stack()");
return 0;
}