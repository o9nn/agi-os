#include <testlib.h>
#include <mach/vm_param.h>
#include <mach.user.h>
thread_t test_thread_start(task_t task, void(*routine)(void*), void* arg) {
const vm_size_t stack_size = vm_page_size * 16;
kern_return_t ret;
vm_address_t stack, local_stack;
ret = vm_allocate(mach_task_self(), &local_stack, vm_page_size, TRUE);
ASSERT_RET(ret, "can't allocate local stack");
ret = vm_allocate(task, &stack, stack_size, TRUE);
ASSERT_RET(ret, "can't allocate the stack for a new thread");
ret = vm_protect(task, stack, vm_page_size, FALSE, VM_PROT_NONE);
ASSERT_RET(ret, "can't protect the stack from overflows");
long *top = (long*)(local_stack + vm_page_size) - 1;
#ifdef __i386__
*top = (long)arg;
*(top - 1) = 0;
#elif defined(__x86_64__)
*top = 0;
#endif
ret = vm_write(task, stack + stack_size - vm_page_size, local_stack, vm_page_size);
ASSERT_RET(ret, "can't initialize the stack for the new thread");
ret = vm_deallocate(mach_task_self(), local_stack, vm_page_size);
ASSERT_RET(ret, "can't deallocate local stack");
thread_t thread;
ret = thread_create(task, &thread);
ASSERT_RET(ret, "thread_create()");
struct i386_thread_state state;
unsigned int count;
count = i386_THREAD_STATE_COUNT;
ret = thread_get_state(thread, i386_REGS_SEGS_STATE,
(thread_state_t) &state, &count);
ASSERT_RET(ret, "thread_get_state()");
#ifdef __i386__
state.eip = (long) routine;
state.uesp = (long) (stack + stack_size - sizeof(long) * 2);
state.ebp = 0;
#elif defined(__x86_64__)
state.rip = (long) routine;
state.ursp = (long) (stack + stack_size - sizeof(long) * 1);
state.rbp = 0;
state.rdi = (long)arg;
#endif
ret = thread_set_state(thread, i386_REGS_SEGS_STATE,
(thread_state_t) &state, i386_THREAD_STATE_COUNT);
ASSERT_RET(ret, "thread_set_state");
ret = thread_resume(thread);
ASSERT_RET(ret, "thread_resume");
return thread;
}