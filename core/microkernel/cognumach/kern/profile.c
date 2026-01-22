#if 0
#include <kern/thread.h>
#include <kern/queue.h>
#include <mach/profil.h>
#include <kern/sched_prim.h>
#include <ipc/ipc_space.h>
extern vm_map_t kernel_map;
thread_t profile_thread_id = THREAD_NULL;
void profile_thread()
{
struct message {
mach_msg_header_t head;
mach_msg_type_t type;
int arg[SIZE_PROF_BUFFER+1];
} msg;
spl_t s;
buf_to_send_t buf_entry;
queue_entry_t prof_queue_entry;
prof_data_t pbuf;
simple_lock_t lock;
msg_return_t mr;
int j;
mpqueue_init(&prof_queue);
msg.head.msgh_bits = MACH_MSGH_BITS(MACH_MSG_TYPE_COPY_SEND, MACH_MSG_TYPE_MAKE_SEND_ONCE);
msg.head.msgh_size = sizeof(msg);
msg.head.msgh_local_port = MACH_PORT_NULL;
msg.head.msgh_kind = MACH_MSGH_KIND_NORMAL;
msg.head.msgh_id = 666666;
msg.type.msgt_name = MACH_MSG_TYPE_INTEGER_32;
msg.type.msgt_size = 32;
msg.type.msgt_number = SIZE_PROF_BUFFER+1;
msg.type.msgt_inline = TRUE;
msg.type.msgt_longform = FALSE;
msg.type.msgt_deallocate = FALSE;
msg.type.msgt_unused = 0;
while (TRUE) {
s = splsched();
mpdequeue_head(&prof_queue, &prof_queue_entry);
splx(s);
if ((buf_entry = (buf_to_send_t) prof_queue_entry) == NULLBTS)
{
thread_sleep((event_t) profile_thread, lock, TRUE);
if (current_thread()->wait_result != THREAD_AWAKENED)
break;
}
else {
task_t curr_task;
thread_t curr_th;
int *sample;
int curr_buf;
int imax;
curr_th = (thread_t) buf_entry->thread;
curr_buf = (int) buf_entry->number;
pbuf = curr_th->profil_buffer;
msg.head.msgh_remote_port = (mach_port_t) pbuf->prof_port;
sample = pbuf->prof_area[curr_buf].p_zone;
imax = pbuf->prof_area[curr_buf].p_index;
for(j=0 ;j<imax; j++,sample++)
msg.arg[j] = *sample;
pbuf->prof_area[curr_buf].p_full = FALSE;
msg.arg[SIZE_PROF_BUFFER] = imax;
mr = mach_msg(&(msg.head), MACH_SEND_MSG,
sizeof(struct message), 0,
MACH_PORT_NULL, MACH_MSG_TIMEOUT_NONE,
MACH_PORT_NULL);
if (mr != MACH_MSG_SUCCESS) {
printf("profile_thread: mach_msg failed returned %x\n",(int)mr);
}
if (buf_entry->wakeme)
thread_wakeup((event_t) &buf_entry->wakeme);
kmem_free(kernel_map, (buf_to_send_t) buf_entry,
sizeof(struct buf_to_send));
}
}
profile_thread_id = THREAD_NULL;
while (1) {
mpdequeue_head(&prof_queue, &prof_queue_entry);
if ((buf_entry = (buf_to_send_t) prof_queue_entry) == NULLBTS)
break;
if (buf_entry->wakeme)
thread_wakeup((event_t) &buf_entry->wakeme);
kmem_free(kernel_map, (buf_to_send_t) buf_entry,
sizeof(struct buf_to_send));
}
thread_halt_self(thread_exception_return);
}
#include <mach/message.h>
void
send_last_sample_buf(thread_t th)
{
spl_t s;
buf_to_send_t buf_entry;
vm_offset_t vm_buf_entry;
if (th->profil_buffer == NULLPBUF)
return;
if (kmem_alloc( kernel_map, &vm_buf_entry,
sizeof(struct buf_to_send)) != KERN_SUCCESS)
return;
buf_entry = (buf_to_send_t) vm_buf_entry;
buf_entry->thread = (int *) th;
buf_entry->number = th->profil_buffer->prof_index;
s = splsched();
if (profile_thread_id != THREAD_NULL) {
simple_lock_t lock;
buf_entry->wakeme = 1;
mpenqueue_tail( &prof_queue, &(buf_entry->list));
thread_wakeup((event_t) profile_thread);
assert_wait((event_t) &buf_entry->wakeme, TRUE);
splx(s);
thread_block(thread_no_continuation);
} else {
splx(s);
kmem_free(kernel_map, vm_buf_entry, sizeof(struct buf_to_send));
}
}
profile(pc) {
thread_t it_thread = current_thread();
int inout_val = pc;
buf_to_send_t buf_entry;
vm_offset_t vm_buf_entry;
int *val;
if (it_thread->thread_profiled) {
set_pbuf_value(it_thread->profil_buffer, &inout_val);
switch(inout_val) {
case 0:
if (profile_thread_id == THREAD_NULL) {
reset_pbuf_area(it_thread->profil_buffer);
} else printf("ERROR : hardclock : full buffer unsent\n");
break;
case 1:
break;
case 2 :
if (profile_thread_id == THREAD_NULL ||
kmem_alloc(kernel_map,
&vm_buf_entry ,
sizeof(struct buf_to_send)) !=
KERN_SUCCESS) {
reset_pbuf_area(it_thread->profil_buffer);
break;
}
buf_entry = (buf_to_send_t) vm_buf_entry;
buf_entry->thread = (int *)it_thread;
buf_entry->number =
(it_thread->profil_buffer)->prof_index;
mpenqueue_tail(&prof_queue, &(buf_entry->list));
reset_pbuf_area(it_thread->profil_buffer);
if (profile_thread_id != THREAD_NULL)
thread_wakeup((event_t) profile_thread);
break;
default:
printf("ERROR: profile : unexpected case\n");
}
}
}
kern_return_t
mach_sample_thread (ipc_space_t task,
ipc_object_t reply,
thread_t cur_thread)
{
prof_data_t pbuf;
vm_offset_t vmpbuf;
if (reply != MACH_PORT_NULL) {
if (cur_thread->thread_profiled && cur_thread->thread_profiled_own) {
if (reply == cur_thread->profil_buffer->prof_port)
return KERN_SUCCESS;
mach_sample_thread(MACH_PORT_NULL, cur_thread);
}
alloc_pbuf_area(pbuf, vmpbuf);
if ((cur_thread->profil_buffer = pbuf) == NULLPBUF) {
printf("ERROR:mach_sample_thread:cannot allocate pbuf\n");
return KERN_RESOURCE_SHORTAGE;
} else {
if (!set_pbuf_nb(pbuf, NB_PROF_BUFFER-1)) {
printf("ERROR:mach_sample_thread:cannot set pbuf_nb\n");
return KERN_FAILURE;
}
reset_pbuf_area(pbuf);
}
pbuf->prof_port = reply;
cur_thread->thread_profiled = TRUE;
cur_thread->thread_profiled_own = TRUE;
if (profile_thread_id == THREAD_NULL)
profile_thread_id = kernel_thread(current_task(), "profile", profile_thread);
} else {
if (!cur_thread->thread_profiled_own)
cur_thread->thread_profiled = FALSE;
if (!cur_thread->thread_profiled)
return KERN_SUCCESS;
send_last_sample_buf(cur_thread);
cur_thread->thread_profiled_own = FALSE;
cur_thread->thread_profiled = FALSE;
dealloc_pbuf_area(cur_thread->profil_buffer);
cur_thread->profil_buffer = NULLPBUF;
}
return KERN_SUCCESS;
}
kern_return_t
mach_sample_task (ipc_space_t task, ipc_object_t reply, task_t cur_task)
{
prof_data_t pbuf=cur_task->profil_buffer;
vm_offset_t vmpbuf;
int turnon = (reply != MACH_PORT_NULL);
if (turnon) {
if (cur_task->task_profiled) {
if (cur_task->profil_buffer->prof_port == reply)
return KERN_SUCCESS;
(void) mach_sample_task(task, MACH_PORT_NULL, cur_task);
}
if (pbuf == NULLPBUF) {
alloc_pbuf_area(pbuf, vmpbuf);
if (pbuf == NULLPBUF) {
return KERN_RESOURCE_SHORTAGE;
}
cur_task->profil_buffer = pbuf;
}
if (!set_pbuf_nb(pbuf, NB_PROF_BUFFER-1)) {
return KERN_FAILURE;
}
reset_pbuf_area(pbuf);
pbuf->prof_port = reply;
}
if (turnon != cur_task->task_profiled) {
int actual,i,sentone;
thread_t thread;
if (turnon && profile_thread_id == THREAD_NULL)
profile_thread_id =
kernel_thread(current_task(), "profile", profile_thread);
cur_task->task_profiled = turnon;
actual = cur_task->thread_count;
sentone = 0;
for (i=0, thread=(thread_t) queue_first(&cur_task->thread_list);
i < actual;
i++, thread=(thread_t) queue_next(&thread->thread_list)) {
if (!thread->thread_profiled_own) {
thread->thread_profiled = turnon;
if (turnon)
thread->profil_buffer = cur_task->profil_buffer;
else if (!sentone) {
send_last_sample_buf(thread);
sentone = 1;
}
}
}
if (!turnon) {
dealloc_pbuf_area(pbuf);
cur_task->profil_buffer = NULLPBUF;
}
}
return KERN_SUCCESS;
}
#endif