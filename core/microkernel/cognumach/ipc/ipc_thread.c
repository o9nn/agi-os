#include <kern/assert.h>
#include <ipc/ipc_thread.h>
void
ipc_thread_enqueue(
ipc_thread_queue_t queue,
ipc_thread_t thread)
{
ipc_thread_enqueue_macro(queue, thread);
}
ipc_thread_t
ipc_thread_dequeue(
ipc_thread_queue_t queue)
{
ipc_thread_t first;
first = ipc_thread_queue_first(queue);
if (first != ITH_NULL)
ipc_thread_rmqueue_first_macro(queue, first);
return first;
}
void
ipc_thread_rmqueue(
ipc_thread_queue_t queue,
ipc_thread_t thread)
{
ipc_thread_t next, prev;
assert(queue->ithq_base != ITH_NULL);
next = thread->ith_next;
prev = thread->ith_prev;
if (next == thread) {
assert(prev == thread);
assert(queue->ithq_base == thread);
queue->ithq_base = ITH_NULL;
} else {
if (queue->ithq_base == thread)
queue->ithq_base = next;
next->ith_prev = prev;
prev->ith_next = next;
ipc_thread_links_init(thread);
}
}