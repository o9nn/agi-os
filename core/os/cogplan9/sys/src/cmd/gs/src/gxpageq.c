#include "gx.h"
#include "gxdevice.h"
#include "gxclist.h"
#include "gxpageq.h"
#include "gserrors.h"
#include "gsstruct.h"
struct gx_page_queue_s {
gs_memory_t *memory;
gx_monitor_t *monitor;
int entry_count;
bool dequeue_in_progress;
gx_semaphore_t *render_req_sema;
bool enable_render_done_signal;
gx_semaphore_t *render_done_sema;
gx_page_queue_entry_t *last_in;
gx_page_queue_entry_t *first_in;
gx_page_queue_entry_t *reserve_entry;
};
private const gx_band_page_info_t null_page_info = { PAGE_INFO_NULL_VALUES };
#define private_st_gx_page_queue()\
gs_private_st_ptrs4(st_gx_page_queue, gx_page_queue_t, "gx_page_queue",\
gx_page_queue_enum_ptrs, gx_page_queue_reloc_ptrs,\
monitor, first_in, last_in, reserve_entry);
private_st_gx_page_queue_entry();
private_st_gx_page_queue();
private gx_page_queue_entry_t *
gx_page_queue_remove_first(
gx_page_queue_t * queue
);
gx_page_queue_t *
gx_page_queue_alloc(gs_memory_t *mem)
{
return gs_alloc_struct(mem, gx_page_queue_t, &st_gx_page_queue,
"gx_page_queue_alloc");
}
gx_page_queue_entry_t *
gx_page_queue_entry_alloc(
gx_page_queue_t * queue
)
{
gx_page_queue_entry_t *entry
= gs_alloc_struct(queue->memory, gx_page_queue_entry_t,
&st_gx_page_queue_entry, "gx_page_queue_entry_alloc");
if (entry != 0) {
entry->next = 0;
entry->queue = queue;
}
return entry;
}
void
gx_page_queue_entry_free(
gx_page_queue_entry_t * entry
)
{
gs_free_object(entry->queue->memory, entry, "gx_page_queue_entry_free");
}
void
gx_page_queue_entry_free_page_info(
gx_page_queue_entry_t * entry
)
{
clist_close_page_info( &entry->page_info );
}
int
gx_page_queue_init(
gx_page_queue_t * queue,
gs_memory_t * memory
)
{
queue->memory = memory;
queue->monitor = gx_monitor_alloc(memory);
queue->entry_count = 0;
queue->dequeue_in_progress = false;
queue->render_req_sema = gx_semaphore_alloc(memory);
queue->enable_render_done_signal = false;
queue->render_done_sema = gx_semaphore_alloc(memory);
queue->first_in = queue->last_in = 0;
queue->reserve_entry = gx_page_queue_entry_alloc(queue);
if (queue->monitor && queue->render_req_sema && queue->render_done_sema
&& queue->reserve_entry)
return 0;
else {
gx_page_queue_dnit(queue);
return gs_error_VMerror;
}
}
void
gx_page_queue_dnit(
gx_page_queue_t * queue
)
{
gx_page_queue_entry_t *entry;
while ((entry = gx_page_queue_remove_first(queue)) != 0) {
gx_page_queue_entry_free_page_info(entry);
gx_page_queue_entry_free(entry);
}
if (queue->monitor) {
gx_monitor_free(queue->monitor);
queue->monitor = 0;
}
if (queue->render_req_sema) {
gx_semaphore_free(queue->render_req_sema);
queue->render_req_sema = 0;
}
if (queue->render_done_sema) {
gx_semaphore_free(queue->render_done_sema);
queue->render_done_sema = 0;
}
if (queue->reserve_entry) {
gx_page_queue_entry_free(queue->reserve_entry);
queue->reserve_entry = 0;
}
}
private gx_page_queue_entry_t *
gx_page_queue_remove_first(
gx_page_queue_t * queue
)
{
gx_page_queue_entry_t *entry = 0;
gx_monitor_enter(queue->monitor);
if (queue->entry_count) {
entry = queue->first_in;
queue->first_in = entry->next;
if (queue->last_in == entry)
queue->last_in = 0;
--queue->entry_count;
}
gx_monitor_leave(queue->monitor);
return entry;
}
private void
gx_page_queue_add_last(
gx_page_queue_entry_t * entry
)
{
gx_page_queue_t *queue = entry->queue;
gx_monitor_enter(queue->monitor);
entry->next = 0;
if (queue->last_in != 0)
queue->last_in->next = entry;
queue->last_in = entry;
if (queue->first_in == 0)
queue->first_in = entry;
++queue->entry_count;
gx_monitor_leave(queue->monitor);
}
int
gx_page_queue_wait_one_page(
gx_page_queue_t * queue
)
{
int code;
gx_monitor_enter(queue->monitor);
if (!queue->entry_count && !queue->dequeue_in_progress) {
code = 0;
gx_monitor_leave(queue->monitor);
} else {
queue->enable_render_done_signal = true;
gx_monitor_leave(queue->monitor);
gx_semaphore_wait(queue->render_done_sema);
code = 1;
}
return code;
}
void
gx_page_queue_wait_until_empty(
gx_page_queue_t * queue
)
{
while (gx_page_queue_wait_one_page(queue));
}
void
gx_page_queue_enqueue(
gx_page_queue_entry_t * entry
)
{
gx_page_queue_t *queue = entry->queue;
gx_page_queue_add_last(entry);
gx_semaphore_signal(queue->render_req_sema);
}
int
gx_page_queue_add_page(
gx_page_queue_t * queue,
gx_page_queue_action_t action,
const gx_band_page_info_t * page_info,
int page_count
)
{
int code = 0;
gx_page_queue_entry_t *entry
= gx_page_queue_entry_alloc(queue);
if (!entry) {
gx_monitor_enter(queue->monitor);
entry = queue->reserve_entry;
queue->reserve_entry = 0;
gx_monitor_leave(queue->monitor);
}
entry->action = action;
if (page_info != 0)
entry->page_info = *page_info;
else
entry->page_info = null_page_info;
entry->num_copies = page_count;
gx_page_queue_enqueue(entry);
while (!queue->reserve_entry) {
queue->reserve_entry = gx_page_queue_entry_alloc(queue);
if (!queue->reserve_entry && !gx_page_queue_wait_one_page(queue)) {
code = gs_note_error(gs_error_Fatal);
break;
}
}
return code;
}
gx_page_queue_entry_t *
gx_page_queue_start_dequeue(
gx_page_queue_t * queue
)
{
gx_semaphore_wait(queue->render_req_sema);
queue->dequeue_in_progress = true;
return gx_page_queue_remove_first(queue);
}
void
gx_page_queue_finish_dequeue(
gx_page_queue_entry_t * entry
)
{
gx_page_queue_t *queue = entry->queue;
gx_monitor_enter(queue->monitor);
if (queue->enable_render_done_signal) {
queue->enable_render_done_signal = false;
gx_semaphore_signal(queue->render_done_sema);
}
queue->dequeue_in_progress = false;
gx_page_queue_entry_free_page_info(entry);
gx_page_queue_entry_free(entry);
gx_monitor_leave(queue->monitor);
}