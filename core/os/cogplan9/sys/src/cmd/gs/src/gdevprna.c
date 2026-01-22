#include "gdevprna.h"
#include "gsalloc.h"
#include "gsdevice.h"
#include "gsmemlok.h"
#include "gsmemret.h"
#include "gsnogc.h"
#include "gxcldev.h"
#include "gxclpath.h"
#include "gxpageq.h"
#include "gzht.h"
#define RendererAllocationOverheadBytes 503000
#ifdef DEBUG
#endif
private dev_proc_close_device(gdev_prn_async_write_close_device);
private dev_proc_output_page(gdev_prn_async_write_output_page);
private dev_proc_put_params(gdev_prn_async_write_put_params);
private dev_proc_get_hardware_params(gdev_prn_async_write_get_hardware_params);
private dev_proc_put_params(gdev_prn_async_render_put_params);
private void gdev_prn_dealloc(gx_device_printer *);
private proc_free_up_bandlist_memory(gdev_prn_async_write_free_up_bandlist_memory);
private int flush_page(gx_device_printer *, bool);
private int reopen_clist_after_flush(gx_device_printer *);
private void reinit_printer_into_printera(gx_device_printer * const);
private int alloc_bandlist_memory(gs_memory_t **, gs_memory_t *);
private void free_bandlist_memory(gs_memory_t *);
private int alloc_render_memory(gs_memory_t **, gs_memory_t *, long);
private void free_render_memory(gs_memory_t *);
private gs_memory_recover_status_t
prna_mem_recover(gs_memory_retrying_t *rmem, void *proc_data);
int
gdev_prn_async_write_open(gx_device_printer * pwdev, int max_raster,
int min_band_height, int max_src_image_row)
{
gx_device *const pdev = (gx_device *) pwdev;
int code;
bool writer_is_open = false;
gx_device_clist_writer *const pcwdev =
&((gx_device_clist *) pwdev)->writer;
gx_device_clist_reader *pcrdev = 0;
gx_device_printer *prdev = 0;
gs_memory_t *render_memory = 0;
pwdev->page_queue = 0;
pwdev->bandlist_memory = 0;
pwdev->async_renderer = 0;
if ((code = alloc_render_memory(&render_memory,
pwdev->memory->non_gc_memory, RendererAllocationOverheadBytes + max_raster
+ (max_raster + sizeof(void *) * 2) * min_band_height
+ max_src_image_row + gx_ht_cache_default_bits() * 2)) < 0)
goto open_err;
if ((code = alloc_bandlist_memory
(&pwdev->bandlist_memory, pwdev->memory->non_gc_memory)) < 0)
goto open_err;
pwdev->space_params.banding_type = BandingAlways;
pwdev->space_params.params_are_read_only = true;
code = gs_copydevice((gx_device **) & prdev, pdev, render_memory);
pcrdev = &((gx_device_clist *) prdev)->reader;
if (code < 0)
goto open_err;
pwdev->OpenOutputFile = 0;
pwdev->free_up_bandlist_memory = &gdev_prn_async_write_free_up_bandlist_memory;
pwdev->clist_disable_mask |= clist_disable_fill_path |
clist_disable_stroke_path | clist_disable_complex_clip |
clist_disable_nonrect_hl_image | clist_disable_pass_thru_params;
if ((code = gdev_prn_open(pdev)) >= 0) {
writer_is_open = true;
reinit_printer_into_printera(pwdev);
pwdev->async_renderer = prdev;
if ((pwdev->page_queue = gx_page_queue_alloc(pwdev->bandlist_memory)) == 0)
code = gs_note_error(gs_error_VMerror);
else
code = gx_page_queue_init(pwdev->page_queue, pwdev->bandlist_memory);
}
if (code >= 0) {
gx_semaphore_t *open_semaphore;
prdev->space_params.band = pcwdev->page_info.band_params;
prdev->buffer_memory = prdev->memory;
prdev->space_params.params_are_read_only = false;
prdev->page_queue = pwdev->page_queue;
if (!(open_semaphore = gx_semaphore_alloc(prdev->memory)))
code = gs_note_error(gs_error_VMerror);
else {
gdev_prn_start_render_params thread_params;
thread_params.writer_device = pwdev;
thread_params.open_semaphore = open_semaphore;
thread_params.open_code = 0;
code = (*pwdev->printer_procs.start_render_thread)
(&thread_params);
if (code >= 0)
gx_semaphore_wait(open_semaphore);
code = thread_params.open_code;
gx_semaphore_free(open_semaphore);
}
}
if (code >= 0) {
gs_memory_retrying_set_recover(
(gs_memory_retrying_t *)pwdev->memory->non_gc_memory,
prna_mem_recover,
(void *)pcwdev
);
}
if (code < 0) {
open_err:
if (render_memory && !prdev)
free_render_memory(render_memory);
gdev_prn_dealloc(pwdev);
if (writer_is_open) {
gdev_prn_close(pdev);
pwdev->free_up_bandlist_memory = 0;
}
}
return code;
}
private gs_memory_recover_status_t
prna_mem_recover(gs_memory_retrying_t *rmem, void *proc_data)
{
int pages_remain = 0;
gx_device_clist_writer *cldev = proc_data;
if (cldev->free_up_bandlist_memory != NULL)
pages_remain =
(*cldev->free_up_bandlist_memory)( (gx_device *)cldev, false );
return (pages_remain > 0) ? RECOVER_STATUS_RETRY_OK : RECOVER_STATUS_NO_RETRY;
}
private void
reinit_printer_into_printera(
gx_device_printer * const pdev
)
{
if (dev_proc(pdev, close_device) == gdev_prn_close)
set_dev_proc(pdev, close_device, gdev_prn_async_write_close_device);
set_dev_proc(pdev, output_page, gdev_prn_async_write_output_page);
set_dev_proc(pdev, put_params, gdev_prn_async_write_put_params);
set_dev_proc(pdev, get_xfont_procs, gx_default_get_xfont_procs);
set_dev_proc(pdev, get_xfont_device, gx_default_get_xfont_device);
set_dev_proc(pdev, get_hardware_params, gdev_prn_async_write_get_hardware_params);
pdev->free_up_bandlist_memory = &gdev_prn_async_write_free_up_bandlist_memory;
}
private int
gdev_prn_async_write_close_device(gx_device * pdev)
{
gx_device_printer *const pwdev = (gx_device_printer *) pdev;
gx_page_queue_add_page(pwdev->page_queue,
GX_PAGE_QUEUE_ACTION_TERMINATE, 0, 0);
gx_page_queue_wait_until_empty(pwdev->page_queue);
gdev_prn_close(pdev);
pwdev->free_up_bandlist_memory = 0;
gdev_prn_dealloc(pwdev);
return 0;
}
private void
gdev_prn_dealloc(gx_device_printer * pwdev)
{
gx_device_printer *const prdev = pwdev->async_renderer;
if (prdev) {
gs_memory_t *render_alloc = prdev->memory;
gs_free_object(render_alloc, prdev, "gdev_prn_dealloc");
free_render_memory(render_alloc);
}
if (pwdev->page_queue) {
gx_page_queue_dnit(pwdev->page_queue);
gs_free_object(pwdev->bandlist_memory, pwdev->page_queue,
"gdev_prn_dealloc");
pwdev->page_queue = 0;
}
if (pwdev->bandlist_memory)
free_bandlist_memory(pwdev->bandlist_memory);
}
int
gdev_prn_async_render_open(gx_device_printer * prdev)
{
gx_device *const pdev = (gx_device *) prdev;
prdev->is_async_renderer = true;
return gdev_prn_open(pdev);
}
int
gdev_prn_async_render_close_device(gx_device_printer * prdev)
{
gx_device *const pdev = (gx_device *) prdev;
return gdev_prn_close(pdev);
}
private void
reinit_printer_into_renderer(
gx_device_printer * const pdev
)
{
set_dev_proc(pdev, put_params, gdev_prn_async_render_put_params);
}
int
gdev_prn_async_render_thread(
gdev_prn_start_render_params * params
)
{
gx_device_printer *const pwdev = params->writer_device;
gx_device_printer *const prdev = pwdev->async_renderer;
gx_page_queue_entry_t *entry;
int code;
if (prdev->printer_procs.open_render_device ==
gx_default_open_render_device)
code = gdev_prn_async_render_open(prdev);
else
code = (*prdev->printer_procs.open_render_device) (prdev);
reinit_printer_into_renderer(prdev);
if (code >= 0 &&
((gx_device_clist *) pwdev)->writer.page_tile_cache_size !=
((gx_device_clist *) prdev)->writer.page_tile_cache_size) {
gdev_prn_async_render_close_device(prdev);
code = gs_note_error(gs_error_VMerror);
}
params->open_code = code;
gx_semaphore_signal(params->open_semaphore);
if (code < 0)
return code;
prdev->is_open = true;
while ((entry = gx_page_queue_start_dequeue(prdev->page_queue))
&& entry->action != GX_PAGE_QUEUE_ACTION_TERMINATE) {
if (!prdev->is_open) {
if (prdev->printer_procs.open_render_device ==
gx_default_open_render_device)
code = gdev_prn_async_render_open(prdev);
else
code = (*prdev->printer_procs.open_render_device) (prdev);
reinit_printer_into_renderer(prdev);
if (code >= 0) {
prdev->is_open = true;
gdev_prn_output_page((gx_device *) prdev, 0, true);
}
}
if (prdev->is_open) {
((gx_device_clist *) prdev)->common.page_info = entry->page_info;
if (clist_setup_params((gx_device *) prdev) >= 0)
((gx_device_clist *) prdev)->common.page_info = entry->page_info;
switch (entry->action) {
case GX_PAGE_QUEUE_ACTION_FULL_PAGE:
(*dev_proc(prdev, output_page))((gx_device *) prdev,
entry->num_copies, true);
break;
case GX_PAGE_QUEUE_ACTION_PARTIAL_PAGE:
case GX_PAGE_QUEUE_ACTION_COPY_PAGE:
(*dev_proc(prdev, output_page))((gx_device *) prdev,
entry->num_copies, false);
break;
}
}
gx_page_queue_finish_dequeue(entry);
}
if (prdev->printer_procs.close_render_device ==
gx_default_close_render_device)
gdev_prn_async_render_close_device(prdev);
else
(*prdev->printer_procs.close_render_device)(prdev);
prdev->is_open = false;
gx_page_queue_finish_dequeue(entry);
return 0;
}
private int
gdev_prn_async_write_put_params(gx_device * pdev, gs_param_list * plist)
{
gx_device_clist_writer *const pclwdev =
&((gx_device_clist *) pdev)->writer;
gx_device_printer *const pwdev = (gx_device_printer *) pdev;
gdev_prn_space_params save_sp = pwdev->space_params;
int save_height = pwdev->height;
int save_width = pwdev->width;
int code, ecode;
if (!pwdev->is_open)
return (*pwdev->orig_procs.put_params) (pdev, plist);
pwdev->is_open = false;
code = (*pwdev->orig_procs.put_params) (pdev, plist);
pwdev->is_open = true;
pwdev->OpenOutputFile = 0;
if (memcmp(&pwdev->space_params, &save_sp, sizeof(save_sp)) != 0 ||
pwdev->width != save_width || pwdev->height != save_height
) {
int pageq_remaining;
int new_width = pwdev->width;
int new_height = pwdev->height;
gdev_prn_space_params new_sp = pwdev->space_params;
pwdev->width = save_width;
pwdev->height = save_height;
pwdev->space_params = save_sp;
code = flush_page(pwdev, false);
pageq_remaining = 1;
do {
ecode =
gdev_prn_reallocate_memory(pdev,
&new_sp, new_width, new_height);
if (ecode >= 0)
break;
if (!pdev->is_open) {
gdev_prn_async_write_close_device(pdev);
return ecode;
}
pclwdev->error_is_retryable = (ecode == gs_error_VMerror);
}
while (pageq_remaining >= 1 &&
(pageq_remaining = ecode =
clist_VMerror_recover(pclwdev, ecode)) >= 0);
if (ecode < 0) {
gdev_prn_free_memory(pdev);
pclwdev->is_open = false;
code = ecode;
}
} else if (code >= 0) {
do
if ((ecode = cmd_put_params(pclwdev, plist)) >= 0)
break;
while ((ecode = clist_VMerror_recover(pclwdev, ecode)) >= 0);
if (ecode < 0 && pclwdev->error_is_retryable &&
pclwdev->driver_call_nesting == 0
)
ecode = clist_VMerror_recover_flush(pclwdev, ecode);
if (ecode < 0)
code = ecode;
}
reinit_printer_into_printera(pwdev);
return code;
}
private int
gdev_prn_async_write_get_hardware_params(gx_device * pdev, gs_param_list * plist)
{
gx_device_printer *const pwdev = (gx_device_printer *) pdev;
gx_device_printer *const prdev = pwdev->async_renderer;
if (!pwdev->is_open || !prdev)
return (dev_proc(pwdev, get_hardware_params))(pdev, plist);
else {
gx_page_queue_wait_until_empty(pwdev->page_queue);
return (dev_proc(prdev, get_hardware_params))
((gx_device *) prdev, plist);
}
}
private int
gdev_prn_async_render_put_params(gx_device * pdev, gs_param_list * plist)
{
gx_device_printer *const prdev = (gx_device_printer *) pdev;
bool save_is_open = prdev->is_open;
(*prdev->orig_procs.put_params) (pdev, plist);
if (!prdev->is_open && save_is_open) {
int code;
if (prdev->printer_procs.open_render_device ==
gx_default_open_render_device)
code = gdev_prn_async_render_open(prdev);
else
code = (*prdev->printer_procs.open_render_device) (prdev);
reinit_printer_into_renderer(prdev);
if (code >= 0)
;
else
return code;
}
return 0;
}
private int
gdev_prn_async_write_output_page(gx_device * pdev, int num_copies, int flush)
{
gx_device_printer *const pwdev = (gx_device_printer *) pdev;
gx_device_clist_writer *const pcwdev =
&((gx_device_clist *) pdev)->writer;
int flush_code;
int add_code;
int open_code;
int one_last_time = 1;
flush_code = clist_end_page(pcwdev);
add_code = gx_page_queue_add_page(pwdev->page_queue,
(flush ? GX_PAGE_QUEUE_ACTION_FULL_PAGE :
GX_PAGE_QUEUE_ACTION_COPY_PAGE),
&pcwdev->page_info, num_copies);
if (flush && (flush_code >= 0) && (add_code >= 0)) {
gx_finish_output_page(pdev, num_copies, flush);
}
while ((open_code = (*gs_clist_device_procs.open_device)
((gx_device *) pdev)) == gs_error_VMerror) {
if (!gx_page_queue_wait_one_page(pwdev->page_queue)
&& one_last_time-- <= 0)
break;
}
return
(flush_code < 0 ? flush_code : open_code < 0 ? open_code :
add_code < 0 ? add_code : 0);
}
private int
gdev_prn_async_write_free_up_bandlist_memory(gx_device * pdev, bool flush_current)
{
gx_device_printer *const pwdev = (gx_device_printer *) pdev;
if (flush_current) {
int code = flush_page(pwdev, true);
if (code < 0)
return code;
}
return gx_page_queue_wait_one_page(pwdev->page_queue);
}
private int
flush_page(
gx_device_printer * pwdev,
bool partial
)
{
gx_device_clist *const pcldev = (gx_device_clist *) pwdev;
gx_device_clist_writer *const pcwdev = &pcldev->writer;
int flush_code = 0;
int add_code = 0;
flush_code = clist_end_page(pcwdev);
add_code = gx_page_queue_add_page(pwdev->page_queue,
(partial ? GX_PAGE_QUEUE_ACTION_PARTIAL_PAGE :
GX_PAGE_QUEUE_ACTION_FULL_PAGE),
&pcwdev->page_info, 0);
pcwdev->page_info.bfile = pcwdev->page_info.cfile = 0;
if (flush_code < 0)
return flush_code;
return add_code;
}
private int
reopen_clist_after_flush(
gx_device_printer * pwdev
)
{
int open_code;
int one_last_time = 1;
while ((open_code = (*gs_clist_device_procs.open_device)
((gx_device *) pwdev)) == gs_error_VMerror) {
if (!gx_page_queue_wait_one_page(pwdev->page_queue)
&& one_last_time-- <= 0)
break;
}
return open_code;
}
private int
alloc_bandlist_memory(gs_memory_t ** final_allocator,
gs_memory_t * base_allocator)
{
gs_memory_t *data_allocator = 0;
gs_memory_locked_t *locked_allocator = 0;
int code = 0;
#if defined(DEBUG) && defined(DebugBandlistMemorySize)
code = alloc_render_memory(&data_allocator, base_allocator,
DebugBandlistMemorySize);
if (code < 0)
return code;
#else
data_allocator = (gs_memory_t *)gs_malloc_memory_init();
if (!data_allocator)
return_error(gs_error_VMerror);
#endif
locked_allocator = (gs_memory_locked_t *)
gs_alloc_bytes_immovable(data_allocator, sizeof(gs_memory_locked_t),
"alloc_bandlist_memory(locked allocator)");
if (!locked_allocator)
goto alloc_err;
code = gs_memory_locked_init(locked_allocator, data_allocator);
if (code < 0)
goto alloc_err;
*final_allocator = (gs_memory_t *)locked_allocator;
return 0;
alloc_err:
if (locked_allocator)
free_bandlist_memory((gs_memory_t *)locked_allocator);
else if (data_allocator)
gs_memory_free_all(data_allocator, FREE_ALL_EVERYTHING,
"alloc_bandlist_memory(data allocator)");
return (code < 0 ? code : gs_note_error(gs_error_VMerror));
}
private void
free_bandlist_memory(gs_memory_t *bandlist_allocator)
{
gs_memory_locked_t *const lmem = (gs_memory_locked_t *)bandlist_allocator;
gs_memory_t *data_mem = gs_memory_locked_target(lmem);
gs_memory_free_all(bandlist_allocator,
FREE_ALL_STRUCTURES | FREE_ALL_ALLOCATOR,
"free_bandlist_memory(locked allocator)");
if (data_mem)
gs_memory_free_all(data_mem, FREE_ALL_EVERYTHING,
"free_bandlist_memory(data allocator)");
}
private int
alloc_render_memory(gs_memory_t **final_allocator,
gs_memory_t *base_allocator, long space)
{
gs_ref_memory_t *rmem =
ialloc_alloc_state((gs_memory_t *)base_allocator, space);
vm_spaces spaces;
int i, code;
if (rmem == 0)
return_error(gs_error_VMerror);
code = ialloc_add_chunk(rmem, space, "alloc_render_memory");
if (code < 0) {
gs_memory_free_all((gs_memory_t *)rmem, FREE_ALL_EVERYTHING,
"alloc_render_memory");
return code;
}
*final_allocator = (gs_memory_t *)rmem;
for ( i = 0; i < countof(spaces_indexed); ++i )
spaces_indexed[i] = 0;
space_local = space_global = (gs_ref_memory_t *)rmem;
spaces.vm_reclaim = gs_nogc_reclaim;
GS_RECLAIM(&spaces, false);
return 0;
}
private void
free_render_memory(gs_memory_t *render_allocator)
{
if (render_allocator)
gs_memory_free_all(render_allocator, FREE_ALL_EVERYTHING,
"free_render_memory");
}