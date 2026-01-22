#ifndef gxpageq_INCLUDED
# define gxpageq_INCLUDED
#include "gsmemory.h"
#include "gxband.h"
#include "gxsync.h"
typedef enum {
GX_PAGE_QUEUE_ACTION_PARTIAL_PAGE,
GX_PAGE_QUEUE_ACTION_FULL_PAGE,
GX_PAGE_QUEUE_ACTION_COPY_PAGE,
GX_PAGE_QUEUE_ACTION_TERMINATE
} gx_page_queue_action_t;
#ifndef gx_page_queue_DEFINED
# define gx_page_queue_DEFINED
typedef struct gx_page_queue_s gx_page_queue_t;
#endif
typedef struct gx_page_queue_entry_s gx_page_queue_entry_t;
struct gx_page_queue_entry_s {
gx_band_page_info_t page_info;
gx_page_queue_action_t action;
int num_copies;
gx_page_queue_entry_t *next;
gx_page_queue_t *queue;
};
#define private_st_gx_page_queue_entry()\
gs_private_st_ptrs2(st_gx_page_queue_entry, gx_page_queue_entry_t,\
"gx_page_queue_entry",\
gx_page_queue_entry_enum_ptrs, gx_page_queue_entry_reloc_ptrs,\
next, queue)
gx_page_queue_t *gx_page_queue_alloc(gs_memory_t *mem);
gx_page_queue_entry_t *
gx_page_queue_entry_alloc(
gx_page_queue_t * queue
);
void gx_page_queue_entry_free(
gx_page_queue_entry_t * entry
);
void gx_page_queue_entry_free_page_info(
gx_page_queue_entry_t * entry
);
int gx_page_queue_init(
gx_page_queue_t * queue,
gs_memory_t * memory
);
void gx_page_queue_dnit(
gx_page_queue_t * queue
);
int gx_page_queue_wait_one_page(
gx_page_queue_t * queue
);
void gx_page_queue_wait_until_empty(
gx_page_queue_t * queue
);
void gx_page_queue_enqueue(
gx_page_queue_entry_t * entry
);
int gx_page_queue_add_page(
gx_page_queue_t * queue,
gx_page_queue_action_t action,
const gx_band_page_info_t * page_info,
int page_count
);
gx_page_queue_entry_t *
gx_page_queue_start_dequeue(
gx_page_queue_t * queue
);
void gx_page_queue_finish_dequeue(
gx_page_queue_entry_t * entry
);
#endif