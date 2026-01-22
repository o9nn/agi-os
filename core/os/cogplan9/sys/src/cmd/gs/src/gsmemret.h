#if !defined(gsmemret_INCLUDED)
# define gsmemret_INCLUDED
#include "gsmemory.h"
typedef struct gs_memory_retrying_s gs_memory_retrying_t;
typedef enum {
RECOVER_STATUS_NO_RETRY,
RECOVER_STATUS_RETRY_OK
} gs_memory_recover_status_t;
typedef gs_memory_recover_status_t (*gs_memory_recover_proc_t)
(gs_memory_retrying_t *rmem, void *proc_data);
struct gs_memory_retrying_s {
gs_memory_common;
gs_memory_t *target;
gs_memory_recover_proc_t recover_proc;
void *recover_proc_data;
};
int gs_memory_retrying_init(
gs_memory_retrying_t * rmem,
gs_memory_t * target
);
void gs_memory_retrying_release(gs_memory_retrying_t *rmem);
void gs_memory_retrying_set_recover(gs_memory_retrying_t *rmem,
gs_memory_recover_proc_t recover_proc,
void *recover_proc_data);
gs_memory_t * gs_memory_retrying_target(const gs_memory_retrying_t *rmem);
#endif