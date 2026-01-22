#ifndef gxstate_INCLUDED
# define gxstate_INCLUDED
#ifndef gs_state_DEFINED
# define gs_state_DEFINED
typedef struct gs_state_s gs_state;
#endif
#include "gscspace.h"
gs_memory_t *gs_state_memory(const gs_state *);
gs_state *gs_state_saved(const gs_state *);
gs_state *gs_state_swap_saved(gs_state *, gs_state *);
gs_memory_t *gs_state_swap_memory(gs_state *, gs_memory_t *);
typedef void *(*gs_state_alloc_proc_t) (gs_memory_t * mem);
typedef int (*gs_state_copy_proc_t) (void *to, const void *from);
typedef void (*gs_state_free_proc_t) (void *old, gs_memory_t * mem);
typedef enum {
copy_for_gsave,
copy_for_grestore,
copy_for_gstate,
copy_for_setgstate,
copy_for_copygstate,
copy_for_currentgstate
} gs_state_copy_reason_t;
typedef int (*gs_state_copy_for_proc_t) (void *to, void *from,
gs_state_copy_reason_t reason);
typedef struct gs_state_client_procs_s {
gs_state_alloc_proc_t alloc;
gs_state_copy_proc_t copy;
gs_state_free_proc_t free;
gs_state_copy_for_proc_t copy_for;
} gs_state_client_procs;
void gs_state_set_client(gs_state *, void *, const gs_state_client_procs *,
bool client_has_pattern_streams);
#ifndef gs_state_client_data
void *gs_state_client_data(const gs_state *);
#endif
gs_id gx_get_clip_path_id(gs_state *);
#endif