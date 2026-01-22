#ifndef isave_INCLUDED
# define isave_INCLUDED
#include "idosave.h"
#ifndef alloc_save_t_DEFINED
typedef struct alloc_save_s alloc_save_t;
# define alloc_save_t_DEFINED
#endif
extern void alloc_save_init(gs_dual_memory_t *);
alloc_save_t *alloc_find_save(const gs_dual_memory_t *, ulong);
ulong alloc_save_state(gs_dual_memory_t *, void *);
void *alloc_save_client_data(const alloc_save_t *);
ulong alloc_save_current_id(const gs_dual_memory_t *);
alloc_save_t *alloc_save_current(const gs_dual_memory_t *);
bool alloc_is_since_save(const void *, const alloc_save_t *);
bool alloc_name_is_since_save(const gs_memory_t *mem, const ref *, const alloc_save_t *);
bool alloc_name_index_is_since_save(const gs_memory_t *mem, uint, const alloc_save_t *);
bool alloc_any_names_since_save(const alloc_save_t *);
bool alloc_restore_step_in(gs_dual_memory_t *, alloc_save_t *);
#define alloc_restore_state_step(save) alloc_restore_step_in(idmemory, save)
void alloc_forget_save_in(gs_dual_memory_t *, alloc_save_t *);
#define alloc_forget_save(save) alloc_forget_save_in(idmemory, save)
void alloc_restore_all(gs_dual_memory_t *);
void alloc_set_in_save(gs_dual_memory_t *);
void alloc_set_not_in_save(gs_dual_memory_t *);
void font_restore(const alloc_save_t * save);
#endif