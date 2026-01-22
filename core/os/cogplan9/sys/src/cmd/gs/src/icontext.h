#ifndef icontext_INCLUDED
#  define icontext_INCLUDED
#include "gsstype.h"
#include "icstate.h"
extern_st(st_context_state);
extern int set_user_params(i_ctx_t *i_ctx_p, const ref * paramdict);
int context_state_alloc(gs_context_state_t ** ppcst,
const ref *psystem_dict,
const gs_dual_memory_t * dmem);
int context_state_load(gs_context_state_t *);
int context_state_store(gs_context_state_t *);
int context_state_free(gs_context_state_t *);
#endif