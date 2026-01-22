#ifndef iinit_INCLUDED
#  define iinit_INCLUDED
int obj_init(i_ctx_t **, gs_dual_memory_t *);
int zop_init(i_ctx_t *);
int op_init(i_ctx_t *);
bool gs_have_level2(void);
#endif