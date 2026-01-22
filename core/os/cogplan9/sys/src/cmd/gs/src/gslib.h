#ifndef gslib_INCLUDED
#  define gslib_INCLUDED
int gs_lib_init(FILE * debug_out);
gs_memory_t *gs_lib_init0(FILE * debug_out);
int gs_lib_init1(gs_memory_t *);
void gs_lib_finit(int exit_status, int code, gs_memory_t *);
#endif