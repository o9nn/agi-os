#ifndef gsexit_INCLUDED
#  define gsexit_INCLUDED
int gs_to_exit(const gs_memory_t *mem, int exit_status);
int gs_to_exit_with_code(const gs_memory_t *mem, int exit_status, int code);
void gs_abort(const gs_memory_t *mem);
#endif