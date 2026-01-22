#include "stdio_.h"
#include "memory_.h"
#include "gdebug.h"
#include "gscdefs.h"
#include "gsmemory.h"
#include "gsmalloc.h"
#include "gp.h"
#include "gslib.h"
extern_gx_init_table();
int
gs_lib_init(FILE * debug_out)
{
return gs_lib_init1(gs_lib_init0(debug_out));
}
gs_memory_t *
gs_lib_init0(FILE * debug_out)
{
gs_memory_t *mem;
mem = (gs_memory_t *) gs_malloc_init(NULL);
memset(gs_debug, 0, 128);
gs_log_errors = 0;
return mem;
}
int
gs_lib_init1(gs_memory_t * mem)
{
init_proc((*const *ipp));
int code;
for (ipp = gx_init_table; *ipp != 0; ++ipp)
if ((code = (**ipp)(mem)) < 0)
return code;
return 0;
}
void
gs_lib_finit(int exit_status, int code, gs_memory_t *mem)
{
gp_exit(exit_status, code);
}