#ifndef GSLIBCTX_H
#define GSLIBCTX_H
#include "std.h"
#include "stdio_.h"
#include "gs_dll_call.h"
typedef struct name_table_s *name_table_ptr;
typedef struct gs_lib_ctx_s
{
gs_memory_t *memory;
FILE *fstdin;
FILE *fstdout;
FILE *fstderr;
FILE *fstdout2;
bool stdout_is_redirected;
bool stdout_to_stderr;
bool stdin_is_interactive;
void *caller_handle;
int (GSDLLCALL *stdin_fn)(void *caller_handle, char *buf, int len);
int (GSDLLCALL *stdout_fn)(void *caller_handle, const char *str, int len);
int (GSDLLCALL *stderr_fn)(void *caller_handle, const char *str, int len);
int (GSDLLCALL *poll_fn)(void *caller_handle);
ulong gs_next_id;
void *top_of_system;
name_table_ptr gs_name_table;
bool dict_auto_expand;
} gs_lib_ctx_t;
int gs_lib_ctx_init( gs_memory_t *mem );
void *gs_lib_ctx_get_interp_instance( gs_memory_t *mem );
const gs_memory_t * gs_lib_ctx_get_non_gc_memory_t(void);
#endif