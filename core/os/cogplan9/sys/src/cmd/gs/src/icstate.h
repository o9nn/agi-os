#ifndef icstate_INCLUDED
#  define icstate_INCLUDED
#include "imemory.h"
#include "iref.h"
#include "idsdata.h"
#include "iesdata.h"
#include "iosdata.h"
#ifndef gs_context_state_t_DEFINED
#  define gs_context_state_t_DEFINED
typedef struct gs_context_state_s gs_context_state_t;
#endif
#ifndef gs_file_path_ptr_DEFINED
#  define gs_file_path_ptr_DEFINED
typedef struct gs_file_path_s *gs_file_path_ptr;
#endif
struct gs_context_state_s {
gs_state *pgs;
gs_dual_memory_t memory;
int language_level;
ref array_packing;
ref binary_object_format;
long rand_state;
long usertime_total;
bool keep_usertime;
int in_superexec;
ref userparams;
int scanner_options;
bool LockFilePermissions;
bool starting_arg_file;
gs_file_path_ptr lib_path;
ref stdio[3];
dict_stack_t dict_stack;
exec_stack_t exec_stack;
op_stack_t op_stack;
struct i_plugin_holder_s *plugin_list;
};
extern const long rand_state_initial;
#define public_st_context_state()	\
gs_public_st_complex_only(st_context_state, gs_context_state_t,\
"gs_context_state_t", context_state_clear_marks,\
context_state_enum_ptrs, context_state_reloc_ptrs, 0)
#endif