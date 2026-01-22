#ifndef imain_INCLUDED
#  define imain_INCLUDED
#include "gsexit.h"
#ifndef gs_main_instance_DEFINED
#  define gs_main_instance_DEFINED
typedef struct gs_main_instance_s gs_main_instance;
#endif
gs_main_instance* get_minst_from_memory(const gs_memory_t *mem);
gs_main_instance *gs_main_alloc_instance(gs_memory_t *);
int gs_main_init0(gs_main_instance *minst, FILE *in, FILE *out, FILE *err,
int max_lib_paths);
int gs_main_init1(gs_main_instance * minst);
int gs_main_init2(gs_main_instance * minst);
int gs_main_add_lib_path(gs_main_instance * minst, const char *path);
int gs_main_set_lib_paths(gs_main_instance * minst);
int gs_main_lib_open(gs_main_instance * minst, const char *fname,
ref * pfile);
int gs_main_run_file(gs_main_instance * minst, const char *fname,
int user_errors, int *pexit_code,
ref * perror_object);
int gs_main_run_string(gs_main_instance * minst, const char *str,
int user_errors, int *pexit_code,
ref * perror_object);
int gs_main_run_string_with_length(gs_main_instance * minst,
const char *str, uint length,
int user_errors, int *pexit_code,
ref * perror_object);
int gs_main_run_file_open(gs_main_instance * minst,
const char *file_name, ref * pfref);
int gs_main_run_string_begin(gs_main_instance * minst, int user_errors,
int *pexit_code, ref * perror_object);
int gs_main_run_string_continue(gs_main_instance * minst,
const char *str, uint length,
int user_errors, int *pexit_code,
ref * perror_object);
int gs_main_run_string_end(gs_main_instance * minst, int user_errors,
int *pexit_code, ref * perror_object);
int gs_push_boolean(gs_main_instance * minst, bool value);
int gs_push_integer(gs_main_instance * minst, long value);
int gs_push_real(gs_main_instance * minst, floatp value);
int gs_push_string(gs_main_instance * minst, byte * chars, uint length,
bool read_only);
int gs_pop_boolean(gs_main_instance * minst, bool * result);
int gs_pop_integer(gs_main_instance * minst, long *result);
int gs_pop_real(gs_main_instance * minst, float *result);
int gs_pop_string(gs_main_instance * minst, gs_string * result);
void gs_main_dump_stack(gs_main_instance *minst, int code,
ref * perror_object);
int gs_main_finit(gs_main_instance * minst, int exit_status, int code);
#endif