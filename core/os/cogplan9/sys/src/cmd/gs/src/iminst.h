#ifndef iminst_INCLUDED
# define iminst_INCLUDED
#ifndef gs_main_instance_DEFINED
# define gs_main_instance_DEFINED
typedef struct gs_main_instance_s gs_main_instance;
#endif
typedef struct gs_file_path_s {
ref container;
ref list;
const char *env;
const char *final;
uint count;
} gs_file_path;
#define STDIN_BUF_SIZE 128
#define STDOUT_BUF_SIZE 128
#define STDERR_BUF_SIZE 128
struct gs_main_instance_s {
gs_memory_t *heap;
uint memory_chunk_size;
ulong name_table_size;
uint run_buffer_size;
int init_done;
int user_errors;
bool search_here_first;
bool run_start;
gs_file_path lib_path;
long base_time[2];
void *readline_data;
char stdin_buf[STDIN_BUF_SIZE];
char stdout_buf[STDOUT_BUF_SIZE];
char stderr_buf[STDERR_BUF_SIZE];
ref error_object;
#if 1
display_callback *display;
#endif
i_ctx_t *i_ctx_p;
};
#define gs_main_instance_default_init_values\
0, 20000, 0, 0, -1, 0, SEARCH_HERE_FIRST, 1
extern const gs_main_instance gs_main_instance_init_values;
#endif