#ifndef gsargs_INCLUDED
#  define gsargs_INCLUDED
#define arg_str_max 2048
#define arg_depth_max 10
typedef struct arg_source_s {
bool is_file;
union _u {
struct _su {
char *chars;
gs_memory_t *memory;
const char *str;
} s;
FILE *file;
} u;
} arg_source;
typedef struct arg_list_s {
bool expand_ats;
FILE *(*arg_fopen) (const char *fname, void *fopen_data);
void *fopen_data;
const char **argp;
int argn;
int depth;
char cstr[arg_str_max + 1];
arg_source sources[arg_depth_max];
} arg_list;
void arg_init(arg_list * pal, const char **argv, int argc,
FILE * (*arg_fopen) (const char *fname, void *fopen_data),
void *fopen_data);
int arg_push_memory_string(arg_list * pal, char *str, gs_memory_t * mem);
#define arg_push_string(pal, str)\
arg_push_memory_string(pal, str, (gs_memory_t *)0);
void arg_finit(arg_list * pal);
const char *arg_next(arg_list * pal, int *code);
char *arg_copy(const char *str, gs_memory_t * mem);
#endif