#ifndef gp_INCLUDED
#  define gp_INCLUDED
#include "gstypes.h"
#include "gpgetenv.h"
#include "srdline.h"
void gp_init(void);
void gp_exit(int exit_status, int code);
void gp_do_exit(int exit_status);
const char *gp_strerror(int);
void gp_get_realtime(long ptm[2]);
void gp_get_usertime(long ptm[2]);
int gp_readline_init(void **preadline_data, gs_memory_t *mem);
int gp_readline(stream *s_in, stream *s_out, void *readline_data,
gs_const_string *prompt, gs_string *buf,
gs_memory_t *bufmem, uint *pcount, bool *pin_eol,
bool (*is_stdin)(const stream *));
void gp_readline_finit(void *readline_data);
int gp_stdin_read(char *buf, int len, int interactive, FILE *f);
const char *gp_getenv_display(void);
#define gp_file_name_sizeof 260
extern const char gp_file_name_list_separator;
extern const char gp_scratch_file_name_prefix[];
extern const char gp_null_file_name[];
extern const char gp_current_directory_name[];
extern const char gp_fmode_binary_suffix[];
extern const char gp_fmode_rb[];
extern const char gp_fmode_wb[];
FILE *gp_open_scratch_file(const char *prefix,
char fname[gp_file_name_sizeof],
const char *mode);
FILE *gp_fopen(const char *fname, const char *mode);
int gp_setmode_binary(FILE * pfile, bool mode);
typedef enum {
gp_combine_small_buffer = -1,
gp_combine_cant_handle = 0,
gp_combine_success = 1
} gp_file_name_combine_result;
gp_file_name_combine_result gp_file_name_combine(const char *prefix, uint plen,
const char *fname, uint flen, bool no_sibling, char *buffer, uint *blen);
uint gp_file_name_root(const char *fname, uint len);
uint gs_file_name_check_separator(const char *fname, int len, const char *item);
bool gp_file_name_is_parent(const char *fname, uint len);
bool gp_file_name_is_current(const char *fname, uint len);
const char *gp_file_name_current(void);
const char *gp_file_name_separator(void);
const char *gp_file_name_directory_separator(void);
const char *gp_file_name_parent(void);
bool gp_file_name_is_partent_allowed(void);
bool gp_file_name_is_empty_item_meanful(void);
int gp_read_macresource(byte *buf, const char *fname,
const uint type, const ushort id);
int gp_cache_insert(int type, byte *key, int keylen, void *buffer, int buflen);
typedef void *(*gp_cache_alloc)(void *userdata, int bytes);
int gp_cache_query(int type, byte* key, int keylen, void **buffer,
gp_cache_alloc alloc, void *userdata);
#define GP_CACHE_TYPE_TEST 0
#define GP_CACHE_TYPE_FONTMAP 1
#define GP_CACHE_TYPE_WTS 2
FILE *gp_open_printer(char fname[gp_file_name_sizeof], int binary_mode);
void gp_close_printer(FILE * pfile, const char *fname);
#ifndef file_enum_DEFINED
#  define file_enum_DEFINED
typedef struct file_enum_s file_enum;
#endif
file_enum *gp_enumerate_files_init(const char *pat, uint patlen,
gs_memory_t * memory);
uint gp_enumerate_files_next(file_enum * pfen, char *ptr, uint maxlen);
void gp_enumerate_files_close(file_enum * pfen);
void *gp_enumerate_fonts_init(gs_memory_t *mem);
int gp_enumerate_fonts_next(void *enum_state, char **fontname, char **path);
void gp_enumerate_fonts_free(void *enum_state);
#endif