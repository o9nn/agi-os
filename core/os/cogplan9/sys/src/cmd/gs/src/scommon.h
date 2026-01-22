#ifndef scommon_INCLUDED
#  define scommon_INCLUDED
#include "gsmemory.h"
#include "gstypes.h"
#include "gsstype.h"
#ifndef stream_DEFINED
#  define stream_DEFINED
typedef struct stream_s stream;
#endif
typedef struct stream_state_s stream_state;
typedef struct stream_template_s stream_template;
#define EOFC ((int)(-1))
#define ERRC ((int)(-2))
#define INTC ((int)(-3))
#define CALLC ((int)(-4))
#define max_stream_exception 4
#define stream_exception_repeat(x) x, x, x, x
typedef struct stream_cursor_read_s {
const byte *ptr;
const byte *limit;
byte *_skip;
} stream_cursor_read;
typedef struct stream_cursor_write_s {
const byte *_skip;
byte *ptr;
byte *limit;
} stream_cursor_write;
typedef union stream_cursor_s {
stream_cursor_read r;
stream_cursor_write w;
} stream_cursor;
#define stream_proc_init(proc)\
int proc(stream_state *)
#define stream_proc_process(proc)\
int proc(stream_state *, stream_cursor_read *,\
stream_cursor_write *, bool)
#define stream_proc_release(proc)\
void proc(stream_state *)
#define stream_proc_set_defaults(proc)\
void proc(stream_state *)
#define stream_proc_reinit(proc)\
int proc(stream_state *)
#define stream_proc_report_error(proc)\
int proc(stream_state *, const char *)
stream_proc_report_error(s_no_report_error);
#define stream_state_proc_get_params(proc, state_type)\
int proc(gs_param_list *plist, const state_type *ss, bool all)
#define stream_state_proc_put_params(proc, state_type)\
int proc(gs_param_list *plist, state_type *ss)
#define STREAM_MAX_ERROR_STRING 79
#define stream_state_common\
const stream_template *template;\
gs_memory_t *memory;\
stream_proc_report_error((*report_error));\
int min_left;  \
char error_string[STREAM_MAX_ERROR_STRING + 1]
struct stream_state_s {
stream_state_common;
};
extern_st(st_stream_state);
#define public_st_stream_state() \
gs_public_st_simple(st_stream_state, stream_state, "stream_state")
#endif