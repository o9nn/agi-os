#ifndef strimpl_INCLUDED
# define strimpl_INCLUDED
#include "scommon.h"
#include "gstypes.h"
#include "gsstruct.h"
struct stream_template_s {
gs_memory_type_ptr_t stype;
stream_proc_init((*init));
stream_proc_process((*process));
uint min_in_size;
uint min_out_size;
stream_proc_release((*release));
stream_proc_set_defaults((*set_defaults));
stream_proc_reinit((*reinit));
};
int stream_move(stream_cursor_read *, stream_cursor_write *);
typedef enum {
hex_ignore_garbage = 0,
hex_ignore_whitespace = 1,
hex_ignore_leading_whitespace = 2
} hex_syntax;
int s_hex_process(stream_cursor_read *, stream_cursor_write *, int *, hex_syntax);
#endif