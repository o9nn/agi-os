#ifndef ifilter_INCLUDED
#  define ifilter_INCLUDED
#include "istream.h"
#include "ivmspace.h"
int filter_read(
i_ctx_t *i_ctx_p,
int npop,
const stream_template * template,
stream_state * st,
uint space
);
int filter_write(i_ctx_t *i_ctx_p, int npop,
const stream_template * template,
stream_state * st, uint space);
int filter_read_simple(i_ctx_t *i_ctx_p,
const stream_template * template);
int filter_write_simple(i_ctx_t *i_ctx_p,
const stream_template * template);
void filter_mark_temp(const ref * fop, int is_temp);
void filter_mark_strm_temp(const ref * fop, int is_temp);
stream_proc_report_error(filter_report_error);
typedef struct stream_proc_state_s {
stream_state_common;
bool eof;
uint index;
ref proc;
ref data;
} stream_proc_state;
#define private_st_stream_proc_state() \
gs_private_st_complex_only(st_sproc_state, stream_proc_state,\
"procedure stream state", sproc_clear_marks, sproc_enum_ptrs, sproc_reloc_ptrs, 0)
bool s_is_proc(const stream *s);
#endif