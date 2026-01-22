#ifndef iscan_INCLUDED
# define iscan_INCLUDED
#include "sa85x.h"
#include "sstring.h"
#ifndef scanner_state_DEFINED
# define scanner_state_DEFINED
typedef struct scanner_state_s scanner_state;
#endif
#define max_comment_line 255
#define max_dsc_line max_comment_line
#define da_buf_size (max_comment_line + 2)
typedef struct dynamic_area_s {
byte *base;
byte *next;
byte *limit;
bool is_dynamic;
byte buf[da_buf_size];
gs_memory_t *memory;
} dynamic_area;
#define da_size(pda) ((uint)((pda)->limit - (pda)->base))
typedef dynamic_area *da_ptr;
typedef struct scan_binary_state_s {
int num_format;
int (*cont)(i_ctx_t *, stream *, ref *, scanner_state *);
ref bin_array;
uint index;
uint max_array_index;
uint min_string_index;
uint top_size;
uint size;
} scan_binary_state;
struct scanner_state_s {
uint s_pstack;
uint s_pdepth;
int s_options;
enum {
scanning_none,
scanning_binary,
scanning_comment,
scanning_name,
scanning_string
} s_scan_type;
dynamic_area s_da;
union sss_ {
scan_binary_state binary;
struct sns_ {
int s_name_type;
bool s_try_number;
} s_name;
stream_state st;
stream_A85D_state a85d;
stream_AXD_state axd;
stream_PSSD_state pssd;
} s_ss;
};
extern_st(st_scanner_state);
#define public_st_scanner_state() \
gs_public_st_complex_only(st_scanner_state, scanner_state, "scanner state",\
scanner_clear_marks, scanner_enum_ptrs, scanner_reloc_ptrs, 0)
#define SCAN_FROM_STRING 1
#define SCAN_CHECK_ONLY 2
#define SCAN_PROCESS_COMMENTS 4
#define SCAN_PROCESS_DSC_COMMENTS 8
#define SCAN_PDF_RULES 16
#define SCAN_PDF_INV_NUM 32
void scanner_state_init_options(scanner_state *sstate, int options);
#define scanner_state_init_check(pstate, from_string, check_only)\
scanner_state_init_options(pstate,\
(from_string ? SCAN_FROM_STRING : 0) |\
(check_only ? SCAN_CHECK_ONLY : 0))
#define scanner_state_init(pstate, from_string)\
scanner_state_init_check(pstate, from_string, false)
#define scan_BOS 1
#define scan_EOF 2
#define scan_Refill 3
#define scan_Comment 4
#define scan_DSC_Comment 5
int scan_token(i_ctx_t *i_ctx_p, stream * s, ref * pref,
scanner_state * pstate);
int scan_string_token_options(i_ctx_t *i_ctx_p, ref * pstr, ref * pref,
int options);
#define scan_string_token(i_ctx_p, pstr, pref)\
scan_string_token_options(i_ctx_p, pstr, pref, 0)
int scan_handle_refill(i_ctx_t *i_ctx_p, const ref * fop,
scanner_state * pstate, bool save, bool push_file,
op_proc_t cont);
extern int (*scan_dsc_proc) (const byte *, uint);
extern int (*scan_comment_proc) (const byte *, uint);
#endif