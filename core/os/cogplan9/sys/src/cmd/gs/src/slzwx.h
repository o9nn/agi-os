#ifndef slzwx_INCLUDED
# define slzwx_INCLUDED
typedef struct lzw_decode_s lzw_decode;
typedef struct lzw_encode_table_s lzw_encode_table;
typedef struct stream_LZW_state_s {
stream_state_common;
int InitialCodeLength;
bool FirstBitLowOrder;
bool BlockData;
int EarlyChange;
uint bits;
int bits_left;
int bytes_left;
union _lzt {
lzw_decode *decode;
lzw_encode_table *encode;
} table;
uint next_code;
int code_size;
int prev_code;
uint prev_len;
int copy_code;
uint copy_len;
int copy_left;
bool first;
} stream_LZW_state;
extern_st(st_LZW_state);
#define public_st_LZW_state() \
gs_public_st_ptrs1(st_LZW_state, stream_LZW_state,\
"LZWDecode state", lzwd_enum_ptrs, lzwd_reloc_ptrs, table.decode)
#define s_LZW_set_defaults_inline(ss)\
((ss)->InitialCodeLength = 8,\
(ss)->FirstBitLowOrder = false,\
(ss)->BlockData = false,\
(ss)->EarlyChange = 1,\
\
(ss)->table.decode = 0)
extern const stream_template s_LZWD_template;
extern const stream_template s_LZWE_template;
void s_LZW_set_defaults(stream_state *);
void s_LZW_release(stream_state *);
#endif