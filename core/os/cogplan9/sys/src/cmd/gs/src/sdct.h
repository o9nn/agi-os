#ifndef sdct_INCLUDED
# define sdct_INCLUDED
#include <setjmp.h>
typedef struct jpeg_block_s jpeg_block_t;
struct jpeg_block_s {
jpeg_block_t *next;
void *data;
};
#define private_st_jpeg_block() \
gs_private_st_ptrs2(st_jpeg_block, jpeg_block_t, "jpeg_block_t",\
jpeg_block_enum_ptrs, jpeg_block_reloc_ptrs, next, data)
#define jpeg_stream_data_common\
\
\
stream_template template;\
struct jpeg_error_mgr err;\
jmp_buf exit_jmpbuf;\
gs_memory_t *memory; \
jpeg_block_t *blocks; \
\
int Picky; \
int Relax
typedef struct jpeg_stream_data_s {
jpeg_stream_data_common;
} jpeg_stream_data;
#define jpeg_stream_data_common_init(pdata)\
BEGIN\
(pdata)->Picky = 0;\
(pdata)->Relax = 0;\
(pdata)->blocks = 0;\
END
typedef struct jpeg_compress_data_s {
jpeg_stream_data_common;
struct jpeg_compress_struct cinfo;
struct jpeg_destination_mgr destination;
byte finish_compress_buf[100];
int fcb_size, fcb_pos;
} jpeg_compress_data;
extern_st(st_jpeg_compress_data);
#define public_st_jpeg_compress_data() \
gs_public_st_ptrs1(st_jpeg_compress_data, jpeg_compress_data,\
"JPEG compress data", jpeg_compress_data_enum_ptrs, jpeg_compress_data_reloc_ptrs, blocks)
typedef struct jpeg_decompress_data_s {
jpeg_stream_data_common;
struct jpeg_decompress_struct dinfo;
struct jpeg_source_mgr source;
long skip;
bool input_eod;
bool faked_eoi;
byte *scanline_buffer;
uint bytes_in_scanline;
} jpeg_decompress_data;
#define private_st_jpeg_decompress_data() \
gs_private_st_ptrs2(st_jpeg_decompress_data, jpeg_decompress_data,\
"JPEG decompress data", jpeg_decompress_data_enum_ptrs,\
jpeg_decompress_data_reloc_ptrs, blocks, scanline_buffer)
typedef struct stream_DCT_state_s {
stream_state_common;
gs_const_string Markers;
float QFactor;
int ColorTransform;
bool NoMarker;
gs_memory_t *jpeg_memory;
union _jd {
jpeg_stream_data *common;
jpeg_compress_data *compress;
jpeg_decompress_data *decompress;
} data;
uint scan_line_size;
int phase;
} stream_DCT_state;
extern_st(st_DCT_state);
#define public_st_DCT_state() \
gs_public_st_const_strings1_ptrs1(st_DCT_state, stream_DCT_state,\
"DCTEncode/Decode state", dct_enum_ptrs, dct_reloc_ptrs, Markers, data.common)
extern const stream_template s_DCTD_template;
extern const stream_template s_DCTE_template;
void s_DCT_set_defaults(stream_state * st);
#endif