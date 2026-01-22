#ifndef gxclmem_INCLUDED
# define gxclmem_INCLUDED
#include "gxclio.h"
#include "strimpl.h"
#define MEMFILE_DATA_SIZE (16384 - 160)
typedef struct RAW_BUFFER {
struct RAW_BUFFER *fwd, *back;
struct LOG_MEMFILE_BLK *log_blk;
char data[MEMFILE_DATA_SIZE];
} RAW_BUFFER;
typedef struct PHYS_MEMFILE_BLK {
struct PHYS_MEMFILE_BLK *link;
char *data_limit;
char data_spare[4];
char data[MEMFILE_DATA_SIZE];
} PHYS_MEMFILE_BLK;
typedef struct LOG_MEMFILE_BLK {
struct LOG_MEMFILE_BLK *link;
PHYS_MEMFILE_BLK *phys_blk;
char *phys_pdata;
RAW_BUFFER *raw_block;
} LOG_MEMFILE_BLK;
typedef struct MEMFILE {
gs_memory_t *memory;
gs_memory_t *data_memory;
bool ok_to_compress;
PHYS_MEMFILE_BLK *reservePhysBlockChain;
int reservePhysBlockCount;
LOG_MEMFILE_BLK *reserveLogBlockChain;
int reserveLogBlockCount;
LOG_MEMFILE_BLK *log_head;
LOG_MEMFILE_BLK *log_curr_blk;
long log_length;
long log_curr_pos;
char *pdata;
char *pdata_end;
long total_space;
PHYS_MEMFILE_BLK *phys_curr;
RAW_BUFFER *raw_head, *raw_tail;
int error_code;
stream_cursor_read rd;
stream_cursor_write wt;
bool compressor_initialized;
stream_state *compress_state;
stream_state *decompress_state;
} MEMFILE;
#define private_st_MEMFILE() \
gs_private_st_ptrs2(st_MEMFILE, MEMFILE, "MEMFILE",\
MEMFILE_enum_ptrs, MEMFILE_reloc_ptrs, compress_state, decompress_state)
#define memfile_fopen(fname, fmode, pcf, mem, data_mem, compress)\
clist_fopen(fname, fmode, pcf, mem, data_mem, compress)
#define memfile_fclose(cf, fname, delete)\
clist_fclose(cf, fname, delete)
#define memfile_unlink(fname)\
clist_unlink(fname)
#define memfile_space_available(req)\
clist_space_available(req)
#define memfile_fwrite_chars(data, len, cf)\
clist_fwrite_chars(data, len, cf)
#define memfile_fread_chars(data, len, cf)\
clist_fread_chars(data, len, cf)
#define memfile_set_memory_warning(cf, nbytes) clist_set_memory_warning(cf, nbytes)
#define memfile_ferror_code(cf) clist_ferror_code(cf)
#define memfile_ftell(cf) clist_ftell(cf)
#define memfile_rewind(cf, discard, fname) clist_rewind(cf, discard, fname)
#define memfile_fseek(cf, offset, mode, fname) clist_fseek(cf, offset, mode, fname)
const stream_state *clist_compressor_state(void *);
const stream_state *clist_decompressor_state(void *);
#endif