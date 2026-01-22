#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gxclmem.h"
private const long COMPRESSION_THRESHOLD = 300000;
#define NEED_TO_COMPRESS(f)\
((f)->ok_to_compress && (f)->total_space > COMPRESSION_THRESHOLD)
#define GET_NUM_RAW_BUFFERS( f ) 					\
max(f->log_length/MEMFILE_DATA_SIZE/32, 8)
#define MALLOC(f, siz, cname)\
(void *)gs_alloc_bytes((f)->data_memory, siz, cname)
#define FREE(f, obj, cname)\
(gs_free_object((f)->data_memory, obj, cname),\
(f)->total_space -= sizeof(*(obj)))
private_st_MEMFILE();
private void memfile_free_mem(MEMFILE * f);
private int memfile_init_empty(MEMFILE * f);
#ifdef DEBUG
long tot_compressed;
long tot_raw;
long tot_cache_miss;
long tot_cache_hits;
long tot_swap_out;
byte *decomp_wt_ptr0, *decomp_wt_limit0;
const byte *decomp_rd_ptr0, *decomp_rd_limit0;
byte *decomp_wt_ptr1, *decomp_wt_limit1;
const byte *decomp_rd_ptr1, *decomp_rd_limit1;
#endif
void *
allocateWithReserve(
MEMFILE  *f,
int      sizeofBlock,
int      *return_code,
const   char     *allocName,
const   char     *errorMessage
)
{
int code = 0;
void *block = MALLOC(f, sizeofBlock, allocName);
if (block == NULL) {
if (sizeofBlock == sizeof(LOG_MEMFILE_BLK)) {
if (f->reserveLogBlockCount > 0) {
block = f->reserveLogBlockChain;
f->reserveLogBlockChain = f->reserveLogBlockChain->link;
--f->reserveLogBlockCount;
}
} else if (sizeofBlock == sizeof(PHYS_MEMFILE_BLK) ||
sizeofBlock == sizeof(RAW_BUFFER)
) {
if (f->reservePhysBlockCount > 0) {
block = f->reservePhysBlockChain;
f->reservePhysBlockChain = f->reservePhysBlockChain->link;
--f->reservePhysBlockCount;
}
}
if (block != NULL)
code = 1;
}
if (block != NULL)
f->total_space += sizeofBlock;
else
code = gs_note_error(gs_error_VMerror);
*return_code = code;
return block;
}
int
memfile_fopen(char fname[gp_file_name_sizeof], const char *fmode,
clist_file_ptr   * pf,
gs_memory_t *mem, gs_memory_t *data_mem, bool ok_to_compress)
{
MEMFILE *f = 0;
int code = 0;
if (fname[0] != 0 || fmode[0] != 'w') {
code = gs_note_error(gs_error_invalidfileaccess);
goto finish;
}
fname[0] = (ok_to_compress ? 'a' : 'b');
fname[1] = 0;
f = gs_alloc_struct(mem, MEMFILE, &st_MEMFILE,
"memfile_open_scratch(MEMFILE)");
if (f == NULL) {
eprintf1("memfile_open_scratch(%s): gs_alloc_struct failed\n", fname);
code = gs_note_error(gs_error_VMerror);
goto finish;
}
f->memory = mem;
f->data_memory = data_mem;
f->compress_state = 0;
f->decompress_state = 0;
f->total_space = 0;
f->reservePhysBlockChain = NULL;
f->reservePhysBlockCount = 0;
f->reserveLogBlockChain = NULL;
f->reserveLogBlockCount = 0;
if ((code = memfile_init_empty(f)) < 0)
goto finish;
if ((code = memfile_set_memory_warning(f, 0)) < 0)
goto finish;
f->ok_to_compress =  true;
f->compress_state = 0;
f->decompress_state = 0;
if (f->ok_to_compress) {
const stream_state *compress_proto = clist_compressor_state(NULL);
const stream_state *decompress_proto = clist_decompressor_state(NULL);
const stream_template *compress_template = compress_proto->template;
const stream_template *decompress_template = decompress_proto->template;
f->compress_state =
gs_alloc_struct(mem, stream_state, compress_template->stype,
"memfile_open_scratch(compress_state)");
f->decompress_state =
gs_alloc_struct(mem, stream_state, decompress_template->stype,
"memfile_open_scratch(decompress_state)");
if (f->compress_state == 0 || f->decompress_state == 0) {
eprintf1("memfile_open_scratch(%s): gs_alloc_struct failed\n", fname);
code = gs_note_error(gs_error_VMerror);
goto finish;
}
memcpy(f->compress_state, compress_proto,
gs_struct_type_size(compress_template->stype));
f->compress_state->memory = mem;
memcpy(f->decompress_state, decompress_proto,
gs_struct_type_size(decompress_template->stype));
f->decompress_state->memory = mem;
if (compress_template->set_defaults)
(*compress_template->set_defaults) (f->compress_state);
if (decompress_template->set_defaults)
(*decompress_template->set_defaults) (f->decompress_state);
}
f->total_space = 0;
#ifdef DEBUG
if (*fname == 'a') {
tot_compressed = 0;
tot_raw = 0;
tot_cache_miss = 0;
tot_cache_hits = 0;
tot_swap_out = 0;
}
#endif
finish:
if (code < 0) {
if (f != NULL)
memfile_fclose((clist_file_ptr)f, fname, true);
} else {
*pf = f;
}
return code;
}
int
memfile_fclose(clist_file_ptr cf, const char *fname, bool delete)
{
MEMFILE *const f = (MEMFILE *)cf;
if (!delete)
return_error(gs_error_invalidfileaccess);
memfile_free_mem(f);
while (f->reserveLogBlockChain != NULL) {
LOG_MEMFILE_BLK *block = f->reserveLogBlockChain;
f->reserveLogBlockChain = block->link;
FREE(f, block, "memfile_set_block_size");
}
while (f->reservePhysBlockChain != NULL) {
PHYS_MEMFILE_BLK *block = f->reservePhysBlockChain;
f->reservePhysBlockChain = block->link;
FREE(f, block, "memfile_set_block_size");
}
gs_free_object(f->memory, f->decompress_state,
"memfile_close_and_unlink(decompress_state)");
gs_free_object(f->memory, f->compress_state,
"memfile_close_and_unlink(compress_state)");
gs_free_object(f->memory, f, "memfile_close_and_unlink(MEMFILE)");
return 0;
}
int
memfile_unlink(const char *fname)
{
return_error(gs_error_invalidfileaccess);
}
int
memfile_set_memory_warning(clist_file_ptr cf, int bytes_left)
{
MEMFILE *const f = (MEMFILE *)cf;
int code = 0;
int logNeeded =
(bytes_left + MEMFILE_DATA_SIZE - 1) / MEMFILE_DATA_SIZE;
int physNeeded = logNeeded;
if (bytes_left > 0)
++physNeeded;
if (f->raw_head == NULL)
++physNeeded;
while (logNeeded > f->reserveLogBlockCount) {
LOG_MEMFILE_BLK *block =
MALLOC( f, sizeof(LOG_MEMFILE_BLK), "memfile_set_block_size" );
if (block == NULL) {
code = gs_note_error(gs_error_VMerror);
goto finish;
}
block->link = f->reserveLogBlockChain;
f->reserveLogBlockChain = block;
++f->reserveLogBlockCount;
}
while (logNeeded < f->reserveLogBlockCount) {
LOG_MEMFILE_BLK *block = f->reserveLogBlockChain;
f->reserveLogBlockChain = block->link;
FREE(f, block, "memfile_set_block_size");
--f->reserveLogBlockCount;
}
while (physNeeded > f->reservePhysBlockCount) {
PHYS_MEMFILE_BLK *block =
MALLOC( f,
max( sizeof(PHYS_MEMFILE_BLK), sizeof(RAW_BUFFER) ),
"memfile_set_block_size");
if (block == NULL) {
code = gs_note_error(gs_error_VMerror);
goto finish;
}
block->link = f->reservePhysBlockChain;
f->reservePhysBlockChain = block;
++f->reservePhysBlockCount;
}
while (physNeeded < f->reservePhysBlockCount) {
PHYS_MEMFILE_BLK *block = f->reservePhysBlockChain;
f->reservePhysBlockChain = block->link;
FREE(f, block, "memfile_set_block_size");
--f->reservePhysBlockCount;
}
f->error_code = 0;
finish:
return code;
}
private int
compress_log_blk(MEMFILE * f, LOG_MEMFILE_BLK * bp)
{
int status;
int ecode = 0;
int code;
long compressed_size;
byte *start_ptr;
PHYS_MEMFILE_BLK *newphys;
f->rd.ptr = (const byte *)(bp->phys_blk->data) - 1;
f->rd.limit = f->rd.ptr + MEMFILE_DATA_SIZE;
bp->phys_blk = f->phys_curr;
bp->phys_pdata = (char *)(f->wt.ptr) + 1;
if (f->compress_state->template->reinit != 0)
(*f->compress_state->template->reinit)(f->compress_state);
compressed_size = 0;
start_ptr = f->wt.ptr;
status = (*f->compress_state->template->process)(f->compress_state,
&(f->rd), &(f->wt), true);
bp->phys_blk->data_limit = (char *)(f->wt.ptr);
if (status == 1) {
compressed_size = f->wt.limit - start_ptr;
newphys =
allocateWithReserve(f, sizeof(*newphys), &code, "memfile newphys",
"compress_log_blk : MALLOC for 'newphys' failed\n");
if (code < 0)
return code;
ecode |= code;
newphys->link = NULL;
bp->phys_blk->link = newphys;
f->phys_curr = newphys;
f->wt.ptr = (byte *) (newphys->data) - 1;
f->wt.limit = f->wt.ptr + MEMFILE_DATA_SIZE;
start_ptr = f->wt.ptr;
status =
(*f->compress_state->template->process)(f->compress_state,
&(f->rd), &(f->wt), true);
if (status != 0) {
eprintf("Compression required more than one full block!\n");
return_error(gs_error_Fatal);
}
newphys->data_limit = (char *)(f->wt.ptr);
}
compressed_size += f->wt.ptr - start_ptr;
if (compressed_size > MEMFILE_DATA_SIZE) {
eprintf2("\nCompression didn't - raw=%d, compressed=%ld\n",
MEMFILE_DATA_SIZE, compressed_size);
}
#ifdef DEBUG
tot_compressed += compressed_size;
#endif
return (status < 0 ? gs_note_error(gs_error_ioerror) : ecode);
}
private int
memfile_next_blk(MEMFILE * f)
{
LOG_MEMFILE_BLK *bp = f->log_curr_blk;
LOG_MEMFILE_BLK *newbp;
PHYS_MEMFILE_BLK *newphys, *oldphys;
int ecode = 0;
int code;
if (f->phys_curr == NULL) {
newphys =
allocateWithReserve(f, sizeof(*newphys), &code, "memfile newphys",
"memfile_next_blk: MALLOC 1 for 'newphys' failed\n");
if (code < 0)
return code;
ecode |= code;
newphys->link = NULL;
newphys->data_limit = NULL;
newbp =
allocateWithReserve(f, sizeof(*newbp), &code, "memfile newbp",
"memfile_next_blk: MALLOC 1 for 'newbp' failed\n");
if (code < 0) {
FREE(f, newphys, "memfile newphys");
return code;
}
ecode |= code;
bp->link = newbp;
newbp->link = NULL;
newbp->raw_block = NULL;
f->log_curr_blk = newbp;
if (NEED_TO_COMPRESS(f)) {
if_debug0(':', "[:]Beginning compression\n");
if (!f->compressor_initialized) {
int code = 0;
if (f->compress_state->template->init != 0)
code = (*f->compress_state->template->init) (f->compress_state);
if (code < 0)
return_error(gs_error_VMerror);
if (f->decompress_state->template->init != 0)
code = (*f->decompress_state->template->init)
(f->decompress_state);
if (code < 0)
return_error(gs_error_VMerror);
f->compressor_initialized = true;
}
f->phys_curr = newphys;
f->wt.ptr = (byte *) (newphys->data) - 1;
f->wt.limit = f->wt.ptr + MEMFILE_DATA_SIZE;
bp = f->log_head;
while (bp != newbp) {
int code;
oldphys = bp->phys_blk;
if ((code = compress_log_blk(f, bp)) < 0)
return code;
ecode |= code;
FREE(f, oldphys, "memfile_next_blk(oldphys)");
bp = bp->link;
}
newphys =
allocateWithReserve(f, sizeof(*newphys), &code,
"memfile newphys",
"memfile_next_blk: MALLOC 2 for 'newphys' failed\n");
if (code < 0)
return code;
ecode |= code;
newphys->link = NULL;
newphys->data_limit = NULL;
}
newbp->phys_blk = newphys;
f->pdata = newphys->data;
f->pdata_end = newphys->data + MEMFILE_DATA_SIZE;
}
else {
int code;
oldphys = bp->phys_blk;
if ((code = compress_log_blk(f, bp)) < 0)
return code;
ecode |= code;
newbp =
allocateWithReserve(f, sizeof(*newbp), &code, "memfile newbp",
"memfile_next_blk: MALLOC 2 for 'newbp' failed\n");
if (code < 0)
return code;
ecode |= code;
bp->link = newbp;
newbp->link = NULL;
newbp->raw_block = NULL;
newbp->phys_blk = oldphys;
f->pdata = oldphys->data;
f->pdata_end = f->pdata + MEMFILE_DATA_SIZE;
f->log_curr_blk = newbp;
}
return (ecode);
}
int
memfile_fwrite_chars(const void *data, uint len, clist_file_ptr cf)
{
const char *str = (const char *)data;
MEMFILE *f = (MEMFILE *) cf;
uint count = len;
int ecode;
if (f->log_curr_pos == 0) {
int code;
memfile_free_mem(f);
if ((code = memfile_init_empty(f)) < 0) {
f->error_code = code;
return 0;
}
}
if (f->log_curr_blk->link != 0) {
eprintf(" Write file truncate -- need to free physical blocks.\n");
}
while (count) {
uint move_count = f->pdata_end - f->pdata;
if (move_count == 0) {
if ((ecode = memfile_next_blk(f)) != 0) {
f->error_code = ecode;
if (ecode < 0)
return 0;
}
} else {
if (move_count > count)
move_count = count;
memmove(f->pdata, str, move_count);
f->pdata += move_count;
str += move_count;
count -= move_count;
}
}
f->log_curr_pos += len;
f->log_length = f->log_curr_pos;
#ifdef DEBUG
tot_raw += len;
#endif
return (len);
}
private int
memfile_get_pdata(MEMFILE * f)
{
int i, num_raw_buffers, status;
LOG_MEMFILE_BLK *bp = f->log_curr_blk;
if (bp->phys_blk->data_limit == NULL) {
f->pdata = (bp->phys_blk)->data;
i = f->log_curr_pos % MEMFILE_DATA_SIZE;
i = f->log_curr_pos - i;
if (i + MEMFILE_DATA_SIZE > f->log_length)
f->pdata_end = f->pdata + f->log_length - i;
else
f->pdata_end = f->pdata + MEMFILE_DATA_SIZE;
} else {
if (f->raw_head == NULL) {
num_raw_buffers = GET_NUM_RAW_BUFFERS(f);
if (f->reservePhysBlockCount) {
f->raw_head = (RAW_BUFFER *)f->reservePhysBlockChain;
f->reservePhysBlockChain = f->reservePhysBlockChain->link;
--f->reservePhysBlockCount;
} else {
int code;
f->raw_head =
allocateWithReserve(f, sizeof(*f->raw_head), &code,
"memfile raw buffer",
"memfile_get_pdata: MALLOC for 'raw_head' failed\n");
if (code < 0)
return code;
}
f->raw_head->back = NULL;
f->raw_tail = f->raw_head;
f->raw_tail->log_blk = NULL;
for (i = 0; i < num_raw_buffers; i++) {
f->raw_tail->fwd = (RAW_BUFFER *) MALLOC(f, sizeof(RAW_BUFFER),
"memfile raw buffer");
if (!f->raw_tail->fwd)
break;
f->total_space += sizeof(RAW_BUFFER);
f->raw_tail->fwd->back = f->raw_tail;
f->raw_tail = f->raw_tail->fwd;
f->raw_tail->log_blk = NULL;
}
f->raw_tail->fwd = NULL;
num_raw_buffers = i + 1;
if_debug1(':', "[:]Number of raw buffers allocated=%d\n",
num_raw_buffers);
}
if (bp->raw_block == NULL) {
#ifdef DEBUG
tot_cache_miss++;
#endif
if (f->raw_tail->log_blk != NULL) {
#ifdef DEBUG
tot_swap_out++;
#endif
f->raw_tail->log_blk->raw_block = NULL;
f->raw_tail->log_blk = NULL;
}
f->raw_tail->back->fwd = NULL;
f->raw_tail->fwd = f->raw_head;
f->raw_head->back = f->raw_tail;
f->raw_tail = f->raw_tail->back;
f->raw_head = f->raw_head->back;
f->raw_head->back = NULL;
f->raw_head->log_blk = bp;
if (f->decompress_state->template->reinit != 0)
(*f->decompress_state->template->reinit) (f->decompress_state);
f->wt.ptr = (byte *) (f->raw_head->data) - 1;
f->wt.limit = f->wt.ptr + MEMFILE_DATA_SIZE;
f->rd.ptr = (const byte *)(bp->phys_pdata) - 1;
f->rd.limit = (const byte *)bp->phys_blk->data_limit;
#ifdef DEBUG
decomp_wt_ptr0 = f->wt.ptr;
decomp_wt_limit0 = f->wt.limit;
decomp_rd_ptr0 = f->rd.ptr;
decomp_rd_limit0 = f->rd.limit;
#endif
status = (*f->decompress_state->template->process)
(f->decompress_state, &(f->rd), &(f->wt), true);
if (status == 0) {
int back_up = 0;
if (f->rd.ptr != f->rd.limit) {
back_up = f->rd.limit - f->rd.ptr;
for (i = 0; i < back_up; i++)
*(bp->phys_blk->link->data - back_up + i) = *++f->rd.ptr;
}
f->rd.ptr = (const byte *)bp->phys_blk->link->data - back_up - 1;
f->rd.limit = (const byte *)bp->phys_blk->link->data_limit;
#ifdef DEBUG
decomp_wt_ptr1 = f->wt.ptr;
decomp_wt_limit1 = f->wt.limit;
decomp_rd_ptr1 = f->rd.ptr;
decomp_rd_limit1 = f->rd.limit;
#endif
status = (*f->decompress_state->template->process)
(f->decompress_state, &(f->rd), &(f->wt), true);
if (status == 0) {
eprintf("Decompression required more than one full block!\n");
return_error(gs_error_Fatal);
}
}
bp->raw_block = f->raw_head;
}
else {
if (bp->raw_block != f->raw_head) {
bp->raw_block->back->fwd = bp->raw_block->fwd;
if (bp->raw_block->fwd != NULL)
bp->raw_block->fwd->back = bp->raw_block->back;
else
f->raw_tail = bp->raw_block->back;
f->raw_head->back = bp->raw_block;
bp->raw_block->fwd = f->raw_head;
f->raw_head = bp->raw_block;
f->raw_head->back = NULL;
#ifdef DEBUG
tot_cache_hits++;
#endif
}
}
f->pdata = bp->raw_block->data;
f->pdata_end = f->pdata + MEMFILE_DATA_SIZE;
}
return (0);
}
int
memfile_fread_chars(void *data, uint len, clist_file_ptr cf)
{
char *str = (char *)data;
MEMFILE *f = (MEMFILE *) cf;
uint count = len, num_read, move_count;
num_read = f->log_length - f->log_curr_pos;
if (count > num_read)
count = num_read;
num_read = count;
while (count) {
f->log_curr_pos++;
if (f->pdata == f->pdata_end) {
f->log_curr_blk = (f->log_curr_blk)->link;
memfile_get_pdata(f);
}
move_count = f->pdata_end - f->pdata;
if (move_count > count)
move_count = count;
f->log_curr_pos += move_count - 1;
memmove(str, f->pdata, move_count);
str += move_count;
f->pdata += move_count;
count -= move_count;
}
return (num_read);
}
int
memfile_ferror_code(clist_file_ptr cf)
{
return (((MEMFILE *) cf)->error_code);
}
long
memfile_ftell(clist_file_ptr cf)
{
return (((MEMFILE *) cf)->log_curr_pos);
}
void
memfile_rewind(clist_file_ptr cf, bool discard_data, const char *ignore_fname)
{
MEMFILE *f = (MEMFILE *) cf;
if (discard_data) {
memfile_free_mem(f);
memfile_init_empty(f);
} else {
f->log_curr_blk = f->log_head;
f->log_curr_pos = 0;
memfile_get_pdata(f);
}
}
int
memfile_fseek(clist_file_ptr cf, long offset, int mode, const char *ignore_fname)
{
MEMFILE *f = (MEMFILE *) cf;
long i, block_num, new_pos;
switch (mode) {
case SEEK_SET:
new_pos = offset;
break;
case SEEK_CUR:
new_pos = offset + f->log_curr_pos;
break;
case SEEK_END:
new_pos = f->log_length - offset;
break;
default:
return (-1);
}
if (new_pos < 0 || new_pos > f->log_length)
return -1;
if ((f->pdata == f->pdata_end) && (f->log_curr_blk->link != NULL)) {
f->log_curr_blk = f->log_curr_blk->link;
}
block_num = new_pos / MEMFILE_DATA_SIZE;
i = f->log_curr_pos / MEMFILE_DATA_SIZE;
if (block_num < i) {
f->log_curr_blk = f->log_head;
i = 0;
}
for (; i < block_num; i++) {
f->log_curr_blk = f->log_curr_blk->link;
}
f->log_curr_pos = new_pos;
memfile_get_pdata(f);
f->pdata += new_pos - (block_num * MEMFILE_DATA_SIZE);
return 0;
}
private void
memfile_free_mem(MEMFILE * f)
{
LOG_MEMFILE_BLK *bp, *tmpbp;
#ifdef DEBUG
if (tot_raw > 100) {
if_debug2(':', "[:]tot_raw=%ld, tot_compressed=%ld\n",
tot_raw, tot_compressed);
}
if (tot_cache_hits != 0) {
if_debug3(':', "[:]Cache hits=%ld, cache misses=%ld, swapouts=%ld\n",
tot_cache_hits,
tot_cache_miss - (f->log_length / MEMFILE_DATA_SIZE),
tot_swap_out);
}
tot_raw = 0;
tot_compressed = 0;
tot_cache_hits = 0;
tot_cache_miss = 0;
tot_swap_out = 0;
#endif
bp = f->log_head;
#if 0
if (bp != NULL) {
PHYS_MEMFILE_BLK *pphys = (f->log_head)->phys_blk;
if (pphys->data_limit != NULL) {
while (pphys != NULL) {
PHYS_MEMFILE_BLK *tmpphys = pphys->link;
FREE(f, pphys, "memfile_free_mem(pphys)");
pphys = tmpphys;
}
}
}
while (bp != NULL) {
if (bp->phys_blk->data_limit == NULL) {
FREE(f, bp->phys_blk, "memfile_free_mem(phys_blk)");
}
tmpbp = bp->link;
FREE(f, bp, "memfile_free_mem(log_blk)");
bp = tmpbp;
}
#else
# if 1
if (bp != NULL) {
PHYS_MEMFILE_BLK *pphys = bp->phys_blk;
{
for (tmpbp = bp; tmpbp != NULL; tmpbp = tmpbp->link)
if (tmpbp->phys_blk->data_limit != NULL)
tmpbp->phys_blk = 0;
}
if (pphys->data_limit != NULL) {
while (pphys != NULL) {
PHYS_MEMFILE_BLK *tmpphys = pphys->link;
FREE(f, pphys, "memfile_free_mem(pphys)");
pphys = tmpphys;
}
}
}
while (bp != NULL) {
if (bp->phys_blk != NULL) {
FREE(f, bp->phys_blk, "memfile_free_mem(phys_blk)");
}
tmpbp = bp->link;
FREE(f, bp, "memfile_free_mem(log_blk)");
bp = tmpbp;
}
# else
{
PHYS_MEMFILE_BLK *prev_phys = 0;
while (bp != NULL) {
PHYS_MEMFILE_BLK *phys = bp->phys_blk;
if (phys != prev_phys) {
FREE(f, phys, "memfile_free_mem(phys_blk)");
prev_phys = phys;
}
tmpbp = bp->link;
FREE(f, bp, "memfile_free_mem(log_blk)");
bp = tmpbp;
}
}
# endif
#endif
f->log_head = NULL;
if (f->compressor_initialized) {
if (f->decompress_state->template->release != 0)
(*f->decompress_state->template->release) (f->decompress_state);
if (f->compress_state->template->release != 0)
(*f->compress_state->template->release) (f->compress_state);
f->compressor_initialized = false;
}
while (f->raw_head != NULL) {
RAW_BUFFER *tmpraw = f->raw_head->fwd;
FREE(f, f->raw_head, "memfile_free_mem(raw)");
f->raw_head = tmpraw;
}
}
private int
memfile_init_empty(MEMFILE * f)
{
PHYS_MEMFILE_BLK *pphys;
LOG_MEMFILE_BLK *plog;
f->phys_curr = NULL;
f->log_head = NULL;
f->log_curr_blk = NULL;
f->log_curr_pos = 0;
f->log_length = 0;
f->raw_head = NULL;
f->compressor_initialized = false;
f->total_space = 0;
pphys = MALLOC(f, sizeof(*pphys), "memfile pphys");
if (!pphys) {
eprintf("memfile_init_empty: MALLOC for 'pphys' failed\n");
return_error(gs_error_VMerror);
}
f->total_space += sizeof(*pphys);
pphys->data_limit = NULL;
plog = (LOG_MEMFILE_BLK *)MALLOC( f, sizeof(*plog), "memfile_init_empty" );
if (plog == NULL) {
FREE(f, pphys, "memfile_init_empty");
eprintf("memfile_init_empty: MALLOC for log_curr_blk failed\n");
return_error(gs_error_VMerror);
}
f->total_space += sizeof(*plog);
f->log_head = f->log_curr_blk = plog;
f->log_curr_blk->link = NULL;
f->log_curr_blk->phys_blk = pphys;
f->log_curr_blk->phys_pdata = NULL;
f->log_curr_blk->raw_block = NULL;
f->pdata = pphys->data;
f->pdata_end = f->pdata + MEMFILE_DATA_SIZE;
f->error_code = 0;
return 0;
}