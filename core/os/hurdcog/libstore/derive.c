#include <assert-backtrace.h>
#include <sys/types.h>
#include <mach.h>
#include "store.h"
void
_store_derive (struct store *store)
{
unsigned i;
struct store_run *runs = store->runs;
unsigned num_runs = store->num_runs;
size_t bsize = store->block_size;
store->blocks = 0;
store->wrap_src = 0;
for (i = 0; i < num_runs; i++)
{
store->wrap_src += runs[i].length;
if (runs[i].start >= 0)
store->blocks += runs[i].length;
}
if (store->end == 0)
store->end = store->wrap_src;
else if (store->wrap_src < store->end)
{
size_t num_iters = store->end / store->wrap_src;
store_offset_t last_part_base = num_iters * store->wrap_src;
store->blocks *= num_iters;
for (i = 0; i < num_runs; i++)
if (last_part_base + runs[i].length < store->end)
{
store->blocks += store->end - (last_part_base + runs[i].length);
break;
}
else if (runs[i].start >= 0)
store->blocks += runs[i].length;
}
store->size = store->end * bsize;
store->log2_block_size = 0;
store->log2_blocks_per_page = 0;
if (bsize != 0)
{
while ((1 << store->log2_block_size) < bsize)
store->log2_block_size++;
assert_backtrace ((1 << store->log2_block_size) == bsize);
while ((bsize << store->log2_blocks_per_page) < vm_page_size)
store->log2_blocks_per_page++;
assert_backtrace ((bsize << store->log2_blocks_per_page) == vm_page_size);
}
}