#include <mach/kern_return.h>
#include <mach/vm_param.h>
#include <ipc/ipc_table.h>
#include <ipc/ipc_port.h>
#include <ipc/ipc_entry.h>
#include <kern/kalloc.h>
#include <kern/slab.h>
#include <vm/vm_kern.h>
ipc_table_size_t ipc_table_dnrequests;
const unsigned int ipc_table_dnrequests_size = 64;
void
ipc_table_fill(
ipc_table_size_t its,
unsigned int num,
unsigned int min,
vm_size_t elemsize)
{
unsigned int index;
vm_size_t minsize = min * elemsize;
vm_size_t size;
vm_size_t incrsize;
for (index = 0, size = 1;
(index < num) && (size < PAGE_SIZE);
size <<= 1) {
if (size >= minsize) {
its[index].its_size = size / elemsize;
index++;
}
}
for (incrsize = PAGE_SIZE; index < num;) {
unsigned int period;
for (period = 0;
(period < 15) && (index < num);
period++, size += incrsize) {
if (size >= minsize) {
its[index].its_size = size / elemsize;
index++;
}
}
if (incrsize < (PAGE_SIZE << 3))
incrsize <<= 1;
}
}
void
ipc_table_init(void)
{
ipc_table_dnrequests = (ipc_table_size_t)
kalloc(sizeof(struct ipc_table_size) *
ipc_table_dnrequests_size);
assert(ipc_table_dnrequests != ITS_NULL);
ipc_table_fill(ipc_table_dnrequests, ipc_table_dnrequests_size - 1,
2, sizeof(struct ipc_port_request));
ipc_table_dnrequests[ipc_table_dnrequests_size - 1].its_size = 0;
}
vm_offset_t
ipc_table_alloc(
vm_size_t size)
{
return kalloc(size);
}
void
ipc_table_free(
vm_size_t size,
vm_offset_t table)
{
kfree(table, size);
}