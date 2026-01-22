#include <malloc.h>
#include <string.h>
#include <stddef.h>
#include <sys/mman.h>
#include <assert-backtrace.h>
#include "pq.h"
error_t
pq_create (struct pq **pq)
{
*pq = malloc (sizeof (struct pq));
if (! *pq)
return ENOMEM;
(*pq)->head = (*pq)->tail = 0;
(*pq)->free = 0;
return 0;
}
static void
free_packets (struct packet *head)
{
if (head)
{
struct packet *next = head->next;
if (head->ports)
free (head->ports);
if (head->buf_len > 0)
{
if (head->buf_vm_alloced)
munmap (head->buf, head->buf_len);
else
free (head->buf);
}
free (head);
free_packets (next);
}
}
void
pq_free (struct pq *pq)
{
pq_drain (pq);
free_packets (pq->free);
free (pq);
}
int
pq_dequeue (struct pq *pq)
{
extern void pipe_dealloc_addr (void *addr);
struct packet *packet = pq->head;
if (! packet)
return 0;
if (packet->num_ports)
packet_dealloc_ports (packet);
if (packet->source)
pipe_dealloc_addr (packet->source);
pq->head = packet->next;
packet->next = pq->free;
pq->free = packet;
if (pq->head)
pq->head->prev = 0;
else
pq->tail = 0;
return 1;
}
void
pq_drain (struct pq *pq)
{
while (pq_dequeue (pq))
;
}
struct packet *
pq_queue (struct pq *pq, unsigned type, void *source)
{
struct packet *packet = pq->free;
if (!packet)
{
packet = malloc (sizeof (struct packet));
if (!packet)
return 0;
packet->buf = 0;
packet->buf_len = 0;
packet->ports = 0;
packet->ports_alloced = 0;
packet->buf_vm_alloced = 0;
}
else
pq->free = packet->next;
packet->num_ports = 0;
packet->buf_start = packet->buf_end = packet->buf;
packet->type = type;
packet->source = source;
packet->next = 0;
packet->prev = pq->tail;
if (pq->tail)
pq->tail->next = packet;
pq->tail = packet;
if (!pq->head)
pq->head = packet;
return packet;
}
size_t
packet_new_size (struct packet *packet, size_t extra)
{
size_t new_len = (packet->buf_end - packet->buf) + extra;
if (packet->buf_vm_alloced || new_len >= PACKET_SIZE_LARGE)
return round_page (new_len);
else
return (new_len + 511) & ~511;
}
int
packet_extend (struct packet *packet, size_t new_len)
{
size_t old_len = packet->buf_len;
if (old_len == 0)
return 0;
if (packet->buf_vm_alloced)
{
char *extension = packet->buf + old_len;
if (vm_allocate (mach_task_self (),
(vm_address_t *)&extension, new_len - old_len, 0) != 0)
return 0;
}
else
{
char *new_buf;
ptrdiff_t start_offset = packet->buf_start - packet->buf;
ptrdiff_t end_offset = packet->buf_end - packet->buf;
if (new_len >= PACKET_SIZE_LARGE)
return 0;
new_buf = realloc (packet->buf, new_len);
if (! new_buf)
return 0;
packet->buf = new_buf;
packet->buf_start = new_buf + start_offset;
packet->buf_end = new_buf + end_offset;
}
packet->buf_len = new_len;
return 1;
}
error_t
packet_realloc (struct packet *packet, size_t new_len)
{
error_t err;
char *new_buf;
char *old_buf = packet->buf;
int vm_alloc = (new_len >= PACKET_SIZE_LARGE);
if (vm_alloc)
{
new_buf = mmap (0, new_len, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
err = (new_buf == (char *) -1) ? errno : 0;
}
else
{
new_buf = malloc (new_len);
err = (new_buf ? 0 : ENOMEM);
}
if (! err)
{
size_t old_len = packet->buf_len;
char *start = packet->buf_start, *end = packet->buf_end;
if (end != start)
memcpy (new_buf, start, end - start);
if (old_len > 0)
{
if (packet->buf_vm_alloced)
vm_deallocate (mach_task_self (), (vm_address_t)old_buf, old_len);
else
free (old_buf);
}
packet->buf = new_buf;
packet->buf_len = new_len;
packet->buf_vm_alloced = vm_alloc;
packet->buf_start = new_buf;
packet->buf_end = new_buf + (end - start);
}
return err;
}
void
packet_dealloc_ports (struct packet *packet)
{
unsigned i;
for (i = 0; i < packet->num_ports; i++)
{
mach_port_t port = packet->ports[i];
if (port != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), port);
}
}
error_t
packet_set_ports (struct packet *packet,
const mach_port_t *ports, size_t num_ports)
{
if (packet->num_ports > 0)
packet_dealloc_ports (packet);
if (num_ports > packet->ports_alloced)
{
mach_port_t *new_ports = malloc (sizeof (mach_port_t) * num_ports);
if (! new_ports)
return ENOMEM;
free (packet->ports);
packet->ports = new_ports;
packet->ports_alloced = num_ports;
}
memcpy (packet->ports, ports, sizeof (mach_port_t) * num_ports);
packet->num_ports = num_ports;
return 0;
}
error_t
packet_read_ports (struct packet *packet,
mach_port_t **ports, size_t *num_ports)
{
int length = packet->num_ports * sizeof (mach_port_t);
if (*num_ports < packet->num_ports)
{
*ports = mmap (0, length, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (*ports == (mach_port_t *) -1)
return errno;
}
*num_ports = packet->num_ports;
memcpy (*ports, packet->ports, length);
packet->num_ports = 0;
return 0;
}
error_t
packet_write (struct packet *packet,
const char *data, size_t data_len, size_t *amount)
{
error_t err = packet_ensure (packet, data_len);
if (err)
return err;
memcpy (packet->buf_end, data, data_len);
packet->buf_end += data_len;
if (amount != NULL)
*amount = data_len;
return 0;
}
static error_t
packet_fetch (struct packet *packet,
char **data, size_t *data_len, size_t amount, int remove)
{
char *start = packet->buf_start;
char *end = packet->buf_end;
if (amount > end - start)
amount = end - start;
if (amount > 0)
{
char *buf = packet->buf;
if (remove && packet->buf_vm_alloced && amount >= vm_page_size)
{
if (buf + vm_page_size <= start)
vm_deallocate (mach_task_self (),
(vm_address_t)buf,
trunc_page (start) - (vm_address_t)buf);
*data = start;
start += amount;
if (start < end)
{
char *non_aligned_start = start;
start = (char *)trunc_page (start);
amount -= non_aligned_start - start;
}
else
{
start = (char *)round_page (start);
packet->buf_end = start;
}
packet->buf = start;
packet->buf_start = start;
packet->buf_len -= start - buf;
}
else
{
if (*data_len < amount)
{
*data = mmap (0, amount, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (*data == MAP_FAILED)
assert_perror_backtrace (errno);
else
assert_backtrace (*data);
}
memcpy (*data, start, amount);
start += amount;
if (remove && start - buf > 2 * PACKET_SIZE_LARGE)
{
vm_size_t dealloc = trunc_page (start) - (vm_address_t)buf;
vm_deallocate (mach_task_self (), (vm_address_t)buf, dealloc);
packet->buf = buf + dealloc;
packet->buf_len -= dealloc;
}
if (remove)
packet->buf_start = start;
}
}
*data_len = amount;
return 0;
}
error_t
packet_read (struct packet *packet,
char **data, size_t *data_len, size_t amount)
{
return packet_fetch (packet, data, data_len, amount, 1);
}
error_t
packet_peek (struct packet *packet,
char **data, size_t *data_len, size_t amount)
{
return packet_fetch (packet, data, data_len, amount, 0);
}