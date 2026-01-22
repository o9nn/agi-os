#ifndef __PQ_H__
#define __PQ_H__
#include <errno.h>
#include <stddef.h>
#include <string.h>
#include <mach/mach.h>
#include <features.h>
#ifdef PQ_DEFINE_EI
#define PQ_EI
#else
#define PQ_EI __extern_inline
#endif
struct packet
{
unsigned short type;
void *source;
char *buf;
size_t buf_len;
char *buf_start, *buf_end;
int buf_vm_alloced;
mach_port_t *ports;
size_t num_ports, ports_alloced;
struct packet *next, *prev;
};
#define PACKET_TYPE_ANY 0
#define PACKET_TYPE_DATA 1
#define PACKET_TYPE_CONTROL 2
error_t packet_set_ports (struct packet *packet,
const mach_port_t *ports, size_t num_ports);
void packet_dealloc_ports (struct packet *packet);
extern size_t packet_readable (struct packet *packet);
#if defined(__USE_EXTERN_INLINES) || defined(PQ_DEFINE_EI)
PQ_EI size_t
packet_readable (struct packet *packet)
{
return packet->buf_end - packet->buf_start;
}
#endif
error_t packet_write (struct packet *packet,
const char *data, size_t data_len, size_t *amount);
error_t packet_read (struct packet *packet,
char **data, size_t *data_len, size_t amount);
error_t packet_peek (struct packet *packet,
char **data, size_t *data_len, size_t amount);
error_t packet_read_ports (struct packet *packet,
mach_port_t **ports, size_t *num_ports);
extern void packet_read_source (struct packet *packet, void **source);
#if defined(__USE_EXTERN_INLINES) || defined(PQ_DEFINE_EI)
PQ_EI void
packet_read_source (struct packet *packet, void **source)
{
*source = packet->source;
packet->source = 0;
}
#endif
#define PACKET_SIZE_LARGE 8192
size_t packet_new_size (struct packet *packet, size_t extra);
int packet_extend (struct packet *packet, size_t new_len);
error_t packet_realloc (struct packet *packet, size_t new_len);
extern int packet_fit (struct packet *packet, size_t amount);
extern error_t packet_ensure (struct packet *packet, size_t amount);
extern int packet_ensure_efficiently (struct packet *packet, size_t amount);
#if defined(__USE_EXTERN_INLINES) || defined(PQ_DEFINE_EI)
PQ_EI int
packet_fit (struct packet *packet, size_t amount)
{
char *buf = packet->buf, *end = packet->buf_end;
size_t buf_len = packet->buf_len;
size_t left = buf + buf_len - end;
if (amount > left)
{
char *start = packet->buf_start;
size_t cur_len = end - start;
if (buf_len - cur_len >= amount
&& cur_len < PACKET_SIZE_LARGE && cur_len < (buf_len >> 2))
{
memmove (buf, start, cur_len);
packet->buf_start = buf;
packet->buf_end = buf + cur_len;
}
else
return 0;
}
return 1;
}
PQ_EI error_t
packet_ensure (struct packet *packet, size_t amount)
{
if (! packet_fit (packet, amount))
{
size_t new_len = packet_new_size (packet, amount);
if (! packet_extend (packet, new_len))
return packet_realloc (packet, new_len);
}
return 0;
}
PQ_EI int
packet_ensure_efficiently (struct packet *packet, size_t amount)
{
if (! packet_fit (packet, amount))
{
size_t new_len = packet_new_size (packet, amount);
if (packet_extend (packet, new_len))
return 1;
if ((packet->buf_end - packet->buf_start) < PACKET_SIZE_LARGE)
return packet_realloc (packet, new_len) == 0;
}
return 0;
}
#endif
struct pq
{
struct packet *head, *tail;
struct packet *free;
};
struct packet *pq_queue (struct pq *pq, unsigned type, void *source);
extern struct packet * pq_tail (struct pq *pq, unsigned type, void *source);
#if defined(__USE_EXTERN_INLINES) || defined(PQ_DEFINE_EI)
PQ_EI struct packet *
pq_tail (struct pq *pq, unsigned type, void *source)
{
struct packet *tail = pq->tail;
if (!tail
|| (type && tail->type != type) || (source && tail->source != source))
tail = pq_queue (pq, type, source);
return tail;
}
#endif
int pq_dequeue (struct pq *pq);
extern struct packet * pq_head (struct pq *pq, unsigned type, void *source);
extern struct packet * pq_next (struct pq *pq, unsigned type, void *source);
#if defined(__USE_EXTERN_INLINES) || defined(PQ_DEFINE_EI)
PQ_EI struct packet *
pq_head (struct pq *pq, unsigned type, void *source)
{
struct packet *head = pq->head;
if (!head)
return 0;
if (type && head->type != type)
return 0;
if (source && head->source != source)
return 0;
return head;
}
PQ_EI struct packet *
pq_next (struct pq *pq, unsigned type, void *source)
{
if (!pq->head)
return 0;
pq_dequeue (pq);
return pq_head (pq, type, source);
}
#endif
void pq_drain (struct pq *pq);
error_t pq_create (struct pq **pq);
void pq_free (struct pq *pq);
#endif