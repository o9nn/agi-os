#include <sys/socket.h>
#include "pipe.h"
#include "pq.h"
static error_t
dgram_write (struct pq *pq, void *source,
const char *data, size_t data_len, size_t *amount)
{
struct packet *packet = pq_queue (pq, PACKET_TYPE_DATA, source);
if (!packet)
return ENOBUFS;
else
return packet_write (packet, data, data_len, amount);
}
static error_t
dgram_read (struct packet *packet, int *dequeue, unsigned *flags,
char **data, size_t *data_len, size_t amount)
{
if (flags && *flags & MSG_PEEK)
{
*dequeue = 0;
return packet_peek (packet, data, data_len, amount);
}
else
{
*dequeue = 1;
return packet_read (packet, data, data_len, amount);
}
}
struct pipe_class _dgram_pipe_class =
{
SOCK_DGRAM, PIPE_CLASS_CONNECTIONLESS, dgram_read, dgram_write
};
struct pipe_class *dgram_pipe_class = &_dgram_pipe_class;