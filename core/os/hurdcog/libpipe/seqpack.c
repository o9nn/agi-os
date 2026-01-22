#include <sys/socket.h>
#include "pipe.h"
#include "pq.h"
static error_t
seqpack_write (struct pq *pq, void *source,
const char *data, size_t data_len, size_t *amount)
{
struct packet *packet = pq_queue (pq, PACKET_TYPE_DATA, source);
if (!packet)
return ENOBUFS;
else
return packet_write (packet, data, data_len, amount);
}
static error_t
seqpack_read (struct packet *packet, int *dequeue, unsigned *flags,
char **data, size_t *data_len, size_t amount)
{
error_t err;
if (flags && *flags & MSG_PEEK)
{
err = packet_peek (packet, data, data_len, amount);
*dequeue = 0;
}
else
{
err = packet_read (packet, data, data_len, amount);
*dequeue = (packet_readable (packet) == 0);
}
return err;
}
struct pipe_class _seqpack_pipe_class =
{
SOCK_SEQPACKET, 0, seqpack_read, seqpack_write
};
struct pipe_class *seqpack_pipe_class = &_seqpack_pipe_class;