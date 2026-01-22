#include <sys/socket.h>
#include "pipe.h"
#include "pq.h"
static inline int page_aligned (vm_offset_t num)
{
return trunc_page (num) == num;
}
static error_t
stream_write (struct pq *pq, void *source,
const char *data, size_t data_len, size_t *amount)
{
struct packet *packet = pq_tail (pq, PACKET_TYPE_DATA, source);
if (packet_readable (packet) > 0
&& data_len > PACKET_SIZE_LARGE
&& (! page_aligned (data - packet->buf_end)
|| ! packet_ensure_efficiently (packet, data_len)))
packet = pq_queue (pq, PACKET_TYPE_DATA, source);
if (!packet)
return ENOBUFS;
else
return packet_write (packet, data, data_len, amount);
}
static error_t
stream_read (struct packet *packet, int *dequeue, unsigned *flags,
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
struct pipe_class _stream_pipe_class =
{
SOCK_STREAM, 0, stream_read, stream_write
};
struct pipe_class *stream_pipe_class = &_stream_pipe_class;