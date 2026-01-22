#include <iconv.h>
#include <error.h>
#include <string.h>
#include <errno.h>
#include <malloc.h>
#include <sys/types.h>
#include <pthread.h>
#include "input.h"
struct input
{
pthread_mutex_t lock;
pthread_cond_t data_available;
pthread_cond_t space_available;
#define INPUT_QUEUE_SIZE 300
char buffer[INPUT_QUEUE_SIZE];
int full;
size_t size;
iconv_t cd;
char *cd_buffer;
size_t cd_size;
size_t cd_allocated;
};
error_t input_create (input_t *r_input, const char *encoding)
{
input_t input = calloc (1, sizeof *input);
if (!input)
return ENOMEM;
pthread_mutex_init (&input->lock, NULL);
pthread_cond_init (&input->data_available, NULL);
pthread_cond_init (&input->space_available, NULL);
input->cd = iconv_open (encoding, "UTF-8");
if (input->cd == (iconv_t) -1)
{
free (input);
return errno;
}
*r_input = input;
return 0;
}
void input_destroy (input_t input)
{
iconv_close (input->cd);
free (input);
}
ssize_t input_enqueue (input_t input, int nonblock, const char *data,
size_t datalen)
{
error_t err = 0;
int was_empty;
int enqueued = 0;
char *buffer;
size_t buffer_size;
ssize_t amount;
size_t nconv;
char *outbuf;
size_t outbuf_size;
error_t ensure_cd_buffer_size (size_t new_size)
{
#define CD_ALLOCSIZE 32
if (input->cd_allocated < new_size)
{
char *new_buffer;
new_size = (new_size + CD_ALLOCSIZE - 1)
& ~(CD_ALLOCSIZE - 1);
new_buffer = realloc (input->cd_buffer, new_size);
if (!new_buffer)
return ENOMEM;
input->cd_buffer = new_buffer;
input->cd_allocated = new_size;
}
return 0;
}
pthread_mutex_lock (&input->lock);
was_empty = !input->size;
while (datalen)
{
while (input->full)
{
if (nonblock)
{
err = EWOULDBLOCK;
goto out;
}
if (pthread_hurd_cond_wait_np (&input->space_available, &input->lock))
{
err = EINTR;
goto out;
}
was_empty = !input->size;
}
if (input->cd_size)
{
err = ensure_cd_buffer_size (input->cd_size + datalen);
if (err)
goto out;
buffer = input->cd_buffer;
buffer_size = input->cd_size;
memcpy (buffer + buffer_size, data, datalen);
buffer_size += datalen;
}
else
{
buffer = (char*) data;
buffer_size = datalen;
}
outbuf = &input->buffer[input->size];
outbuf_size = INPUT_QUEUE_SIZE - input->size;
amount = buffer_size;
nconv = iconv (input->cd, &buffer, &buffer_size, &outbuf, &outbuf_size);
amount -= buffer_size;
enqueued += amount;
data = buffer;
datalen = buffer_size;
input->size = INPUT_QUEUE_SIZE - outbuf_size;
if (nconv == (size_t) -1)
{
if (errno == E2BIG)
{
input->full = 1;
if (was_empty)
pthread_cond_broadcast (&input->data_available);
was_empty = 0;
}
else
break;
}
}
if (errno == EINVAL && datalen)
{
err = ensure_cd_buffer_size (datalen);
if (err)
{
pthread_mutex_unlock (&input->lock);
errno = err;
return enqueued ?: -1;
}
memmove (input->cd_buffer, data, datalen);
}
out:
if (enqueued)
{
if (was_empty)
pthread_cond_broadcast (&input->data_available);
}
else
errno = err;
pthread_mutex_unlock (&input->lock);
return enqueued ?: -1;
}
ssize_t input_dequeue (input_t input, int nonblock, char *data,
size_t datalen)
{
size_t amount = datalen;
pthread_mutex_lock (&input->lock);
while (!input->size)
{
if (nonblock)
{
pthread_mutex_unlock (&input->lock);
errno = EWOULDBLOCK;
return -1;
}
if (pthread_hurd_cond_wait_np (&input->data_available, &input->lock))
{
pthread_mutex_unlock (&input->lock);
errno = EINTR;
return -1;
}
}
if (amount > input->size)
amount = input->size;
memcpy (data, input->buffer, amount);
memmove (input->buffer, input->buffer + amount, input->size - amount);
input->size -= amount;
if (amount && input->full)
{
input->full = 0;
pthread_cond_broadcast (&input->space_available);
}
pthread_mutex_unlock (&input->lock);
return amount;
}
void input_flush (input_t input)
{
pthread_mutex_lock (&input->lock);
input->size = 0;
if (input->full)
{
input->full = 0;
pthread_cond_broadcast (&input->space_available);
}
pthread_mutex_unlock (&input->lock);
}