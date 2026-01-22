#include <sys/types.h>
#include <string.h>
#include "ring.h"
void hyp_ring_store(void *dest, const void *src, size_t size, void *start, void *end)
{
if (dest + size > end) {
size_t first_size = end - dest;
memcpy(dest, src, first_size);
src += first_size;
dest = start;
size -= first_size;
}
memcpy(dest, src, size);
}
void hyp_ring_fetch(void *dest, const void *src, size_t size, void *start, void *end)
{
if (src + size > end) {
size_t first_size = end - src;
memcpy(dest, src, first_size);
dest += first_size;
src = start;
size -= first_size;
}
memcpy(dest, src, size);
}
size_t hyp_ring_next_word(char **c, void *start, void *end)
{
size_t n = 0;
while (**c) {
n++;
if (++(*c) == end)
*c = start;
}
(*c)++;
return n;
}