#include <string.h>
#include <stdlib.h>
#include "cacheq.h"
void
cacheq_make_mru (struct cacheq *cq, void *entry)
{
struct cacheq_hdr *h = entry;
if (h != cq->mru)
{
((struct cacheq_hdr *)h->prev)->next = h->next;
if (h->next)
((struct cacheq_hdr *)h->next)->prev = h->prev;
else
cq->lru = h->prev;
h->next = cq->mru;
h->prev = 0;
((struct cacheq_hdr *)cq->mru)->prev = h;
cq->mru = h;
}
}
void
cacheq_make_lru (struct cacheq *cq, void *entry)
{
struct cacheq_hdr *h = entry;
if (h != cq->lru)
{
((struct cacheq_hdr *)h->next)->prev = h->prev;
if (h->prev)
((struct cacheq_hdr *)h->prev)->next = h->next;
else
cq->mru = h->next;
h->prev = cq->lru;
h->next = 0;
((struct cacheq_hdr *)cq->lru)->next = h;
cq->lru = h;
}
}
error_t
cacheq_set_length (struct cacheq *cq, int length)
{
if (length != cq->length)
{
size_t esz = cq->entry_size;
void *new_entries = malloc (esz * length);
struct cacheq_hdr *fh = cq->mru;
struct cacheq_hdr *th = new_entries;
struct cacheq_hdr *end = new_entries + esz * (length - 1);
struct cacheq_hdr *prev_th = 0;
if (! new_entries)
return ENOMEM;
while (fh || th)
{
struct cacheq_hdr *next_th =
(!th || th >= end) ? 0 : (void *)th + esz;
if (fh && th)
memcpy (th, fh, esz);
else if (th)
memset (th, 0, esz);
if (th)
{
th->prev = prev_th;
th->next = next_th;
}
if (fh && th)
{
if (cq->move_entry)
(*cq->move_entry) (fh, th);
}
else if (th)
{
if (cq->init_entry)
(*cq->init_entry) (th);
}
else
{
if (cq->finalize_entry)
(*cq->finalize_entry) (fh);
}
if (fh)
fh = fh->next;
if (th)
{
prev_th = th;
th = next_th;
}
}
free (cq->entries);
cq->entries = new_entries;
cq->mru = new_entries;
cq->lru = prev_th;
cq->length = length;
}
return 0;
}