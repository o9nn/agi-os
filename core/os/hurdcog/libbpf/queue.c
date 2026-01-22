#include "queue.h"
void enqueue_head(
queue_t que,
queue_entry_t elt)
{
elt->next = que->next;
elt->prev = que;
elt->next->prev = elt;
que->next = elt;
}
void enqueue_tail(
queue_t que,
queue_entry_t elt)
{
elt->next = que;
elt->prev = que->prev;
elt->prev->next = elt;
que->prev = elt;
}
queue_entry_t dequeue_head(
queue_t que)
{
queue_entry_t elt;
if (que->next == que)
return((queue_entry_t)0);
elt = que->next;
elt->next->prev = que;
que->next = elt->next;
return(elt);
}
queue_entry_t dequeue_tail(
queue_t que)
{
queue_entry_t elt;
if (que->prev == que)
return((queue_entry_t)0);
elt = que->prev;
elt->prev->next = que;
que->prev = elt->prev;
return(elt);
}
void remqueue(
queue_t que,
queue_entry_t elt)
{
elt->next->prev = elt->prev;
elt->prev->next = elt->next;
}
void insque(
struct queue_entry *entry,
struct queue_entry *pred)
{
entry->next = pred->next;
entry->prev = pred;
(pred->next)->prev = entry;
pred->next = entry;
}
struct queue_entry
*remque(
struct queue_entry *elt)
{
(elt->next)->prev = elt->prev;
(elt->prev)->next = elt->next;
return(elt);
}