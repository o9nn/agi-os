#include <stdbool.h>
struct item {
struct item *next;
};
struct queue {
struct item *head;
struct item **tail;
};
static inline void
queue_init (struct queue *q)
{
q->head = NULL;
q->tail = &q->head;
}
static inline void
queue_enqueue (struct queue *q, struct item *r)
{
*q->tail = r;
q->tail = &r->next;
r->next = NULL;
}
static inline void *
queue_dequeue (struct queue *q)
{
struct item *r = q->head;
if (r == NULL)
return NULL;
if ((q->head = q->head->next) == NULL)
q->tail = &q->head;
r->next = NULL;
return r;
}
static inline bool
queue_empty (struct queue *q)
{
return q->head == NULL;
}