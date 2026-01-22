#ifndef	_KERN_QUEUE_H_
#define	_KERN_QUEUE_H_
struct queue_entry {
struct queue_entry	*next;
struct queue_entry	*prev;
};
typedef struct queue_entry	*queue_t;
typedef	struct queue_entry	queue_head_t;
typedef	struct queue_entry	queue_chain_t;
typedef	struct queue_entry	*queue_entry_t;
#define enqueue(queue,elt)	enqueue_tail(queue, elt)
#define	dequeue(queue)		dequeue_head(queue)
void		enqueue_head(queue_t, queue_entry_t);
void		enqueue_tail(queue_t, queue_entry_t);
queue_entry_t	dequeue_head(queue_t);
queue_entry_t	dequeue_tail(queue_t);
void		remqueue(queue_t, queue_entry_t);
#define	queue_init(q)	((q)->next = (q)->prev = q)
#define	queue_first(q)	((q)->next)
#define	queue_next(qc)	((qc)->next)
#define	queue_last(q)	((q)->prev)
#define	queue_prev(qc)	((qc)->prev)
#define	queue_end(q, qe)	((q) == (qe))
#define	queue_empty(q)		queue_end((q), queue_first(q))
#define queue_enter(head, elt, type, field)			\
{ 								\
register queue_entry_t prev;				\
\
prev = (head)->prev;					\
if ((head) == prev) {					\
(head)->next = (queue_entry_t) (elt);		\
}							\
else {							\
((type)prev)->field.next = (queue_entry_t)(elt);\
}							\
(elt)->field.prev = prev;				\
(elt)->field.next = head;				\
(head)->prev = (queue_entry_t) elt;			\
}
#define queue_enter_first(head, elt, type, field)		\
{ 								\
register queue_entry_t next;				\
\
next = (head)->next;					\
if ((head) == next) {					\
(head)->prev = (queue_entry_t) (elt);		\
}							\
else {							\
((type)next)->field.prev = (queue_entry_t)(elt);\
}							\
(elt)->field.next = next;				\
(elt)->field.prev = head;				\
(head)->next = (queue_entry_t) elt;			\
}
#define	queue_field(head, thing, type, field)			\
(((head) == (thing)) ? (head) : &((type)(thing))->field)
#define	queue_remove(head, elt, type, field)			\
{								\
register queue_entry_t	next, prev;			\
\
next = (elt)->field.next;				\
prev = (elt)->field.prev;				\
\
if ((head) == next)					\
(head)->prev = prev;				\
else							\
((type)next)->field.prev = prev;		\
\
if ((head) == prev)					\
(head)->next = next;				\
else							\
((type)prev)->field.next = next;		\
}
#define	queue_remove_first(head, entry, type, field)		\
{								\
register queue_entry_t	next;				\
\
(entry) = (type) ((head)->next);			\
next = (entry)->field.next;				\
\
if ((head) == next)					\
(head)->prev = (head);				\
else							\
((type)(next))->field.prev = (head);		\
(head)->next = next;					\
}
#define	queue_remove_last(head, entry, type, field)		\
{								\
register queue_entry_t	prev;				\
\
(entry) = (type) ((head)->prev);			\
prev = (entry)->field.prev;				\
\
if ((head) == prev)					\
(head)->next = (head);				\
else							\
((type)(prev))->field.next = (head);		\
(head)->prev = prev;					\
}
#define	queue_assign(to, from, type, field)			\
{								\
((type)((from)->prev))->field.next = (to);		\
((type)((from)->next))->field.prev = (to);		\
*to = *from;						\
}
#define queue_iterate(head, elt, type, field)			\
for ((elt) = (type) queue_first(head);			\
!queue_end((head), (queue_entry_t)(elt));		\
(elt) = (type) queue_next(&(elt)->field))
#endif