#include <i386at/kd_queue.h>
#define q_next(index)	(((index)+1) % KDQSIZE)
boolean_t
kdq_empty(const kd_event_queue *q)
{
return(q->firstfree == q->firstout);
}
boolean_t
kdq_full(const kd_event_queue *q)
{
return(q_next(q->firstfree) == q->firstout);
}
void
kdq_put(kd_event_queue *q, kd_event *ev)
{
kd_event *qp = q->events + q->firstfree;
qp->type = ev->type;
qp->unused_time = ev->unused_time;
qp->value = ev->value;
q->firstfree = q_next(q->firstfree);
}
kd_event *
kdq_get(kd_event_queue *q)
{
kd_event *result = q->events + q->firstout;
q->firstout = q_next(q->firstout);
return(result);
}
void
kdq_reset(kd_event_queue *q)
{
q->firstout = q->firstfree = 0;
}