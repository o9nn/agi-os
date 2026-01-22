#ifndef _KD_QUEUE_H_
#define _KD_QUEUE_H_
#include <mach/std_types.h>
#include <i386at/kd.h>
#define KDQSIZE	100
typedef struct {
kd_event events[KDQSIZE];
int firstfree, firstout;
} kd_event_queue;
extern void kdq_put(kd_event_queue *, kd_event *);
extern void kdq_reset(kd_event_queue *);
extern boolean_t kdq_empty(const kd_event_queue *);
extern boolean_t kdq_full(const kd_event_queue *);
extern kd_event *kdq_get(kd_event_queue *);
#endif