#ifndef _LINUX_TQUEUE_H
#define _LINUX_TQUEUE_H
#include <asm/bitops.h>
#include <asm/system.h>
struct tq_struct {
struct tq_struct *next;
int sync;
void (*routine)(void *);
void *data;
};
typedef struct tq_struct * task_queue;
#define DECLARE_TASK_QUEUE(q)  task_queue q = NULL
extern task_queue tq_timer, tq_immediate, tq_scheduler, tq_disk;
static __inline__ void queue_task_irq(struct tq_struct *bh_pointer,
task_queue *bh_list)
{
if (!set_bit(0,&bh_pointer->sync)) {
bh_pointer->next = *bh_list;
*bh_list = bh_pointer;
}
}
static __inline__ void queue_task_irq_off(struct tq_struct *bh_pointer,
task_queue *bh_list)
{
if (!(bh_pointer->sync & 1)) {
bh_pointer->sync = 1;
bh_pointer->next = *bh_list;
*bh_list = bh_pointer;
}
}
static __inline__ void queue_task(struct tq_struct *bh_pointer,
task_queue *bh_list)
{
if (!set_bit(0,&bh_pointer->sync)) {
unsigned long flags;
save_flags(flags);
cli();
bh_pointer->next = *bh_list;
*bh_list = bh_pointer;
restore_flags(flags);
}
}
static __inline__ void run_task_queue(task_queue *list)
{
struct tq_struct *p;
p = xchg(list,NULL);
while (p) {
void *arg;
void (*f) (void *);
struct tq_struct *save_p;
arg    = p -> data;
f      = p -> routine;
save_p = p;
p      = p -> next;
save_p -> sync = 0;
(*f)(arg);
}
}
#endif