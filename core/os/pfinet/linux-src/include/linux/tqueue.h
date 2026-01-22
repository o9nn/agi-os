#ifndef _LINUX_TQUEUE_H
#define _LINUX_TQUEUE_H
#include <asm/bitops.h>
#include <asm/system.h>
#include <asm/spinlock.h>
struct tq_struct {
struct tq_struct *next;
unsigned long sync;
void (*routine)(void *);
void *data;
};
typedef struct tq_struct * task_queue;
#define DECLARE_TASK_QUEUE(q) task_queue q = NULL
extern task_queue tq_timer, tq_immediate, tq_scheduler, tq_disk;
extern spinlock_t tqueue_lock;
static __inline__ void queue_task(struct tq_struct *bh_pointer,
task_queue *bh_list)
{
if (!test_and_set_bit(0,&bh_pointer->sync)) {
unsigned long flags;
spin_lock_irqsave(&tqueue_lock, flags);
bh_pointer->next = *bh_list;
*bh_list = bh_pointer;
spin_unlock_irqrestore(&tqueue_lock, flags);
}
}
static __inline__ void run_task_queue(task_queue *list)
{
if (*list) {
unsigned long flags;
struct tq_struct *p;
spin_lock_irqsave(&tqueue_lock, flags);
p = *list;
*list = NULL;
spin_unlock_irqrestore(&tqueue_lock, flags);
while (p) {
void *arg;
void (*f) (void *);
struct tq_struct *save_p;
arg = p -> data;
f = p -> routine;
save_p = p;
p = p -> next;
mb();
save_p -> sync = 0;
(*f)(arg);
}
}
}
#endif