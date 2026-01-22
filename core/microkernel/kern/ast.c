#include <kern/ast.h>
#include <kern/counters.h>
#include <kern/debug.h>
#include "cpu_number.h"
#include <kern/queue.h>
#include <kern/sched.h>
#include <kern/sched_prim.h>
#include <kern/thread.h>
#include <kern/processor.h>
#include <device/net_io.h>
#include <machine/spl.h>
#if MACH_FIXPRI
#include <mach/policy.h>
#endif
volatile ast_t need_ast[NCPUS];
void
ast_init(void)
{
#ifndef MACHINE_AST
int i;
for (i=0; i<NCPUS; i++)
need_ast[i] = 0;
#endif
}
void
ast_taken(void)
{
thread_t self = current_thread();
ast_t reasons;
reasons = need_ast[cpu_number()];
need_ast[cpu_number()] = AST_ZILCH;
(void) spl0();
if (reasons & AST_NETWORK)
net_ast();
if (self != current_processor()->idle_thread) {
#ifndef MIGRATING_THREADS
while (thread_should_halt(self))
thread_halt_self(thread_exception_return);
#endif
if ((reasons & AST_BLOCK) ||
csw_needed(self, current_processor())) {
counter(c_ast_taken_block++);
thread_block(thread_exception_return);
}
}
}
void
ast_check(void)
{
int mycpu = cpu_number();
processor_t myprocessor;
thread_t thread = current_thread();
run_queue_t rq;
spl_t s = splsched();
myprocessor = cpu_to_processor(mycpu);
switch(myprocessor->state) {
case PROCESSOR_OFF_LINE:
case PROCESSOR_IDLE:
case PROCESSOR_DISPATCHING:
break;
#if NCPUS > 1
case PROCESSOR_ASSIGN:
case PROCESSOR_SHUTDOWN:
ast_on(mycpu, AST_BLOCK);
break;
#endif
case PROCESSOR_RUNNING:
ast_propagate(thread, mycpu);
if (ast_needed(mycpu))
break;
if (thread->state & TH_SUSP || myprocessor->runq.count > 0) {
ast_on(mycpu, AST_BLOCK);
break;
}
#if MACH_FIXPRI
if (myprocessor->processor_set->policies & POLICY_FIXEDPRI) {
if (csw_needed(thread,myprocessor)) {
ast_on(mycpu, AST_BLOCK);
break;
}
else {
if (thread->policy == POLICY_FIXEDPRI)
myprocessor->first_quantum = TRUE;
}
}
else {
#endif
rq = &(myprocessor->processor_set->runq);
if (!(myprocessor->first_quantum) && (rq->count > 0)) {
queue_t q;
q = rq->runq + *(volatile int *)&rq->low;
if (queue_empty(q)) {
runq_lock(rq);
q = rq->runq + rq->low;
if (rq->count > 0) {
int i;
for (i = rq->low; i < NRQS; i++) {
if(!(queue_empty(q)))
break;
q++;
}
rq->low = i;
}
runq_unlock(rq);
}
if (rq->low <= thread->sched_pri) {
ast_on(mycpu, AST_BLOCK);
break;
}
}
#if MACH_FIXPRI
}
#endif
break;
default:
panic("ast_check: Bad processor state (cpu %d processor %p) state: %d",
mycpu, myprocessor, myprocessor->state);
}
(void) splx(s);
}