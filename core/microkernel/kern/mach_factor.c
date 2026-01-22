#include <mach/machine.h>
#include <mach/processor_info.h>
#include <kern/mach_clock.h>
#include <kern/sched.h>
#include <kern/processor.h>
#include <mach/kern_return.h>
#include <mach/port.h>
#include "mach_factor.h"
long avenrun[3] = {0, 0, 0};
long mach_factor[3] = {0, 0, 0};
static long fract[3] = {
800,
966,
983,
};
void compute_mach_factor(void)
{
processor_set_t pset;
processor_t processor;
int ncpus;
int nthreads;
long factor_now;
long average_now;
long load_now;
simple_lock(&all_psets_lock);
pset = (processor_set_t) queue_first(&all_psets);
while (!queue_end(&all_psets, (queue_entry_t)pset)) {
pset_lock(pset);
if ((ncpus = pset->processor_count) > 0) {
nthreads = pset->runq.count;
processor = (processor_t) queue_first(&pset->processors);
while (!queue_end(&pset->processors,
(queue_entry_t)processor)) {
nthreads += processor->runq.count;
processor =
(processor_t) queue_next(&processor->processors);
}
nthreads += ncpus - pset->idle_count;
if (pset == &default_pset)
nthreads -= 1;
if (nthreads > ncpus) {
factor_now = (ncpus * LOAD_SCALE) / (nthreads + 1);
load_now = (nthreads << SCHED_SHIFT) / ncpus;
}
else {
factor_now = (ncpus - nthreads) * LOAD_SCALE;
load_now = SCHED_SCALE;
}
average_now = nthreads * LOAD_SCALE;
pset->mach_factor =
((pset->mach_factor << 2) + factor_now)/5;
pset->load_average =
((pset->load_average << 2) + average_now)/5;
if (pset == &default_pset) {
int i;
for (i = 0; i < 3; i++) {
mach_factor[i] = ( (mach_factor[i]*fract[i])
+ (factor_now*(LOAD_SCALE-fract[i])) )
/ LOAD_SCALE;
avenrun[i] = ( (avenrun[i]*fract[i])
+ (average_now*(LOAD_SCALE-fract[i])) )
/ LOAD_SCALE;
}
}
pset->sched_load = (pset->sched_load + load_now) >> 1;
}
pset_unlock(pset);
pset = (processor_set_t) queue_next(&pset->all_psets);
}
simple_unlock(&all_psets_lock);
}