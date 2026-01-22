#ifndef	_KERN_PC_SAMPLE_H_
#define	_KERN_PC_SAMPLE_H_
#include <mach/pc_sample.h>
#include <mach/machine/vm_types.h>
#include <kern/kern_types.h>
#include <kern/macros.h>
struct sample_control {
vm_offset_t		buffer;
unsigned int	seqno;
sampled_pc_flavor_t sampletypes;
};
typedef struct sample_control	sample_control_t;
extern void take_pc_sample(
thread_t	thread,
sample_control_t *cp,
sampled_pc_flavor_t flavor,
boolean_t usermode,
vm_offset_t pc);
#define	take_pc_sample_macro(thread, flavor, usermode, pc) \
MACRO_BEGIN \
task_t	task; \
\
if ((thread)->pc_sample.sampletypes & (flavor)) \
take_pc_sample((thread), &(thread)->pc_sample, (flavor), usermode, pc); \
\
task = (thread)->task; \
if (task->pc_sample.sampletypes & (flavor)) \
take_pc_sample((thread), &task->pc_sample, (flavor), usermode, pc); \
MACRO_END
#endif