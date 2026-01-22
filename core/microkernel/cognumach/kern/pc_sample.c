#include <kern/printf.h>
#include <string.h>
#include <mach/mach_types.h>
#include <mach/std_types.h>
#include <mach/pc_sample.h>
#include <machine/trap.h>
#include <kern/kalloc.h>
#include <kern/host.h>
#include <kern/thread.h>
#include <kern/pc_sample.h>
#include <kern/mach4.server.h>
#include <kern/mach_clock.h>
#if	MACH_PCSAMPLE
#define MAX_PC_SAMPLES 512
typedef sampled_pc_t sampled_pcs[MAX_PC_SAMPLES];
void take_pc_sample(
const thread_t t,
sample_control_t *cp,
sampled_pc_flavor_t flavor,
boolean_t usermode,
vm_offset_t kern_pc)
{
vm_offset_t pc;
struct sampled_pc *sample;
if (usermode)
pc = interrupted_pc(t);
else
pc = kern_pc;
cp->seqno++;
sample = &((sampled_pc_t *)cp->buffer)[cp->seqno % MAX_PC_SAMPLES];
sample->id = (rpc_vm_offset_t)(vm_offset_t)t;
sample->pc = (rpc_vm_offset_t)pc;
sample->sampletype = flavor;
}
kern_return_t
thread_enable_pc_sampling(
thread_t thread,
int *tickp,
sampled_pc_flavor_t flavors)
{
vm_offset_t buf;
if (thread == THREAD_NULL)  {
return KERN_INVALID_ARGUMENT;
}
if (thread->pc_sample.buffer == 0)  {
buf = (vm_offset_t) kalloc(sizeof (sampled_pcs));
if (buf == 0) {
printf("thread_enable_pc_sampling: kalloc failed\n");
return KERN_INVALID_ARGUMENT;
}
thread->pc_sample.buffer = buf;
thread->pc_sample.seqno = 0;
}
*tickp = tick;
thread->pc_sample.sampletypes = flavors;
return KERN_SUCCESS;
}
kern_return_t
task_enable_pc_sampling(
task_t task,
int *tickp,
sampled_pc_flavor_t flavors)
{
vm_offset_t buf;
if (task == TASK_NULL)  {
return KERN_INVALID_ARGUMENT;
}
if (task->pc_sample.buffer == 0)  {
buf = (vm_offset_t) kalloc(sizeof (sampled_pcs));
if (buf == 0) {
printf("task_enable_pc_sampling: kalloc failed\n");
return KERN_INVALID_ARGUMENT;
}
task->pc_sample.buffer = buf;
task->pc_sample.seqno = 0;
}
*tickp = tick;
task->pc_sample.sampletypes = flavors;
return KERN_SUCCESS;
}
kern_return_t
thread_disable_pc_sampling(
thread_t thread,
int *samplecntp)
{
vm_offset_t buf;
if (thread == THREAD_NULL)  {
return KERN_INVALID_ARGUMENT;
}
if ((buf = thread->pc_sample.buffer) != 0)
kfree(buf, sizeof (sampled_pcs));
thread->pc_sample.buffer = (vm_offset_t) 0;
thread->pc_sample.seqno = 0;
thread->pc_sample.sampletypes = 0;
return KERN_SUCCESS;
}
kern_return_t
task_disable_pc_sampling(
task_t task,
int *samplecntp)
{
vm_offset_t buf;
if (task == TASK_NULL)  {
return KERN_INVALID_ARGUMENT;
}
if ((buf = task->pc_sample.buffer) != 0)
kfree(buf, sizeof (sampled_pcs));
task->pc_sample.buffer = (vm_offset_t) 0;
task->pc_sample.seqno = 0;
task->pc_sample.sampletypes = 0;
return KERN_SUCCESS;
}
static kern_return_t
get_sampled_pcs(
sample_control_t *cp,
sampled_pc_seqno_t *seqnop,
sampled_pc_array_t sampled_pcs_out,
mach_msg_type_number_t *sampled_pcs_cntp)
{
int nsamples;
sampled_pc_seqno_t seqidx1, seqidx2;
nsamples	= cp->seqno - *seqnop;
seqidx1	= *seqnop % MAX_PC_SAMPLES;
seqidx2	= cp->seqno % MAX_PC_SAMPLES;
if  (nsamples > MAX_PC_SAMPLES) {
nsamples = MAX_PC_SAMPLES;
seqidx1  = (seqidx2 + 1) % MAX_PC_SAMPLES;
}
if (nsamples > 0)  {
if (seqidx1 < seqidx2) {
memcpy(sampled_pcs_out,
(sampled_pc_array_t)cp->buffer + seqidx1 + 1,
nsamples * sizeof(sampled_pc_t));
} else {
memcpy(sampled_pcs_out,
(sampled_pc_array_t)cp->buffer + seqidx1 + 1,
(MAX_PC_SAMPLES - seqidx1 - 1) * sizeof(sampled_pc_t));
memcpy(sampled_pcs_out + (MAX_PC_SAMPLES - seqidx1 - 1),
(sampled_pc_array_t)cp->buffer,
(seqidx2 + 1) * sizeof(sampled_pc_t));
}
} else if (nsamples < 0) {
nsamples = 0;
} else {
;
}
*sampled_pcs_cntp = nsamples;
*seqnop = cp->seqno;
return KERN_SUCCESS;
}
kern_return_t
thread_get_sampled_pcs(
thread_t thread,
sampled_pc_seqno_t *seqnop,
sampled_pc_array_t sampled_pcs_out,
mach_msg_type_number_t *sampled_pcs_cntp)
{
if (thread == THREAD_NULL)
return KERN_INVALID_ARGUMENT;
if (thread->pc_sample.buffer == 0)
return KERN_FAILURE;
return get_sampled_pcs(&thread->pc_sample, seqnop, sampled_pcs_out,
sampled_pcs_cntp);
}
kern_return_t
task_get_sampled_pcs(
task_t task,
sampled_pc_seqno_t *seqnop,
sampled_pc_array_t sampled_pcs_out,
mach_msg_type_number_t *sampled_pcs_cntp)
{
if (task == TASK_NULL)
return KERN_INVALID_ARGUMENT;
if (task->pc_sample.buffer == 0)
return KERN_FAILURE;
return get_sampled_pcs(&task->pc_sample, seqnop, sampled_pcs_out,
sampled_pcs_cntp);
}
#else
kern_return_t
thread_enable_pc_sampling(
thread_t thread,
int *tickp,
sampled_pc_flavor_t flavors)
{
return KERN_FAILURE;
}
kern_return_t
task_enable_pc_sampling(
task_t task,
int *tickp,
sampled_pc_flavor_t flavors)
{
return KERN_FAILURE;
}
kern_return_t
thread_disable_pc_sampling(
thread_t thread,
int *samplecntp)
{
return KERN_FAILURE;
}
kern_return_t
task_disable_pc_sampling(
task_t task,
int *samplecntp)
{
return KERN_FAILURE;
}
kern_return_t
thread_get_sampled_pcs(
thread_t thread,
sampled_pc_seqno_t *seqnop,
sampled_pc_array_t sampled_pcs_out,
mach_msg_type_number_t *sampled_pcs_cntp)
{
return KERN_FAILURE;
}
kern_return_t
task_get_sampled_pcs(
task_t task,
sampled_pc_seqno_t *seqnop,
sampled_pc_array_t sampled_pcs_out,
mach_msg_type_number_t *sampled_pcs_cntp)
{
return KERN_FAILURE;
}
#endif