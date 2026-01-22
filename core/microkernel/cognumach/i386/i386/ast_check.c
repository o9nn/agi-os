#if	NCPUS > 1
#include <kern/ast.h>
#include <kern/processor.h>
#include <kern/smp.h>
#include <machine/cpu_number.h>
#include <machine/apic.h>
void init_ast_check(const processor_t processor)
{
}
void cause_ast_check(const processor_t processor)
{
smp_remote_ast(APIC_LOGICAL_ID(processor->slot_num));
}
#endif