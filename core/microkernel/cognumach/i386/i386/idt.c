#include <i386/vm_param.h>
#include <i386/seg.h>
#include <i386at/idt.h>
#include <i386/gdt.h>
#include <i386/mp_desc.h>
struct real_gate idt[IDTSZ];
struct idt_init_entry
{
unsigned long entrypoint;
unsigned short vector;
unsigned short type;
#ifdef __x86_64__
unsigned short ist;
unsigned short pad_0;
#endif
};
extern struct idt_init_entry idt_inittab[];
static void
idt_fill(struct real_gate *myidt)
{
#ifdef	MACH_PV_DESCRIPTORS
if (hyp_set_trap_table(kvtolin(idt_inittab)))
panic("couldn't set trap table\n");
#else
struct idt_init_entry *iie = idt_inittab;
while (iie->entrypoint)
{
fill_idt_gate(myidt, iie->vector, iie->entrypoint, KERNEL_CS, iie->type,
#ifdef __x86_64__
iie->ist
#else
0
#endif
);
iie++;
}
{
struct pseudo_descriptor pdesc;
pdesc.limit = (IDTSZ * sizeof(struct real_gate))-1;
pdesc.linear_base = kvtolin(myidt);
lidt(&pdesc);
}
#endif
}
void idt_init(void)
{
idt_fill(idt);
}
#if NCPUS > 1
void ap_idt_init(int cpu)
{
idt_fill(mp_desc_table[cpu]->idt);
}
#endif