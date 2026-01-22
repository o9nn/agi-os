#ifndef	_I386_SEG_H_
#define	_I386_SEG_H_
#include <mach/inline.h>
#include <mach/machine/vm_types.h>
#include <i386/constants.h>
#ifdef	MACH_RING1
#define	KERNEL_RING	1
#else
#define	KERNEL_RING	0
#endif
#ifndef __ASSEMBLER__
struct real_descriptor {
unsigned int	limit_low:16,
base_low:16,
base_med:8,
access:8,
limit_high:4,
granularity:4,
base_high:8;
};
typedef struct real_descriptor real_descriptor_t;
typedef real_descriptor_t *real_descriptor_list_t;
typedef const real_descriptor_list_t const_real_descriptor_list_t;
#ifdef __x86_64__
struct real_descriptor64 {
unsigned int	limit_low:16,
base_low:16,
base_med:8,
access:8,
limit_high:4,
granularity:4,
base_high:8,
base_ext:32,
reserved1:8,
zero:5,
reserved2:19;
};
#endif
struct real_gate {
unsigned int	offset_low:16,
selector:16,
word_count:8,
access:8,
offset_high:16;
#ifdef __x86_64__
unsigned int	offset_ext:32,
reserved:32;
#endif
};
#endif
#define	SZ_64		0x2
#define	SZ_32		0x4
#define SZ_16		0x0
#define	SZ_G		0x8
#define	ACC_A		0x01
#define	ACC_TYPE	0x1e
#define	ACC_TYPE_SYSTEM	0x00
#define	ACC_LDT		0x02
#define	ACC_CALL_GATE_16 0x04
#define	ACC_TASK_GATE	0x05
#define	ACC_TSS		0x09
#define	ACC_CALL_GATE	0x0c
#define	ACC_INTR_GATE	0x0e
#define	ACC_TRAP_GATE	0x0f
#define	ACC_TSS_BUSY	0x02
#define	ACC_TYPE_USER	0x10
#define	ACC_DATA	0x10
#define	ACC_DATA_W	0x12
#define	ACC_DATA_E	0x14
#define	ACC_DATA_EW	0x16
#define	ACC_CODE	0x18
#define	ACC_CODE_R	0x1a
#define	ACC_CODE_C	0x1c
#define	ACC_CODE_CR	0x1e
#define	ACC_PL		0x60
#define	ACC_PL_K	(KERNEL_RING << 5)
#define	ACC_PL_U	0x60
#define	ACC_P		0x80
#define	SEL_LDT		0x04
#define	SEL_PL		0x03
#define	SEL_PL_K	KERNEL_RING
#define	SEL_PL_U	0x03
#define	sel_idx(sel)	((sel)>>3)
#ifndef __ASSEMBLER__
#include <mach/inline.h>
#include <mach/xen.h>
struct pseudo_descriptor
{
unsigned short limit;
unsigned long linear_base;
short pad;
} __attribute__((packed));
static inline void lgdt(struct pseudo_descriptor *pdesc)
{
__asm volatile("lgdt %0" : : "m" (*pdesc));
}
static inline void lidt(struct pseudo_descriptor *pdesc)
{
__asm volatile("lidt %0" : : "m" (*pdesc));
}
static inline void lldt(unsigned short ldt_selector)
{
__asm volatile("lldt %w0" : : "r" (ldt_selector) : "memory");
}
#ifdef CODE16
#define i16_lgdt lgdt
#define i16_lidt lidt
#define i16_lldt lldt
#endif
static inline void
fill_descriptor(struct real_descriptor *_desc, vm_offset_t base, vm_offset_t limit,
unsigned char access, unsigned char sizebits)
{
#ifdef	MACH_PV_DESCRIPTORS
struct real_descriptor __desc, *desc = &__desc;
#else
struct real_descriptor *desc = _desc;
#endif
if (limit > LIMIT_20BIT_MASK)
{
limit >>= 12;
sizebits |= SZ_G;
}
desc->limit_low = limit & WORD_MASK;
desc->base_low = base & WORD_MASK;
desc->base_med = (base >> 16) & BYTE_MASK;
desc->access = access | ACC_P;
desc->limit_high = limit >> 16;
desc->granularity = sizebits;
desc->base_high = base >> 24;
#ifdef	MACH_PV_DESCRIPTORS
union {
struct real_descriptor real_desc;
uint64_t raw_desc;
} desc_union;
desc_union.real_desc = *desc;
if (hyp_do_update_descriptor(kv_to_ma(_desc), desc_union.raw_desc)) {
panic("couldn't update descriptor(%zu to %08lx%08lx)\n", (vm_offset_t) kv_to_ma(_desc),
(unsigned long)(desc_union.raw_desc >> 32), (unsigned long)(desc_union.raw_desc & 0xFFFFFFFF));
}
#endif
}
#ifdef __x86_64__
static inline void
fill_descriptor64(struct real_descriptor64 *_desc, unsigned long base, unsigned limit,
unsigned char access, unsigned char sizebits)
{
#ifdef	MACH_PV_DESCRIPTORS
struct real_descriptor64 __desc, *desc = &__desc;
#else
struct real_descriptor64 *desc = _desc;
#endif
if (limit > LIMIT_20BIT_MASK)
{
limit >>= 12;
sizebits |= SZ_G;
}
desc->limit_low = limit & WORD_MASK;
desc->base_low = base & WORD_MASK;
desc->base_med = (base >> 16) & BYTE_MASK;
desc->access = access | ACC_P;
desc->limit_high = limit >> 16;
desc->granularity = sizebits;
desc->base_high = base >> 24;
desc->base_ext = base >> 32;
desc->reserved1 = 0;
desc->zero = 0;
desc->reserved2 = 0;
#ifdef	MACH_PV_DESCRIPTORS
union {
struct real_descriptor64 real_desc;
uint64_t raw_desc[2];
} desc_union;
desc_union.real_desc = *desc;
if (hyp_do_update_descriptor(kv_to_ma(_desc), desc_union.raw_desc[0])) {
panic("couldn't update descriptor(%lu to %08lx%08lx)\n", (vm_offset_t) kv_to_ma(_desc),
(unsigned long)(desc_union.raw_desc[0] >> 32), (unsigned long)(desc_union.raw_desc[0] & 0xFFFFFFFF));
}
#endif
}
#endif
static inline void
fill_gate(struct real_gate *gate, unsigned long offset, unsigned short selector,
unsigned char access, unsigned char word_count)
{
gate->offset_low = offset & WORD_MASK;
gate->selector = selector;
gate->word_count = word_count;
gate->access = access | ACC_P;
gate->offset_high = (offset >> 16) & WORD_MASK;
#ifdef __x86_64__
gate->offset_ext = offset >> 32;
gate->reserved = 0;
#endif
}
#endif
#endif