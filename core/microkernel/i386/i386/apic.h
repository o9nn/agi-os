#ifndef _IMPS_APIC_
#define _IMPS_APIC_
#ifndef __ASSEMBLER__
#include <stdint.h>
typedef struct ApicReg {
uint32_t r;
uint32_t p[3];
} ApicReg;
typedef struct ApicIoUnit {
ApicReg select;
ApicReg window;
ApicReg unused[2];
ApicReg eoi;
} ApicIoUnit;
struct ioapic_route_entry {
uint32_t vector : 8,
delvmode : 3,
destmode : 1,
delvstatus : 1,
polarity : 1,
irr : 1,
trigger : 1,
mask : 1,
reserved1 : 15;
uint32_t reserved2 : 24,
dest : 8;
} __attribute__ ((packed));
union ioapic_route_entry_union {
struct {
uint32_t lo;
uint32_t hi;
};
struct ioapic_route_entry both;
};
typedef union u_icr_low
{
uint32_t value[4];
struct
{
uint32_t r;
unsigned :32;
unsigned :32;
unsigned :32;
};
struct
{
unsigned vector: 8;
unsigned delivery_mode : 3;
unsigned destination_mode: 1;
unsigned delivery_status: 1;
unsigned :1;
unsigned level: 1;
unsigned trigger_mode: 1;
unsigned remote_read_status: 2;
unsigned destination_shorthand: 2;
unsigned :12;
};
} IcrLReg;
typedef union u_icr_high
{
uint32_t value[4];
struct
{
uint32_t r;
unsigned :32;
unsigned :32;
unsigned :32;
};
struct
{
unsigned :24;
unsigned destination_field :8;
};
} IcrHReg;
typedef enum e_icr_dest_shorthand
{
NO_SHORTHAND = 0,
SELF = 1,
ALL_INCLUDING_SELF = 2,
ALL_EXCLUDING_SELF = 3
} icr_dest_shorthand;
typedef enum e_icr_deliv_mode
{
FIXED = 0,
LOWEST_PRIORITY = 1,
SMI = 2,
NMI = 4,
INIT = 5,
STARTUP = 6,
} icr_deliv_mode;
typedef enum e_icr_dest_mode
{
PHYSICAL = 0,
LOGICAL = 1
} icr_dest_mode;
typedef enum e_icr_deliv_status
{
IDLE = 0,
SEND_PENDING = 1
} icr_deliv_status;
typedef enum e_icr_level
{
DE_ASSERT = 0,
ASSERT = 1
} icr_level;
typedef enum e_irc_trigger_mode
{
EDGE = 0,
LEVEL = 1
} irc_trigger_mode;
typedef struct ApicLocalUnit {
ApicReg reserved0;
ApicReg reserved1;
ApicReg apic_id;
ApicReg version;
ApicReg reserved4;
ApicReg reserved5;
ApicReg reserved6;
ApicReg reserved7;
ApicReg task_pri;
ApicReg arbitration_pri;
ApicReg processor_pri;
ApicReg eoi;
ApicReg remote;
ApicReg logical_dest;
ApicReg dest_format;
ApicReg spurious_vector;
ApicReg isr[8];
ApicReg tmr[8];
ApicReg irr[8];
ApicReg error_status;
ApicReg reserved28[6];
ApicReg lvt_cmci;
IcrLReg icr_low;
IcrHReg icr_high;
ApicReg lvt_timer;
ApicReg lvt_thermal;
ApicReg lvt_performance_monitor;
ApicReg lvt_lint0;
ApicReg lvt_lint1;
ApicReg lvt_error;
ApicReg init_count;
ApicReg cur_count;
ApicReg reserved3a;
ApicReg reserved3b;
ApicReg reserved3c;
ApicReg reserved3d;
ApicReg divider_config;
ApicReg reserved3f;
ApicReg extended_feature;
ApicReg extended_control;
ApicReg specific_eoi;
} ApicLocalUnit;
#define APIC_VERSION_HAS_EXT_APIC_SPACE (1 << 31)
#define APIC_VERSION_HAS_DIRECTED_EOI (1 << 24)
#define APIC_EXT_FEATURE_HAS_SEOI (1 << 1)
#define APIC_EXT_FEATURE_HAS_8BITID (1 << 2)
#define APIC_EXT_CTRL_ENABLE_SEOI (1 << 1)
#define APIC_EXT_CTRL_ENABLE_8BITID (1 << 2)
typedef struct IoApicData {
uint8_t apic_id;
uint8_t ngsis;
uint32_t addr;
uint32_t gsi_base;
ApicIoUnit *ioapic;
} IoApicData;
#define APIC_IRQ_OVERRIDE_POLARITY_MASK 1
#define APIC_IRQ_OVERRIDE_ACTIVE_LOW 2
#define APIC_IRQ_OVERRIDE_TRIGGER_MASK 4
#define APIC_IRQ_OVERRIDE_LEVEL_TRIGGERED 8
typedef struct IrqOverrideData {
uint8_t bus;
uint8_t irq;
uint32_t gsi;
uint16_t flags;
} IrqOverrideData;
#define MAX_IOAPICS 16
#define MAX_IRQ_OVERRIDE 24
typedef struct ApicInfo {
uint8_t ncpus;
uint8_t nioapics;
int nirqoverride;
uint16_t* cpu_lapic_list;
struct IoApicData ioapic_list[MAX_IOAPICS];
struct IrqOverrideData irq_override_list[MAX_IRQ_OVERRIDE];
} ApicInfo;
struct irqinfo {
uint8_t trigger;
uint8_t vector;
};
int apic_data_init(void);
void apic_add_cpu(uint16_t apic_id);
void apic_lapic_init(ApicLocalUnit* lapic_ptr);
void apic_add_ioapic(struct IoApicData);
void apic_add_irq_override(struct IrqOverrideData irq_over);
void apic_send_ipi(unsigned dest_shorthand, unsigned deliv_mode, unsigned dest_mode, unsigned level, unsigned trig_mode, unsigned vector, unsigned dest_id);
IrqOverrideData *acpi_get_irq_override(uint8_t gsi);
int apic_get_cpu_apic_id(int kernel_id);
int apic_get_cpu_kernel_id(uint16_t apic_id);
volatile ApicLocalUnit* apic_get_lapic(void);
struct IoApicData *apic_get_ioapic(int kernel_id);
uint8_t apic_get_numcpus(void);
uint8_t apic_get_num_ioapics(void);
int apic_get_current_cpu(void);
void apic_print_info(void);
int apic_refit_cpulist(void);
void apic_generate_cpu_id_lut(void);
int apic_get_total_gsis(void);
void picdisable(void);
void lapic_eoi(void);
void ioapic_irq_eoi(int pin);
void fix_apic_id_mask(void);
void lapic_setup(void);
void lapic_disable(void);
void lapic_enable(void);
void lapic_enable_timer(void);
void calibrate_lapic_timer(void);
void ioapic_toggle(int pin, int mask);
void ioapic_configure(void);
void hpet_init(void);
void hpet_udelay(uint32_t us);
void hpet_mdelay(uint32_t ms);
extern int timer_pin;
extern void intnull(int unit);
extern volatile ApicLocalUnit* lapic;
extern int cpu_id_lut[];
extern uint32_t *hpet_addr;
extern uint8_t apic_id_mask;
extern struct irqinfo irqinfo[];
#endif
#define APIC_IO_UNIT_ID 0x00
#define APIC_IO_VERSION 0x01
# define APIC_IO_VERSION_SHIFT 0
# define APIC_IO_ENTRIES_SHIFT 16
#define APIC_IO_REDIR_LOW(int_pin) (0x10+(int_pin)*2)
#define APIC_IO_REDIR_HIGH(int_pin) (0x11+(int_pin)*2)
#define IMCR_SELECT 0x22
#define IMCR_DATA 0x23
#define MODE_IMCR 0x70
# define IMCR_USE_PIC 0
# define IMCR_USE_APIC 1
#define LAPIC_LOW_PRIO 0x100
#define LAPIC_NMI 0x400
#define LAPIC_EXTINT 0x700
#define LAPIC_LEVEL_TRIGGERED 0x8000
#define LAPIC_ENABLE 0x100
#define LAPIC_FOCUS 0x200
#define LAPIC_ENABLE_DIRECTED_EOI 0x1000
#define LAPIC_DISABLE 0x10000
#define LAPIC_TIMER_PERIODIC 0x20000
#define LAPIC_TIMER_DIVIDE_2 0
#define LAPIC_TIMER_DIVIDE_4 1
#define LAPIC_TIMER_DIVIDE_8 2
#define LAPIC_TIMER_DIVIDE_16 3
#define LAPIC_TIMER_BASEDIV 0x100000
#define NINTR 64
#define IOAPIC_FIXED 0
#define IOAPIC_PHYSICAL 0
#define IOAPIC_LOGICAL 1
#define IOAPIC_NMI 4
#define IOAPIC_EXTINT 7
#define IOAPIC_ACTIVE_HIGH 0
#define IOAPIC_ACTIVE_LOW 1
#define IOAPIC_EDGE_TRIGGERED 0
#define IOAPIC_LEVEL_TRIGGERED 1
#define IOAPIC_MASK_ENABLED 0
#define IOAPIC_MASK_DISABLED 1
#define APIC_MSR 0x1b
#define APIC_MSR_BSP 0x100
#define APIC_MSR_X2APIC 0x400
#define APIC_MSR_ENABLE 0x800
#define APIC_LOGICAL_CPU_GROUPS 8
#define APIC_LOGICAL_ID(cpu) (1u << ((cpu) % APIC_LOGICAL_CPU_GROUPS))
#define APIC_SET_MASK_BIT(reg, bit) \
((reg)[(bit) >> 5].r |= 1 << ((bit) & 0x1f))
#define APIC_CLEAR_MASK_BIT(reg, bit) \
((reg)[(bit) >> 5].r &= ~(1 << ((bit) & 0x1f)))
#ifndef __ASSEMBLER__
#ifdef APIC
static inline void mask_irq (unsigned int irq_nr) {
ioapic_toggle(irq_nr, IOAPIC_MASK_DISABLED);
}
static inline void unmask_irq (unsigned int irq_nr) {
ioapic_toggle(irq_nr, IOAPIC_MASK_ENABLED);
}
#endif
#endif
#endif