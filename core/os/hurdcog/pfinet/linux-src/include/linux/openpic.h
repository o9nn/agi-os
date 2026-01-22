#ifndef _LINUX_OPENPIC_H
#define _LINUX_OPENPIC_H
#if !defined(__powerpc__) && !defined(__i386__)
#error Unsupported OpenPIC platform
#endif
#ifdef __KERNEL__
#define OPENPIC_MAX_SOURCES 2048
#define OPENPIC_MAX_PROCESSORS 32
#define OPENPIC_NUM_TIMERS 4
#define OPENPIC_NUM_IPI 4
#define OPENPIC_NUM_PRI 16
#define OPENPIC_NUM_VECTORS 256
#define OPENPIC_VEC_TIMER 64
#define OPENPIC_VEC_IPI 70
#define OPENPIC_VEC_SPURIOUS 127
typedef struct _OpenPIC_Reg {
u_int Reg;
char Pad[0xc];
} OpenPIC_Reg;
typedef struct _OpenPIC_Processor {
u_int IPI0_Dispatch_Shadow;
char Pad1[0x4];
u_int IPI0_Vector_Priority_Shadow;
char Pad2[0x34];
OpenPIC_Reg _IPI_Dispatch[OPENPIC_NUM_IPI];
OpenPIC_Reg _Current_Task_Priority;
#ifndef __powerpc__
OpenPIC_Reg _Who_Am_I;
#else
char Pad3[0x10];
#endif
#ifndef __i386__
OpenPIC_Reg _Interrupt_Acknowledge;
#else
char Pad4[0x10];
#endif
OpenPIC_Reg _EOI;
char Pad5[0xf40];
} OpenPIC_Processor;
typedef struct _OpenPIC_Timer {
OpenPIC_Reg _Current_Count;
OpenPIC_Reg _Base_Count;
OpenPIC_Reg _Vector_Priority;
OpenPIC_Reg _Destination;
} OpenPIC_Timer;
typedef struct _OpenPIC_Global {
OpenPIC_Reg _Feature_Reporting0;
OpenPIC_Reg _Feature_Reporting1;
OpenPIC_Reg _Global_Configuration0;
OpenPIC_Reg _Global_Configuration1;
OpenPIC_Reg _Vendor_Specific[4];
OpenPIC_Reg _Vendor_Identification;
OpenPIC_Reg _Processor_Initialization;
OpenPIC_Reg _IPI_Vector_Priority[OPENPIC_NUM_IPI];
OpenPIC_Reg _Spurious_Vector;
OpenPIC_Reg _Timer_Frequency;
OpenPIC_Timer Timer[OPENPIC_NUM_TIMERS];
char Pad1[0xee00];
} OpenPIC_Global;
typedef struct _OpenPIC_Source {
OpenPIC_Reg _Vector_Priority;
OpenPIC_Reg _Destination;
} OpenPIC_Source;
struct OpenPIC {
#ifndef __powerpc__
OpenPIC_Processor Private;
#else
char Pad1[0x1000];
#endif
OpenPIC_Global Global;
OpenPIC_Source Source[OPENPIC_MAX_SOURCES];
OpenPIC_Processor Processor[OPENPIC_MAX_PROCESSORS];
};
extern volatile struct OpenPIC *OpenPIC;
extern u_int OpenPIC_NumInitSenses;
extern u_char *OpenPIC_InitSenses;
#define OPENPIC_CURRENT_TASK_PRIORITY_MASK 0x0000000f
#define OPENPIC_WHO_AM_I_ID_MASK 0x0000001f
#define OPENPIC_FEATURE_LAST_SOURCE_MASK 0x07ff0000
#define OPENPIC_FEATURE_LAST_SOURCE_SHIFT 16
#define OPENPIC_FEATURE_LAST_PROCESSOR_MASK 0x00001f00
#define OPENPIC_FEATURE_LAST_PROCESSOR_SHIFT 8
#define OPENPIC_FEATURE_VERSION_MASK 0x000000ff
#define OPENPIC_CONFIG_RESET 0x80000000
#define OPENPIC_CONFIG_8259_PASSTHROUGH_DISABLE 0x20000000
#define OPENPIC_CONFIG_BASE_MASK 0x000fffff
#define OPENPIC_VENDOR_ID_STEPPING_MASK 0x00ff0000
#define OPENPIC_VENDOR_ID_STEPPING_SHIFT 16
#define OPENPIC_VENDOR_ID_DEVICE_ID_MASK 0x0000ff00
#define OPENPIC_VENDOR_ID_DEVICE_ID_SHIFT 8
#define OPENPIC_VENDOR_ID_VENDOR_ID_MASK 0x000000ff
#define OPENPIC_MASK 0x80000000
#define OPENPIC_ACTIVITY 0x40000000
#define OPENPIC_PRIORITY_MASK 0x000f0000
#define OPENPIC_PRIORITY_SHIFT 16
#define OPENPIC_VECTOR_MASK 0x000000ff
#define OPENPIC_SENSE_POLARITY 0x00800000
#define OPENPIC_SENSE_LEVEL 0x00400000
#define OPENPIC_COUNT_MASK 0x7fffffff
#define OPENPIC_TIMER_TOGGLE 0x80000000
#define OPENPIC_TIMER_COUNT_INHIBIT 0x80000000
#define IPI_Dispatch(i) _IPI_Dispatch[i].Reg
#define Current_Task_Priority _Current_Task_Priority.Reg
#ifndef __powerpc__
#define Who_Am_I _Who_Am_I.Reg
#endif
#ifndef __i386__
#define Interrupt_Acknowledge _Interrupt_Acknowledge.Reg
#endif
#define EOI _EOI.Reg
#define Feature_Reporting0 _Feature_Reporting0.Reg
#define Feature_Reporting1 _Feature_Reporting1.Reg
#define Global_Configuration0 _Global_Configuration0.Reg
#define Global_Configuration1 _Global_Configuration1.Reg
#define Vendor_Specific(i) _Vendor_Specific[i].Reg
#define Vendor_Identification _Vendor_Identification.Reg
#define Processor_Initialization _Processor_Initialization.Reg
#define IPI_Vector_Priority(i) _IPI_Vector_Priority[i].Reg
#define Spurious_Vector _Spurious_Vector.Reg
#define Timer_Frequency _Timer_Frequency.Reg
#define Current_Count _Current_Count.Reg
#define Base_Count _Base_Count.Reg
#define Vector_Priority _Vector_Priority.Reg
#define Destination _Destination.Reg
#define Vector_Priority _Vector_Priority.Reg
#define Destination _Destination.Reg
extern void openpic_init(int);
extern void openpic_reset(void);
extern void openpic_enable_8259_pass_through(void);
extern void openpic_disable_8259_pass_through(void);
#ifndef __i386__
extern u_int openpic_irq(u_int cpu);
#endif
#ifndef __powerpc__
extern void openpic_eoi(void);
extern u_int openpic_get_priority(void);
extern void openpic_set_priority(u_int pri);
#else
extern void openpic_eoi(u_int cpu);
extern u_int openpic_get_priority(u_int cpu);
extern void openpic_set_priority(u_int cpu, u_int pri);
#endif
extern u_int openpic_get_spurious(void);
extern void openpic_set_spurious(u_int vector);
extern void openpic_init_processor(u_int cpumask);
extern void openpic_initipi(u_int ipi, u_int pri, u_int vector);
#ifndef __powerpc__
extern void openpic_cause_IPI(u_int ipi, u_int cpumask);
#else
extern void openpic_cause_IPI(u_int cpu, u_int ipi, u_int cpumask);
#endif
extern void openpic_inittimer(u_int timer, u_int pri, u_int vector);
extern void openpic_maptimer(u_int timer, u_int cpumask);
extern void openpic_enable_irq(u_int irq);
extern void openpic_disable_irq(u_int irq);
extern void openpic_initirq(u_int irq, u_int pri, u_int vector, int polarity,
int is_level);
extern void openpic_mapirq(u_int irq, u_int cpumask);
extern void openpic_set_sense(u_int irq, int sense);
#endif
#endif