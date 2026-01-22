#ifndef _MACH_MACHINE_H_
#define _MACH_MACHINE_H_
#include <mach/machine/vm_types.h>
#include <mach/boolean.h>
struct machine_info {
integer_t major_version;
integer_t minor_version;
integer_t max_cpus;
integer_t avail_cpus;
vm_size_t memory_size;
};
typedef struct machine_info *machine_info_t;
typedef struct machine_info machine_info_data_t;
typedef integer_t cpu_type_t;
typedef integer_t cpu_subtype_t;
#define CPU_STATE_MAX 3
#define CPU_STATE_USER 0
#define CPU_STATE_SYSTEM 1
#define CPU_STATE_IDLE 2
struct machine_slot {
integer_t is_cpu;
cpu_type_t cpu_type;
cpu_subtype_t cpu_subtype;
integer_t running;
integer_t cpu_ticks[CPU_STATE_MAX];
integer_t clock_freq;
};
typedef struct machine_slot *machine_slot_t;
typedef struct machine_slot machine_slot_data_t;
#ifdef MACH_KERNEL
extern struct machine_info machine_info;
extern struct machine_slot machine_slot[NCPUS];
#endif
#define CPU_TYPE_VAX ((cpu_type_t) 1)
#define CPU_TYPE_ROMP ((cpu_type_t) 2)
#define CPU_TYPE_MC68020 ((cpu_type_t) 3)
#define CPU_TYPE_NS32032 ((cpu_type_t) 4)
#define CPU_TYPE_NS32332 ((cpu_type_t) 5)
#define CPU_TYPE_NS32532 ((cpu_type_t) 6)
#define CPU_TYPE_I386 ((cpu_type_t) 7)
#define CPU_TYPE_MIPS ((cpu_type_t) 8)
#define CPU_TYPE_MC68030 ((cpu_type_t) 9)
#define CPU_TYPE_MC68040 ((cpu_type_t) 10)
#define CPU_TYPE_HPPA ((cpu_type_t) 11)
#define CPU_TYPE_ARM ((cpu_type_t) 12)
#define CPU_TYPE_MC88000 ((cpu_type_t) 13)
#define CPU_TYPE_SPARC ((cpu_type_t) 14)
#define CPU_TYPE_I860 ((cpu_type_t) 15)
#define CPU_TYPE_ALPHA ((cpu_type_t) 16)
#define CPU_TYPE_I486 ((cpu_type_t) 17)
#define CPU_TYPE_PENTIUM ((cpu_type_t) 18)
#define CPU_TYPE_PENTIUMPRO ((cpu_type_t) 19)
#define CPU_TYPE_POWERPC ((cpu_type_t) 20)
#define CPU_TYPE_X86_64 ((cpu_type_t) 21)
#define CPU_TYPE_ARM64 ((cpu_type_t) 22)
#define CPU_SUBTYPE_VAX780 ((cpu_subtype_t) 1)
#define CPU_SUBTYPE_VAX785 ((cpu_subtype_t) 2)
#define CPU_SUBTYPE_VAX750 ((cpu_subtype_t) 3)
#define CPU_SUBTYPE_VAX730 ((cpu_subtype_t) 4)
#define CPU_SUBTYPE_UVAXI ((cpu_subtype_t) 5)
#define CPU_SUBTYPE_UVAXII ((cpu_subtype_t) 6)
#define CPU_SUBTYPE_VAX8200 ((cpu_subtype_t) 7)
#define CPU_SUBTYPE_VAX8500 ((cpu_subtype_t) 8)
#define CPU_SUBTYPE_VAX8600 ((cpu_subtype_t) 9)
#define CPU_SUBTYPE_VAX8650 ((cpu_subtype_t) 10)
#define CPU_SUBTYPE_VAX8800 ((cpu_subtype_t) 11)
#define CPU_SUBTYPE_UVAXIII ((cpu_subtype_t) 12)
#define CPU_SUBTYPE_RT_PC ((cpu_subtype_t) 1)
#define CPU_SUBTYPE_RT_APC ((cpu_subtype_t) 2)
#define CPU_SUBTYPE_RT_135 ((cpu_subtype_t) 3)
#define CPU_SUBTYPE_SUN3_50 ((cpu_subtype_t) 1)
#define CPU_SUBTYPE_SUN3_160 ((cpu_subtype_t) 2)
#define CPU_SUBTYPE_SUN3_260 ((cpu_subtype_t) 3)
#define CPU_SUBTYPE_SUN3_110 ((cpu_subtype_t) 4)
#define CPU_SUBTYPE_SUN3_60 ((cpu_subtype_t) 5)
#define CPU_SUBTYPE_HP_320 ((cpu_subtype_t) 6)
#define CPU_SUBTYPE_HP_330 ((cpu_subtype_t) 7)
#define CPU_SUBTYPE_HP_350 ((cpu_subtype_t) 8)
#define CPU_SUBTYPE_MMAX_DPC ((cpu_subtype_t) 1)
#define CPU_SUBTYPE_SQT ((cpu_subtype_t) 2)
#define CPU_SUBTYPE_MMAX_APC_FPU ((cpu_subtype_t) 3)
#define CPU_SUBTYPE_MMAX_APC_FPA ((cpu_subtype_t) 4)
#define CPU_SUBTYPE_MMAX_XPC ((cpu_subtype_t) 5)
#define CPU_SUBTYPE_PC532 ((cpu_subtype_t) 6)
#define CPU_SUBTYPE_AT386 ((cpu_subtype_t) 1)
#define CPU_SUBTYPE_EXL ((cpu_subtype_t) 2)
#define CPU_SUBTYPE_iPSC386 ((cpu_subtype_t) 3)
#define CPU_SUBTYPE_SYMMETRY ((cpu_subtype_t) 4)
#define CPU_SUBTYPE_PS2 ((cpu_subtype_t) 5)
#define CPU_SUBTYPE_MIPS_R2300 ((cpu_subtype_t) 1)
#define CPU_SUBTYPE_MIPS_R2600 ((cpu_subtype_t) 2)
#define CPU_SUBTYPE_MIPS_R2800 ((cpu_subtype_t) 3)
#define CPU_SUBTYPE_MIPS_R2000a ((cpu_subtype_t) 4)
#define CPU_SUBTYPE_MIPS_R2000 ((cpu_subtype_t) 5)
#define CPU_SUBTYPE_MIPS_R3000a ((cpu_subtype_t) 6)
#define CPU_SUBTYPE_MIPS_R3000 ((cpu_subtype_t) 7)
#define CPU_SUBTYPE_NeXT ((cpu_subtype_t) 1)
#define CPU_SUBTYPE_HP_340 ((cpu_subtype_t) 2)
#define CPU_SUBTYPE_HP_360 ((cpu_subtype_t) 3)
#define CPU_SUBTYPE_HP_370 ((cpu_subtype_t) 4)
#define CPU_SUBTYPE_HPPA_825 ((cpu_subtype_t) 1)
#define CPU_SUBTYPE_HPPA_835 ((cpu_subtype_t) 2)
#define CPU_SUBTYPE_HPPA_840 ((cpu_subtype_t) 3)
#define CPU_SUBTYPE_HPPA_850 ((cpu_subtype_t) 4)
#define CPU_SUBTYPE_HPPA_855 ((cpu_subtype_t) 5)
#define CPU_SUBTYPE_ARM_A500_ARCH ((cpu_subtype_t) 1)
#define CPU_SUBTYPE_ARM_A500 ((cpu_subtype_t) 2)
#define CPU_SUBTYPE_ARM_A440 ((cpu_subtype_t) 3)
#define CPU_SUBTYPE_ARM_M4 ((cpu_subtype_t) 4)
#define CPU_SUBTYPE_ARM_A680 ((cpu_subtype_t) 5)
#define CPU_SUBTYPE_MMAX_JPC ((cpu_subtype_t) 1)
#define CPU_SUBTYPE_LUNA88K ((cpu_subtype_t) 2)
#define CPU_SUBTYPE_SUN4_260 ((cpu_subtype_t) 1)
#define CPU_SUBTYPE_SUN4_110 ((cpu_subtype_t) 2)
#define CPU_SUBTYPE_SUN4_330 ((cpu_subtype_t) 3)
#define CPU_SUBTYPE_SUN4C_60 ((cpu_subtype_t) 4)
#define CPU_SUBTYPE_SUN4C_65 ((cpu_subtype_t) 5)
#define CPU_SUBTYPE_SUN4C_20 ((cpu_subtype_t) 6)
#define CPU_SUBTYPE_SUN4C_30 ((cpu_subtype_t) 7)
#define CPU_SUBTYPE_SUN4C_40 ((cpu_subtype_t) 8)
#define CPU_SUBTYPE_SUN4C_50 ((cpu_subtype_t) 9)
#define CPU_SUBTYPE_SUN4C_75 ((cpu_subtype_t) 10)
#define CPU_SUBTYPE_iPSC860 ((cpu_subtype_t) 1)
#define CPU_SUBTYPE_OKI860 ((cpu_subtype_t) 2)
#define CPU_SUBTYPE_ALPHA_EV3 ((cpu_subtype_t) 1)
#define CPU_SUBTYPE_ALPHA_EV4 ((cpu_subtype_t) 2)
#define CPU_SUBTYPE_ALPHA_ISP ((cpu_subtype_t) 3)
#define CPU_SUBTYPE_ALPHA_21064 ((cpu_subtype_t) 4)
#endif