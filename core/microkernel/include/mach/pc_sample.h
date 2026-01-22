#ifndef	_MACH_PC_SAMPLE_H_
#define _MACH_PC_SAMPLE_H_
#include <mach/machine/vm_types.h>
typedef unsigned int	sampled_pc_flavor_t;
#define SAMPLED_PC_PERIODIC			0x1
#define SAMPLED_PC_VM_ZFILL_FAULTS		0x10
#define SAMPLED_PC_VM_REACTIVATION_FAULTS	0x20
#define SAMPLED_PC_VM_PAGEIN_FAULTS		0x40
#define SAMPLED_PC_VM_COW_FAULTS		0x80
#define SAMPLED_PC_VM_FAULTS_ANY		0x100
#define SAMPLED_PC_VM_FAULTS		\
(SAMPLED_PC_VM_ZFILL_FAULTS | \
SAMPLED_PC_VM_REACTIVATION_FAULTS |\
SAMPLED_PC_VM_PAGEIN_FAULTS |\
SAMPLED_PC_VM_COW_FAULTS )
typedef struct sampled_pc {
rpc_vm_offset_t		id;
rpc_vm_offset_t		pc;
sampled_pc_flavor_t sampletype;
} sampled_pc_t;
typedef sampled_pc_t *sampled_pc_array_t;
typedef unsigned int sampled_pc_seqno_t;
#endif