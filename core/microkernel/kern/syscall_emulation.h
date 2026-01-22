#ifndef _KERN_SYSCALL_EMULATION_H_
#define _KERN_SYSCALL_EMULATION_H_
#ifndef __ASSEMBLER__
#include <mach/machine/vm_types.h>
#include <kern/lock.h>
#include <kern/task.h>
typedef vm_offset_t eml_routine_t;
typedef struct eml_dispatch {
decl_simple_lock_data(, lock)
int ref_count;
int disp_count;
int disp_min;
eml_routine_t disp_vector[1];
} *eml_dispatch_t;
typedef vm_offset_t *emulation_vector_t;
#define EML_ROUTINE_NULL (eml_routine_t)0
#define EML_DISPATCH_NULL (eml_dispatch_t)0
#define EML_SUCCESS (0)
#define EML_MOD (err_kern|err_sub(2))
#define EML_BAD_TASK (EML_MOD|0x0001)
#define EML_BAD_CNT (EML_MOD|0x0002)
extern void eml_init(void);
extern void eml_task_reference(task_t task, task_t parent);
extern void eml_task_deallocate(task_t task);
#endif
#endif