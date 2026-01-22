#ifndef	_I386_TASK_H_
#define _I386_TASK_H_
#include <kern/kern_types.h>
#include <kern/slab.h>
struct machine_task
{
decl_simple_lock_data (, iopb_lock);
int iopb_size;
unsigned char *iopb;
};
typedef struct machine_task machine_task_t;
extern struct kmem_cache machine_task_iopb_cache;
void machine_task_module_init (void);
void machine_task_init (task_t);
void machine_task_terminate (task_t);
void machine_task_collect (task_t);
#endif