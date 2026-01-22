#ifndef _KERN_VDSO_H_
#define _KERN_VDSO_H_
#include <mach/mach_types.h>
#include <mach/vm_param.h>
#include <kern/kern_types.h>
#define VDSO_PAGE_SIZE		PAGE_SIZE
#define VDSO_MAX_SYMBOLS	16
#define VDSO_VERSION		1
typedef enum {
VDSO_SYM_GETTIMEOFDAY = 0,
VDSO_SYM_CLOCK_GETTIME,
VDSO_SYM_TIME,
VDSO_SYM_GETPID,
VDSO_SYM_COUNT
} vdso_symbol_type_t;
typedef struct vdso_symbol {
const char *name;
vm_offset_t offset;
boolean_t available;
} vdso_symbol_t;
typedef struct vdso_header {
uint32_t magic;
uint32_t version;
uint32_t symbol_count;
uint32_t reserved;
vdso_symbol_t symbols[VDSO_MAX_SYMBOLS];
} vdso_header_t;
typedef struct vdso_state {
vm_offset_t vdso_page;
vm_size_t vdso_size;
boolean_t initialized;
vdso_header_t *header;
} vdso_state_t;
#define VDSO_MAGIC 0x564453C0
extern void vdso_init(void);
extern kern_return_t vdso_bootstrap(void);
extern kern_return_t vdso_map_into_task(task_t task, vm_offset_t *address);
extern void vdso_unmap_from_task(task_t task);
extern vm_offset_t vdso_lookup_symbol(vdso_symbol_type_t symbol);
extern kern_return_t vdso_add_symbol(vdso_symbol_type_t type,
const char *name,
vm_offset_t offset);
extern int vdso_gettimeofday(time_value_t *tv);
extern int vdso_clock_gettime(int clock_id, time_value_t *tp);
extern long vdso_time(long *t);
extern int vdso_getpid(void);
extern vdso_state_t *vdso_get_state(void);
extern boolean_t vdso_is_available(void);
extern void vdso_arch_init(void);
extern kern_return_t vdso_arch_setup_page(vm_offset_t page);
#define VDSO_USER_BASE  0x7ffff000UL
#endif