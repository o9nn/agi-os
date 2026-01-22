#ifndef gsgc_INCLUDED
#  define gsgc_INCLUDED
typedef enum {
i_vm_foreign = 0,
i_vm_system,
i_vm_global,
i_vm_local,
i_vm_max = i_vm_local
} i_vm_space;
#ifndef gs_ref_memory_DEFINED
#  define gs_ref_memory_DEFINED
typedef struct gs_ref_memory_s gs_ref_memory_t;
#endif
#ifdef r_space_bits
#  if r_space_bits != 2
Error_r_space_bits_is_not_2;
#  endif
#endif
typedef struct vm_spaces_s vm_spaces;
#define vm_reclaim_proc(proc)\
void proc(vm_spaces *pspaces, bool global)
struct vm_spaces_s {
vm_reclaim_proc((*vm_reclaim));
union {
gs_ref_memory_t *indexed[4  ];
struct _ssn {
gs_ref_memory_t *foreign;
gs_ref_memory_t *system;
gs_ref_memory_t *global;
gs_ref_memory_t *local;
} named;
} memories;
};
#define space_foreign spaces.memories.named.foreign
#define space_system spaces.memories.named.system
#define space_global spaces.memories.named.global
#define space_local spaces.memories.named.local
#define spaces_indexed spaces.memories.indexed
#define GS_RECLAIM(pspaces, global) ((pspaces)->vm_reclaim(pspaces, global))
#define gs_reclaim(pspaces, global) GS_RECLAIM(pspaces, global)
#endif