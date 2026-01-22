#ifndef gsnotify_INCLUDED
# define gsnotify_INCLUDED
#include "gsstype.h"
#define GS_NOTIFY_PROC(proc)\
int proc(void *proc_data, void *event_data)
typedef GS_NOTIFY_PROC((*gs_notify_proc_t));
typedef struct gs_notify_registration_s gs_notify_registration_t;
struct gs_notify_registration_s {
gs_notify_proc_t proc;
void *proc_data;
gs_notify_registration_t *next;
};
#define private_st_gs_notify_registration() \
gs_private_st_ptrs2(st_gs_notify_registration, gs_notify_registration_t,\
"gs_notify_registration_t", notify_registration_enum_ptrs,\
notify_registration_reloc_ptrs, proc_data, next)
typedef struct gs_notify_list_s {
gs_memory_t *memory;
gs_notify_registration_t *first;
} gs_notify_list_t;
extern_st(st_gs_notify_list);
#define public_st_gs_notify_list() \
gs_public_st_ptrs1(st_gs_notify_list, gs_notify_list_t,\
"gs_notify_list_t", notify_list_enum_ptrs, notify_list_reloc_ptrs,\
first)
#define st_gs_notify_list_max_ptrs 1
void gs_notify_init(gs_notify_list_t *nlist, gs_memory_t *mem);
int gs_notify_register(gs_notify_list_t *nlist, gs_notify_proc_t proc,
void *proc_data);
int gs_notify_unregister(gs_notify_list_t *nlist, gs_notify_proc_t proc,
void *proc_data);
int gs_notify_unregister_calling(gs_notify_list_t *nlist,
gs_notify_proc_t proc, void *proc_data,
void (*unreg_proc)(void *pdata));
int gs_notify_all(gs_notify_list_t *nlist, void *event_data);
void gs_notify_release(gs_notify_list_t *nlist);
#endif