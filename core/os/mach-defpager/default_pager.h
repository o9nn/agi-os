#ifndef _DEFAULT_PAGER_H_
#define _DEFAULT_PAGER_H_
#include <file_io.h>
extern mach_port_t default_pager_exception_port;
void default_pager(void);
void default_pager_initialize(mach_port_t host_port);
void partition_init(void);
void create_paging_partition(const char *name, struct file_direct *fdp,
int isa_file, int linux_signature);
kern_return_t destroy_paging_partition(const char *name, void **pp_private);
kern_return_t remove_paging_file (const char *file_name);
void paging_space_info(vm_size_t *totp, vm_size_t *freep);
void no_paging_space(boolean_t out_of_memory);
void overcommitted(boolean_t got_more_space, vm_size_t space);
void panic (const char *fmt, ...) __attribute__ ((noreturn));
#endif