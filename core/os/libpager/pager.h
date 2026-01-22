#ifndef _HURD_PAGER_
#define _HURD_PAGER_
#include <hurd/ports.h>
struct user_pager_info;
struct pager_requests;
error_t
pager_start_workers (struct port_bucket *pager_bucket,
struct pager_requests **requests);
error_t
pager_inhibit_workers (struct pager_requests *requests);
void
pager_resume_workers (struct pager_requests *requests);
struct pager *
pager_create (struct user_pager_info *u_pager,
struct port_bucket *bucket,
boolean_t may_cache,
memory_object_copy_strategy_t copy_strategy,
boolean_t notify_on_evict);
struct pager *
pager_create_alloc (size_t u_pager_size,
struct port_bucket *bucket,
boolean_t may_cache,
memory_object_copy_strategy_t copy_strategy,
boolean_t notify_on_evict);
struct user_pager_info *
pager_get_upi (struct pager *p);
void
pager_sync (struct pager *pager,
int wait);
void
pager_sync_some (struct pager *pager,
vm_address_t start,
vm_size_t len,
int wait);
void
pager_flush (struct pager *pager,
int wait);
void
pager_flush_some (struct pager *pager,
vm_address_t start,
vm_size_t len,
int wait);
void
pager_return (struct pager *pager,
int wait);
void
pager_return_some (struct pager *pager,
vm_address_t start,
vm_size_t len,
int wait);
void
pager_offer_page (struct pager *pager,
int precious,
int writelock,
vm_offset_t page,
vm_address_t buf);
void
pager_change_attributes (struct pager *pager,
boolean_t may_cache,
memory_object_copy_strategy_t copy_strategy,
int wait);
mach_port_t
pager_get_port (struct pager *pager);
mach_port_t
pager_create_ro_port (struct pager *pager);
void
pager_shutdown (struct pager *pager);
error_t
pager_get_error (struct pager *p, vm_address_t addr);
error_t
pager_memcpy (struct pager *pager, memory_object_t memobj,
vm_offset_t offset, void *other, size_t *size,
vm_prot_t prot);
error_t
pager_read_page (struct user_pager_info *pager,
vm_offset_t page,
vm_address_t *buf,
int *write_lock);
error_t
pager_write_page (struct user_pager_info *pager,
vm_offset_t page,
vm_address_t buf);
error_t
pager_unlock_page (struct user_pager_info *pager,
vm_offset_t address);
void
pager_notify_evict (struct user_pager_info *pager,
vm_offset_t page);
error_t
pager_report_extent (struct user_pager_info *pager,
vm_address_t *offset,
vm_size_t *size);
void
pager_clear_user_data (struct user_pager_info *pager);
void
pager_dropweak (struct user_pager_info *p);
#endif