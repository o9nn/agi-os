#ifndef PAGER_H
#define PAGER_H
struct user_pager
{
struct pager *pager;
memory_object_t memobj;
};
void user_pager_init (void);
error_t user_pager_create (struct user_pager *user_pager, unsigned int npages,
struct cons_display **user);
void user_pager_destroy (struct user_pager *user_pager,
struct cons_display *user);
mach_port_t user_pager_get_filemap (struct user_pager *user_pager,
vm_prot_t prot);
#endif