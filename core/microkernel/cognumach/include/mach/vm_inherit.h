#ifndef	_MACH_VM_INHERIT_H_
#define	_MACH_VM_INHERIT_H_
typedef int		vm_inherit_t;
#define	VM_INHERIT_SHARE	((vm_inherit_t) 0)
#define	VM_INHERIT_COPY		((vm_inherit_t) 1)
#define VM_INHERIT_NONE		((vm_inherit_t) 2)
#define VM_INHERIT_DEFAULT	VM_INHERIT_COPY
#endif