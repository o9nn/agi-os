#ifndef	_MACH_VM_ATTRIBUTES_H_
#define	_MACH_VM_ATTRIBUTES_H_
typedef unsigned int	vm_machine_attribute_t;
#define	MATTR_CACHE		1
#define MATTR_MIGRATE		2
#define	MATTR_REPLICATE		4
typedef int		vm_machine_attribute_val_t;
#define MATTR_VAL_OFF		0
#define MATTR_VAL_ON		1
#define MATTR_VAL_GET		2
#define MATTR_VAL_CACHE_FLUSH	6
#define MATTR_VAL_DCACHE_FLUSH	7
#define MATTR_VAL_ICACHE_FLUSH	8
#endif