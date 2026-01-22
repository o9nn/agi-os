#ifndef _MACH_VM_WIRE_H_
#define _MACH_VM_WIRE_H_
typedef int vm_wire_t;
#define VM_WIRE_NONE 0
#define VM_WIRE_CURRENT 1
#define VM_WIRE_FUTURE 2
#define VM_WIRE_ALL (VM_WIRE_CURRENT | VM_WIRE_FUTURE)
#endif