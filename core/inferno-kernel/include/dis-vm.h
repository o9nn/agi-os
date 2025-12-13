#ifndef DIS_VM_H
#define DIS_VM_H
#include <stdint.h>
#include <stddef.h>
int dis_vm_init(void);
void dis_vm_shutdown(void);
int dis_vm_execute(const uint8_t *bytecode, size_t bytecode_size);
#endif
