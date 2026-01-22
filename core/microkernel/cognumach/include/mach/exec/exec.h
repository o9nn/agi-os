#ifndef _MACH_EXEC_H_
#define _MACH_EXEC_H_
#include <mach/machine/vm_types.h>
#include <mach/vm_prot.h>
typedef enum
{
EXEC_ELF = 1,
EXEC_AOUT = 2,
} exec_format_t;
typedef struct exec_info
{
exec_format_t format;
vm_offset_t entry;
vm_offset_t init_dp;
vm_offset_t interp;
vm_prot_t stack_prot;
} exec_info_t;
typedef int exec_sectype_t;
#define EXEC_SECTYPE_READ VM_PROT_READ
#define EXEC_SECTYPE_WRITE VM_PROT_WRITE
#define EXEC_SECTYPE_EXECUTE VM_PROT_EXECUTE
#define EXEC_SECTYPE_PROT_MASK VM_PROT_ALL
#define EXEC_SECTYPE_ALLOC ((exec_sectype_t)0x000100)
#define EXEC_SECTYPE_LOAD ((exec_sectype_t)0x000200)
#define EXEC_SECTYPE_DEBUG ((exec_sectype_t)0x010000)
#define EXEC_SECTYPE_AOUT_SYMTAB ((exec_sectype_t)0x020000)
#define EXEC_SECTYPE_AOUT_STRTAB ((exec_sectype_t)0x040000)
typedef int exec_read_func_t(void *handle, vm_offset_t file_ofs,
void *buf, vm_size_t size,
vm_size_t *out_actual);
typedef int exec_read_exec_func_t(void *handle,
vm_offset_t file_ofs, vm_size_t file_size,
vm_offset_t mem_addr, vm_size_t mem_size,
exec_sectype_t section_type);
int exec_load(exec_read_func_t *read, exec_read_exec_func_t *read_exec,
void *handle, exec_info_t *out_info);
#define EX_NOT_EXECUTABLE 6000
#define EX_WRONG_ARCH 6001
#define EX_CORRUPT 6002
#define EX_BAD_LAYOUT 6003
#endif