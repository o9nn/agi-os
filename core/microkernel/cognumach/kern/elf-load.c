#include <alloca.h>
#include <mach/machine/vm_types.h>
#include <mach/exec/elf.h>
#include <mach/exec/exec.h>
int exec_load(exec_read_func_t *read, exec_read_exec_func_t *read_exec,
void *handle, exec_info_t *out_info)
{
vm_size_t actual;
Elf_Ehdr x;
Elf_Phdr *phdr;
vm_size_t phsize;
int i;
int result;
vm_offset_t loadbase = 0;
if ((result = (*read)(handle, 0, &x, sizeof(x), &actual)) != 0)
return result;
if (actual < sizeof(x))
return EX_NOT_EXECUTABLE;
if ((x.e_ident[EI_MAG0] != ELFMAG0) ||
(x.e_ident[EI_MAG1] != ELFMAG1) ||
(x.e_ident[EI_MAG2] != ELFMAG2) ||
(x.e_ident[EI_MAG3] != ELFMAG3))
return EX_NOT_EXECUTABLE;
if ((x.e_ident[EI_CLASS] != MY_ELF_CLASS) ||
(x.e_ident[EI_DATA] != MY_EI_DATA) ||
(x.e_machine != MY_E_MACHINE))
return EX_WRONG_ARCH;
if (x.e_type == ET_DYN || x.e_type == ET_REL) {
loadbase = 128 << 20;
}
out_info->entry = (vm_offset_t) x.e_entry + loadbase;
phsize = x.e_phnum * x.e_phentsize;
phdr = (Elf_Phdr *)alloca(phsize);
result = (*read)(handle, x.e_phoff, phdr, phsize, &actual);
if (result)
return result;
if (actual < phsize)
return EX_CORRUPT;
out_info->stack_prot = VM_PROT_ALL;
for (i = 0; i < x.e_phnum; i++)
{
Elf_Phdr *ph = (Elf_Phdr *)((vm_offset_t)phdr + (unsigned int)i * x.e_phentsize);
if (ph->p_type == PT_LOAD)
{
exec_sectype_t type = EXEC_SECTYPE_ALLOC |
EXEC_SECTYPE_LOAD;
if (ph->p_flags & PF_R) type |= EXEC_SECTYPE_READ;
if (ph->p_flags & PF_W) type |= EXEC_SECTYPE_WRITE;
if (ph->p_flags & PF_X) type |= EXEC_SECTYPE_EXECUTE;
result = (*read_exec)(handle,
ph->p_offset, ph->p_filesz,
ph->p_vaddr + loadbase, ph->p_memsz, type);
if (result)
return result;
} else if (ph->p_type == PT_GNU_STACK) {
out_info->stack_prot = 0;
if (ph->p_flags & PF_R) out_info->stack_prot |= VM_PROT_READ;
if (ph->p_flags & PF_W) out_info->stack_prot |= VM_PROT_WRITE;
if (ph->p_flags & PF_X) out_info->stack_prot |= VM_PROT_EXECUTE;
}
}
return 0;
}