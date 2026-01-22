#ifndef _MACH_I386_EXEC_ELF_H_
#define _MACH_I386_EXEC_ELF_H_
typedef unsigned int	Elf32_Addr;
typedef unsigned short	Elf32_Half;
typedef unsigned int	Elf32_Off;
typedef signed int	Elf32_Sword;
typedef unsigned int	Elf32_Word;
typedef uint64_t	Elf64_Addr;
typedef uint64_t	Elf64_Off;
typedef int32_t		Elf64_Shalf;
typedef int32_t		Elf64_Sword;
typedef uint32_t	Elf64_Word;
typedef int64_t		Elf64_Sxword;
typedef uint64_t	Elf64_Xword;
typedef uint16_t	Elf64_Half;
#if defined(__x86_64__) && ! defined(USER32)
#define MY_ELF_CLASS	ELFCLASS64
#define MY_EI_DATA	ELFDATA2LSB
#define MY_E_MACHINE	EM_X86_64
#else
#define MY_ELF_CLASS	ELFCLASS32
#define MY_EI_DATA	ELFDATA2LSB
#define MY_E_MACHINE	EM_386
#endif
#endif