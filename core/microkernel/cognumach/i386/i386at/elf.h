#ifndef _X86_ELF_H
#define _X86_ELF_H
#define ELF_SHT_SYMTAB  2
#define ELF_SHT_STRTAB  3
struct elf_shdr {
unsigned int name;
unsigned int type;
unsigned int flags;
unsigned long addr;
unsigned long offset;
unsigned int size;
unsigned int link;
unsigned int info;
unsigned int addralign;
unsigned int entsize;
};
#ifdef __LP64__
struct elf_sym {
unsigned int name;
unsigned char info;
unsigned char other;
unsigned short shndx;
unsigned long value;
unsigned long size;
};
#else
struct elf_sym {
unsigned int name;
unsigned long value;
unsigned long size;
unsigned char info;
unsigned char other;
unsigned short shndx;
};
#endif
#endif