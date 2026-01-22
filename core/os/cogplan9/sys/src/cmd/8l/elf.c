#include "l.h"
enum {
Stitext		= 1,
Stidata		= 7,
Stistrtab	= 13,
};
void
elfident(int bo, int class)
{
strnput("\177ELF", 4);
cput(class);
cput(bo);
cput(1);
if(debug['k']){
cput(255);
cput(0);
}
else{
cput(0);
cput(0);
}
strnput("", 7);
}
void
elfstrtab(void)
{
cput(0);
strnput(".text", 5);
cput(0);
strnput(".data", 5);
cput(0);
strnput(".strtab", 7);
cput(0);
cput(0);
}
void
elf32phdr(void (*putl)(long), ulong type, ulong off, ulong vaddr, ulong paddr,
ulong filesz, ulong memsz, ulong prots, ulong align)
{
putl(type);
putl(off);
putl(vaddr);
putl(paddr);
putl(filesz);
putl(memsz);
putl(prots);
putl(align);
}
void
elf32shdr(void (*putl)(long), ulong name, ulong type, ulong flags, ulong vaddr,
ulong off, ulong sectsz, ulong link, ulong addnl, ulong align,
ulong entsz)
{
putl(name);
putl(type);
putl(flags);
putl(vaddr);
putl(off);
putl(sectsz);
putl(link);
putl(addnl);
putl(align);
putl(entsz);
}
static void
elf32sectab(void (*putl)(long))
{
seek(cout, HEADR+textsize+datsize+symsize, 0);
elf32shdr(putl, Stitext, Progbits, Salloc|Sexec, INITTEXT,
HEADR, textsize, 0, 0, 0x10000, 0);
elf32shdr(putl, Stidata, Progbits, Salloc|Swrite, INITDAT,
HEADR+textsize, datsize, 0, 0, 0x10000, 0);
elf32shdr(putl, Stistrtab, Strtab, 1 << 5, 0,
HEADR+textsize+datsize+symsize+3*Shdr32sz, 14, 0, 0, 1, 0);
elfstrtab();
}
void
elf32(int mach, int bo, int addpsects, void (*putpsects)(Putl))
{
ulong phydata;
void (*putw)(long), (*putl)(long);
if(bo == ELFDATA2MSB){
putw = wput;
putl = lput;
}else if(bo == ELFDATA2LSB){
putw = wputl;
putl = lputl;
}else{
print("elf32 byte order is mixed-endian\n");
errorexit();
return;
}
elfident(bo, ELFCLASS32);
putw(EXEC);
putw(mach);
putl(1L);
putl(entryvalue());
putl(Ehdr32sz);
if(debug['S'])
putl(HEADR+textsize+datsize+symsize);
else
putl(0);
putl(0L);
putw(Ehdr32sz);
putw(Phdr32sz);
putw(3 + addpsects);
putw(Shdr32sz);
if(debug['S']){
putw(3);
putw(2);
}else{
putw(0);
putw(0);
}
elf32phdr(putl, PT_LOAD, HEADR, INITTEXT, INITTEXTP,
textsize, textsize, R|X, INITRND);
phydata = INITDAT - (INITTEXT - INITTEXTP);
elf32phdr(putl, PT_LOAD, HEADR+textsize, INITDAT, phydata,
datsize, datsize+bsssize, R|W|X, INITRND);
elf32phdr(putl, NOPTYPE, HEADR+textsize+datsize, 0, 0,
symsize, lcsize, R, 4);
if (addpsects > 0)
putpsects(putl);
cflush();
if(debug['S'])
elf32sectab(putl);
}
void
elf64phdr(void (*putl)(long), void (*putll)(vlong), ulong type, uvlong off,
uvlong vaddr, uvlong paddr, uvlong filesz, uvlong memsz, ulong prots,
uvlong align)
{
putl(type);
putl(prots);
putll(off);
putll(vaddr);
putll(paddr);
putll(filesz);
putll(memsz);
putll(align);
}
void
elf64shdr(void (*putl)(long), void (*putll)(vlong), ulong name, ulong type,
uvlong flags, uvlong vaddr, uvlong off, uvlong sectsz, ulong link,
ulong addnl, uvlong align, uvlong entsz)
{
putl(name);
putl(type);
putll(flags);
putll(vaddr);
putll(off);
putll(sectsz);
putl(link);
putl(addnl);
putll(align);
putll(entsz);
}
static void
elf64sectab(void (*putl)(long), void (*putll)(vlong))
{
seek(cout, HEADR+textsize+datsize+symsize, 0);
elf64shdr(putl, putll, Stitext, Progbits, Salloc|Sexec, INITTEXT,
HEADR, textsize, 0, 0, 0x10000, 0);
elf64shdr(putl, putll, Stidata, Progbits, Salloc|Swrite, INITDAT,
HEADR+textsize, datsize, 0, 0, 0x10000, 0);
elf64shdr(putl, putll, Stistrtab, Strtab, 1 << 5, 0,
HEADR+textsize+datsize+symsize+3*Shdr64sz, 14, 0, 0, 1, 0);
elfstrtab();
}
void
elf64(int mach, int bo, int addpsects, void (*putpsects)(Putl))
{
uvlong phydata;
void (*putw)(long), (*putl)(long);
void (*putll)(vlong);
if(bo == ELFDATA2MSB){
putw = wput;
putl = lput;
putll = llput;
}else if(bo == ELFDATA2LSB){
putw = wputl;
putl = lputl;
putll = llputl;
}else{
print("elf64 byte order is mixed-endian\n");
errorexit();
return;
}
elfident(bo, ELFCLASS64);
putw(EXEC);
putw(mach);
putl(1L);
putll(entryvalue());
putll(Ehdr64sz);
if(debug['S'])
putll(HEADR+textsize+datsize+symsize);
else
putll(0);
putl(0L);
putw(Ehdr64sz);
putw(Phdr64sz);
putw(3 + addpsects);
putw(Shdr64sz);
if(debug['S']){
putw(3);
putw(2);
}else{
putw(0);
putw(0);
}
elf64phdr(putl, putll, PT_LOAD, HEADR, INITTEXT, INITTEXTP,
textsize, textsize, R|X, INITRND);
phydata = INITDAT - (INITTEXT - INITTEXTP);
elf64phdr(putl, putll, PT_LOAD, HEADR+textsize, INITDAT, phydata,
datsize, datsize+bsssize, R|W, INITRND);
elf64phdr(putl, putll, NOPTYPE, HEADR+textsize+datsize, 0, 0,
symsize, lcsize, R, 4);
if (addpsects > 0)
putpsects(putl);
cflush();
if(debug['S'])
elf64sectab(putl, putll);
}