typedef struct {
uchar	ident[16];
ushort	type;
ushort	machine;
int	version;
ulong	elfentry;
ulong	phoff;
ulong	shoff;
int	flags;
ushort	ehsize;
ushort	phentsize;
ushort	phnum;
ushort	shentsize;
ushort	shnum;
ushort	shstrndx;
} Ehdr;
typedef struct {
int	type;
ulong	offset;
ulong	vaddr;
ulong	paddr;
int	filesz;
ulong	memsz;
int	flags;
int	align;
} Phdr;
typedef struct {
ulong	name;
ulong	type;
ulong	flags;
ulong	addr;
ulong	offset;
ulong	size;
ulong	link;
ulong	info;
ulong	addralign;
ulong	entsize;
} Shdr;
enum {
MAG0 = 0,
MAG1 = 1,
MAG2 = 2,
MAG3 = 3,
CLASS = 4,
DATA = 5,
VERSION = 6,
ELFCLASSNONE = 0,
ELFCLASS32 = 1,
ELFCLASS64 = 2,
ELFCLASSNUM = 3,
ELFDATANONE = 0,
ELFDATA2LSB = 1,
ELFDATA2MSB = 2,
ELFDATANUM = 3,
NOETYPE = 0,
REL = 1,
EXEC = 2,
DYN = 3,
CORE = 4,
NONE = 0,
M32 = 1,
SPARC = 2,
I386 = 3,
M68K = 4,
M88K = 5,
I486 = 6,
I860 = 7,
MIPS = 8,
S370 = 9,
SPARC64 = 18,
POWER = 20,
POWER64 = 21,
ARM = 40,
AMD64 = 62,
ARM64 = 183,
RISCV = 243,
NO_VERSION = 0,
CURRENT = 1,
NOPTYPE = 0,
LOAD = 1,
DYNAMIC = 2,
INTERP = 3,
NOTE = 4,
SHLIB = 5,
PHDR = 6,
R = 0x4,
W = 0x2,
X = 0x1,
Progbits = 1,
Strtab = 3,
Nobits = 8,
Swrite = 1,
Salloc = 2,
Sexec = 4,
};
#define	ELF_MAG		((0x7f<<24) | ('E'<<16) | ('L'<<8) | 'F')