#ifndef dos__INCLUDED
# define dos__INCLUDED
#include <dos.h>
#if defined(__WATCOMC__) || defined(_MSC_VER)
# include <conio.h>
# define inport(px) inpw(px)
# define inportb(px) inp(px)
# define outport(px,w) outpw(px,w)
# define outportb(px,b) outp(px,b)
# define enable() _enable()
# define disable() _disable()
# define PTR_OFF(ptr) ((ushort)(uint)(ptr))
#define ff_name name
#define dos_findfirst(n,b) _dos_findfirst(n, _A_NORMAL | _A_RDONLY, b)
#define dos_findnext(b) _dos_findnext(b)
# ifdef __WATCOMC__
# define MK_PTR(seg,off) (((seg) << 4) + (off))
# define int86 int386
# define int86x int386x
# define rshort w
# define ff_struct_t struct find_t
# else
# define MK_PTR(seg,off) (((ulong)(seg) << 16) + (off))
# define cputs _cputs
# define fdopen _fdopen
# define O_BINARY _O_BINARY
# define REGS _REGS
# define rshort x
# define ff_struct_t struct _find_t
# define stdprn _stdprn
# endif
#else
#include <dir.h>
# define MK_PTR(seg,off) MK_FP(seg,off)
# define PTR_OFF(ptr) FP_OFF(ptr)
# define rshort x
#define ff_struct_t struct ffblk
#define dos_findfirst(n,b) findfirst(n, b, 0)
#define dos_findnext(b) findnext(b)
#endif
#endif