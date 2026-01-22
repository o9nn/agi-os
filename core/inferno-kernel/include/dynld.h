typedef struct Dynobj Dynobj;
typedef struct Dynsym Dynsym;
struct Dynobj
{
ulong	size;
ulong	text;
ulong	data;
ulong	bss;
uchar*	base;
int	nexport;
Dynsym*	export;
int	nimport;
Dynsym**	import;
};
struct Dynsym
{
ulong	sig;
ulong	addr;
char	*name;
};
extern Dynsym*	dynfindsym(char*, Dynsym*, int);
extern void	dynfreeimport(Dynobj*);
extern void*	dynimport(Dynobj*, char*, ulong);
extern int	dynloadable(void*, long (*r)(void*,void*,long), vlong(*sk)(void*,vlong,int));
extern Dynobj*	dynloadfd(int, Dynsym*, int, ulong);
extern Dynobj*	dynloadgen(void*, long (*r)(void*,void*,long), vlong (*s)(void*,vlong,int), void (*e)(char*), Dynsym*, int, ulong);
extern long	dynmagic(void);
extern void	dynobjfree(Dynobj*);
extern char*	dynreloc(uchar*, ulong, int, Dynsym**, int);
extern int	dyntabsize(Dynsym*);
extern Dynsym	_exporttab[];