enum {	NONE=0xe800,
TAGS,
TAGE,
SPCS,
PAR,
LIGS,
LACU=LIGS,
LGRV,
LUML,
LCED,
LTIL,
LBRV,
LRNG,
LDOT,
LDTB,
LFRN,
LFRB,
LOGO,
LMAC,
LHCK,
LASP,
LLEN,
LBRB,
LIGE,
MULTI,
MAAS=MULTI,
MALN,
MAND,
MAOQ,
MBRA,
MDD,
MDDD,
MEAS,
MELN,
MEMM,
MHAS,
MHLN,
MIAS,
MILN,
MLCT,
MLFF,
MLFFI,
MLFFL,
MLFL,
MLFI,
MLLS,
MLST,
MOAS,
MOLN,
MOR,
MRAS,
MRLN,
MTT,
MUAS,
MULN,
MWAS,
MWLN,
MOE,
MES,
MULTIE,
};
#define Nligs (LIGE-LIGS)
#define Nmulti (MULTIE-MULTI)
typedef struct Entry Entry;
typedef struct Assoc Assoc;
typedef struct Nassoc Nassoc;
typedef struct Dict Dict;
struct Entry {
char	*start;
char	*end;
long	doff;
};
struct Assoc {
char	*key;
long	val;
};
struct Nassoc {
long	key;
long	val;
};
struct Dict {
char	*name;
char	*desc;
char	*path;
char	*indexpath;
long	(*nextoff)(long);
void	(*printentry)(Entry, int);
void	(*printkey)(void);
};
int	acomp(Rune*, Rune*);
Rune	*changett(Rune *, Rune *, int);
void	err(char*, ...);
void	fold(Rune *);
void	foldre(char*, char*);
Rune	liglookup(Rune, Rune);
long	lookassoc(Assoc*, int, char*);
long	looknassoc(Nassoc*, int, long);
void	outprint(char*, ...);
void	outrune(long);
void	outrunes(Rune *);
void	outchar(int);
void	outchars(char *);
void	outnl(int);
void	outpiece(char *, char *);
void	runescpy(Rune*, Rune*);
long	runetol(Rune*);
long	oednextoff(long);
void	oedprintentry(Entry, int);
void	oedprintkey(void);
long	ahdnextoff(long);
void	ahdprintentry(Entry, int);
void	ahdprintkey(void);
long	pcollnextoff(long);
void	pcollprintentry(Entry, int);
void	pcollprintkey(void);
long	pcollgnextoff(long);
void	pcollgprintentry(Entry, int);
void	pcollgprintkey(void);
long	movienextoff(long);
void	movieprintentry(Entry, int);
void	movieprintkey(void);
long	pgwnextoff(long);
void	pgwprintentry(Entry,int);
void	pgwprintkey(void);
void	rogetprintentry(Entry, int);
long	rogetnextoff(long);
void	rogetprintkey(void);
long	slangnextoff(long);
void	slangprintentry(Entry, int);
void	slangprintkey(void);
long	robertnextoff(long);
void	robertindexentry(Entry, int);
void	robertprintkey(void);
long	robertnextflex(long);
void	robertflexentry(Entry, int);
long	simplenextoff(long);
void	simpleprintentry(Entry, int);
void	simpleprintkey(void);
long	thesnextoff(long);
void	thesprintentry(Entry, int);
void	thesprintkey(void);
long	worldnextoff(long);
void	worldprintentry(Entry, int);
void	worldprintkey(void);
extern Biobuf	*bdict;
extern Biobuf	*bout;
extern int	linelen;
extern int	breaklen;
extern int	outinhibit;
extern int	debug;
extern Rune	*multitab[];
extern Dict	dicts[];
#define asize(a) (sizeof (a)/sizeof(a[0]))