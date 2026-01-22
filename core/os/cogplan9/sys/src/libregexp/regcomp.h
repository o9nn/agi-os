#define NSUBEXP 32
typedef struct Resublist	Resublist;
struct	Resublist
{
Resub	m[NSUBEXP];
};
#define RUNE		0177
#define	OPERATOR	0200
#define	START		0200
#define	RBRA		0201
#define	LBRA		0202
#define	OR		0203
#define	CAT		0204
#define	STAR		0205
#define	PLUS		0206
#define	QUEST		0207
#define	ANY		0300
#define	ANYNL		0301
#define	NOP		0302
#define	BOL		0303
#define	EOL		0304
#define	CCLASS		0305
#define	NCCLASS		0306
#define	END		0377
#define LISTSIZE	10
#define BIGLISTSIZE	(25*LISTSIZE)
typedef struct Relist	Relist;
struct Relist
{
Reinst*		inst;
Resublist	se;
};
typedef struct Reljunk	Reljunk;
struct	Reljunk
{
Relist*	relist[2];
Relist*	reliste[2];
int	starttype;
Rune	startchar;
char*	starts;
char*	eol;
Rune*	rstarts;
Rune*	reol;
};
extern Relist*	_renewthread(Relist*, Reinst*, int, Resublist*);
extern void	_renewmatch(Resub*, int, Resublist*);
extern Relist*	_renewemptythread(Relist*, Reinst*, int, char*);
extern Relist*	_rrenewemptythread(Relist*, Reinst*, int, Rune*);