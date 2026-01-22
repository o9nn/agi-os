typedef struct Exec Exec;
typedef struct Rule Rule;
typedef struct Ruleset Ruleset;
enum
{
OArg,
OAttr,
OData,
ODst,
OPlumb,
OSrc,
OType,
OWdir,
};
enum
{
VAdd,
VClient,
VDelete,
VIs,
VIsdir,
VIsfile,
VMatches,
VSet,
VStart,
VTo,
};
struct Rule
{
int	obj;
int	verb;
char	*arg;
char	*qarg;
Reprog	*regex;
};
struct Ruleset
{
int	npat;
int	nact;
Rule	**pat;
Rule	**act;
char	*port;
};
struct Exec
{
Plumbmsg	*msg;
char			*match[10];
int			p0;
int			p1;
int			clearclick;
int			setdata;
int			holdforclient;
char			*file;
char 			*dir;
};
void		parseerror(char*, ...);
void		error(char*, ...);
void*	emalloc(long);
void*	erealloc(void*, long);
char*	estrdup(char*);
Ruleset**	readrules(char*, int);
void		startfsys(void);
Exec*	matchruleset(Plumbmsg*, Ruleset*);
void		freeexec(Exec*);
char*	startup(Ruleset*, Exec*);
char*	printrules(void);
void		addport(char*);
char*	writerules(char*, int);
char*	expand(Exec*, char*, char**);
void		makeports(Ruleset*[]);
void		printinputstack(void);
int		popinput(void);
Ruleset	**rules;
char		*user;
char		*home;
jmp_buf	parsejmp;
char		*lasterror;
char		**ports;
int		nports;