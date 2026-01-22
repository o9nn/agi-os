#pragma	varargck	argpos	editerror	1
typedef struct Addr	Addr;
typedef struct Address	Address;
typedef struct Cmd	Cmd;
typedef struct List	List;
typedef struct String	String;
struct String
{
int	n;
Rune	*r;
int	nalloc;
};
struct Addr
{
char	type;
union{
String	*re;
Addr	*left;
};
ulong	num;
Addr	*next;
};
struct Address
{
Range	r;
File	*f;
};
struct Cmd
{
Addr	*addr;
String	*re;
union{
Cmd	*cmd;
String	*text;
Addr	*mtaddr;
};
Cmd	*next;
short	num;
ushort	flag;
ushort	cmdc;
};
extern struct cmdtab{
ushort	cmdc;
uchar	text;
uchar	regexp;
uchar	addr;
uchar	defcmd;
uchar	defaddr;
uchar	count;
char	*token;
int	(*fn)(Text*, Cmd*);
}cmdtab[];
#define	INCR	25
struct List
{
int	nalloc;
int	nused;
union{
void	*listptr;
void*	*ptr;
uchar*	*ucharptr;
String*	*stringptr;
};
};
enum Defaddr{
aNo,
aDot,
aAll,
};
int	nl_cmd(Text*, Cmd*), a_cmd(Text*, Cmd*), b_cmd(Text*, Cmd*);
int	c_cmd(Text*, Cmd*), d_cmd(Text*, Cmd*);
int	B_cmd(Text*, Cmd*), D_cmd(Text*, Cmd*), e_cmd(Text*, Cmd*);
int	f_cmd(Text*, Cmd*), g_cmd(Text*, Cmd*), i_cmd(Text*, Cmd*);
int	k_cmd(Text*, Cmd*), m_cmd(Text*, Cmd*), n_cmd(Text*, Cmd*);
int	p_cmd(Text*, Cmd*);
int	s_cmd(Text*, Cmd*), u_cmd(Text*, Cmd*), w_cmd(Text*, Cmd*);
int	x_cmd(Text*, Cmd*), X_cmd(Text*, Cmd*), pipe_cmd(Text*, Cmd*);
int	eq_cmd(Text*, Cmd*);
String	*allocstring(int);
void	freestring(String*);
String	*getregexp(int);
Addr	*newaddr(void);
Address	cmdaddress(Addr*, Address, int);
int	cmdexec(Text*, Cmd*);
void	editerror(char*, ...);
int	cmdlookup(int);
void	resetxec(void);
void	Straddc(String*, int);