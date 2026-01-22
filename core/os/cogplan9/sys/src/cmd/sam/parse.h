typedef struct Addr Addr;
typedef struct Cmd Cmd;
struct Addr
{
char	type;
union{
String	*re;
Addr	*aleft;
} g;
Posn	num;
Addr	*next;
};
#define	are	g.re
#define	left	g.aleft
struct Cmd
{
Addr	*addr;
String	*re;
union{
Cmd	*cmd;
String	*text;
Addr	*addr;
} g;
Cmd	*next;
short	num;
ushort	flag;
ushort	cmdc;
};
#define	ccmd	g.cmd
#define	ctext	g.text
#define	caddr	g.addr
typedef struct Cmdtab Cmdtab;
struct Cmdtab
{
ushort	cmdc;
uchar	text;
uchar	regexp;
uchar	addr;
uchar	defcmd;
uchar	defaddr;
uchar	count;
char	*token;
int	(*fn)(File*, Cmd*);
}cmdtab[];
enum Defaddr{
aNo,
aDot,
aAll,
};
int	nl_cmd(File*, Cmd*), a_cmd(File*, Cmd*), b_cmd(File*, Cmd*);
int	c_cmd(File*, Cmd*), cd_cmd(File*, Cmd*), d_cmd(File*, Cmd*);
int	D_cmd(File*, Cmd*), e_cmd(File*, Cmd*);
int	f_cmd(File*, Cmd*), g_cmd(File*, Cmd*), i_cmd(File*, Cmd*);
int	k_cmd(File*, Cmd*), m_cmd(File*, Cmd*), n_cmd(File*, Cmd*);
int	p_cmd(File*, Cmd*), q_cmd(File*, Cmd*);
int	s_cmd(File*, Cmd*), u_cmd(File*, Cmd*), w_cmd(File*, Cmd*);
int	x_cmd(File*, Cmd*), X_cmd(File*, Cmd*), plan9_cmd(File*, Cmd*);
int	eq_cmd(File*, Cmd*);
String	*getregexp(int);
Addr	*newaddr(void);
Address	address(Addr*, Address, int);
int	cmdexec(File*, Cmd*);