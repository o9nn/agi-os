typedef struct Field Field;
typedef struct Filter Filter;
typedef struct Msg Msg;
typedef struct Mux Mux;
typedef struct Proto Proto;
#define NetS(x) ((((uchar*)x)[0]<<8) | ((uchar*)x)[1])
#define Net3(x) ((((uchar*)x)[0]<<16) | (((uchar*)x)[1]<<8) | ((uchar*)x)[2])
#define NetL(x) ((((uchar*)x)[0]<<24) | (((uchar*)x)[1]<<16) | (((uchar*)x)[2]<<8) | ((uchar*)x)[3])
struct Proto
{
char*	name;
void	(*compile)(Filter*);
int	(*filter)(Filter*, Msg*);
int	(*seprint)(Msg*);
Mux*	mux;
char*	valfmt;
Field*	field;
int	(*framer)(int, uchar*, int);
};
extern Proto *protos[];
struct Mux
{
char*	name;
ulong	val;
Proto*	pr;
};
struct Field
{
char*	name;
int	ftype;
int	subop;
char*	help;
};
struct Msg
{
uchar	*ps;
uchar	*pe;
char	*p;
char	*e;
int	needroot;
Proto	*pr;
};
enum
{
Fnum,
Fether,
Fv4ip,
Fv6ip,
Fba,
};
struct Filter {
int	op;
char	*s;
Filter	*l;
Filter	*r;
Proto	*pr;
int	subop;
ulong	param;
union {
ulong	ulv;
vlong	vlv;
uchar	a[32];
};
};
extern void	yyinit(char*);
extern int	yyparse(void);
extern Filter*	newfilter(void);
extern void	compile_cmp(char*, Filter*, Field*);
extern void	demux(Mux*, ulong, ulong, Msg*, Proto*);
extern int	defaultframer(int, uchar*, int);
extern int Mflag;
extern int Nflag;
extern int dflag;
extern int Cflag;
typedef Filter *Filterptr;
#define YYSTYPE Filterptr
extern Filter *filter;