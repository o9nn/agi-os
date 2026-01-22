#include "../dhcp.h"
enum
{
Maxstr=	256,
};
typedef struct Binding Binding;
struct Binding
{
Binding *next;
uchar	ip[IPaddrlen];
char	*boundto;
char	*offeredto;
long	lease;
long	expoffer;
long	offer;
long	lasttouched;
long	lastcomplained;
long	tried;
Qid	q;
};
typedef struct Info	Info;
struct Info
{
int	indb;
char	domain[Maxstr];
char	bootf[Maxstr];
char	bootf2[Maxstr];
uchar	tftp[NDB_IPlen];
uchar	tftp2[NDB_IPlen];
uchar	ipaddr[NDB_IPlen];
uchar	ipmask[NDB_IPlen];
uchar	ipnet[NDB_IPlen];
uchar	etheraddr[6];
uchar	gwip[NDB_IPlen];
uchar	fsip[NDB_IPlen];
uchar	auip[NDB_IPlen];
char	rootpath[Maxstr];
char	dhcpgroup[Maxstr];
char	vendor[Maxstr];
};
extern int	validip(uchar*);
extern void	warning(int, char*, ...);
extern int	minlease;
extern char*	tohex(char*, uchar*, int);
extern char*	toid(uchar*, int);
extern void	initbinding(uchar*, int);
extern Binding*	iptobinding(uchar*, int);
extern Binding*	idtobinding(char*, Info*, int);
extern Binding*	idtooffer(char*, Info*);
extern int	commitbinding(Binding*);
extern int	releasebinding(Binding*, char*);
extern int	samenet(uchar *ip, Info *iip);
extern void	mkoffer(Binding*, char*, long);
extern int	syncbinding(Binding*, int);
extern int	lookup(Bootp*, Info*, Info*);
extern int	lookupip(uchar*, Info*, int);
extern void	lookupname(char*, Ndbtuple*);
extern Iplifc*	findlifc(uchar*);
extern int	forme(uchar*);
extern int	lookupserver(char*, uchar**, Ndbtuple *t);
extern Ndbtuple* lookupinfo(uchar *ipaddr, char **attr, int n);
extern int	icmpecho(uchar*);
extern char	*binddir;
extern int	debug;
extern char	*blog;
extern Ipifc	*ipifcs;
extern long	now;
extern char	*ndbfile;