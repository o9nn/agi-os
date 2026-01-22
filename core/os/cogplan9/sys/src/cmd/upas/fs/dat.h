typedef struct Message Message;
struct Message
{
int	id;
int	refs;
int	subname;
char	name[Elemlen];
char	*start;
char	*end;
char	*header;
char	*hend;
int	hlen;
char	*mheader;
char	*mhend;
char	*body;
char	*bend;
char	*rbody;
char	*rbend;
char	*lim;
char	deleted;
char	inmbox;
char	mallocd;
char	ballocd;
char	hallocd;
String	*unixheader;
String	*unixfrom;
String	*unixdate;
String	*from822;
String	*sender822;
String	*to822;
String	*bcc822;
String	*cc822;
String	*replyto822;
String	*date822;
String	*inreplyto822;
String	*subject822;
String	*messageid822;
String	*addrs;
String	*mimeversion;
String	*sdigest;
String	*boundary;
String	*type;
int	encoding;
int	disposition;
String	*charset;
String	*filename;
int	converted;
int	decoded;
char	lines[10];
Message	*next;
Message	*part;
Message	*whole;
uchar	digest[SHA1dlen];
vlong	imapuid;
char		uidl[80];
int		mesgno;
};
enum
{
Enone=	0,
Ebase64,
Equoted,
Dnone=	0,
Dinline,
Dfile,
Dignore,
PAD64=	'=',
};
typedef struct Mailbox Mailbox;
struct Mailbox
{
QLock;
int	refs;
Mailbox	*next;
int	id;
int	dolock;
int	std;
char	name[Elemlen];
char	path[Pathlen];
Dir	*d;
Message	*root;
int	vers;
ulong waketime;
char	*(*sync)(Mailbox*, int);
void	(*close)(Mailbox*);
char	*(*fetch)(Mailbox*, Message*);
char	*(*ctl)(Mailbox*, int, char**);
void	*aux;
};
typedef char *Mailboxinit(Mailbox*, char*);
extern Message	*root;
extern Mailboxinit	plan9mbox;
extern Mailboxinit	pop3mbox;
extern Mailboxinit	imap4mbox;
extern Mailboxinit	planbmbox;
extern Mailboxinit	planbvmbox;
char*		syncmbox(Mailbox*, int);
char*		geterrstr(void);
void*		emalloc(ulong);
void*		erealloc(void*, ulong);
Message*	newmessage(Message*);
void		delmessage(Mailbox*, Message*);
void		delmessages(int, char**);
int		newid(void);
void		mailplumb(Mailbox*, Message*, int);
char*		newmbox(char*, char*, int);
void		freembox(char*);
void		logmsg(char*, Message*);
void		msgincref(Message*);
void		msgdecref(Mailbox*, Message*);
void		mboxincref(Mailbox*);
void		mboxdecref(Mailbox*);
void		convert(Message*);
void		decode(Message*);
int		cistrncmp(char*, char*, int);
int		cistrcmp(char*, char*);
int		decquoted(char*, char*, char*, int);
int		xtoutf(char*, char**, char*, char*);
void		countlines(Message*);
int		headerlen(Message*);
void		parse(Message*, int, Mailbox*, int);
void		parseheaders(Message*, int, Mailbox*, int);
void		parsebody(Message*, Mailbox*);
void		parseunix(Message*);
String*	date822tounix(char*);
int		fidmboxrefs(Mailbox*);
int		hashmboxrefs(Mailbox*);
void		checkmboxrefs(void);
extern int	debug;
extern int	fflag;
extern int	logging;
extern char	user[Elemlen];
extern char	stdmbox[Pathlen];
extern QLock	mbllock;
extern Mailbox	*mbl;
extern char	*mntpt;
extern int	biffing;
extern int	plumbing;
extern char*	Enotme;
enum
{
Qbody,
Qbcc,
Qcc,
Qdate,
Qdigest,
Qdisposition,
Qfilename,
Qfrom,
Qheader,
Qinreplyto,
Qlines,
Qmimeheader,
Qmessageid,
Qraw,
Qrawbody,
Qrawheader,
Qrawunix,
Qreplyto,
Qsender,
Qsubject,
Qto,
Qtype,
Qunixheader,
Qinfo,
Qunixdate,
Qmax,
Qtop,
Qmbox,
Qdir,
Qctl,
Qmboxctl,
};
#define PATH(id, f)	((((id)&0xfffff)<<10) | (f))
#define FILE(p)		((p) & 0x3ff)
char *dirtab[];
typedef struct Hash Hash;
struct Hash {
Hash	*next;
char	*name;
ulong	ppath;
Qid	qid;
Mailbox	*mb;
Message	*m;
};
Hash	*hlook(ulong, char*);
void	henter(ulong, char*, Qid, Message*, Mailbox*);
void	hfree(ulong, char*);
ulong msgallocd, msgfreed;