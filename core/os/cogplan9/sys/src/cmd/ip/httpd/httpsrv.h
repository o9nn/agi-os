typedef struct HSPriv	HSPriv;
enum
{
HSTIMEOUT	= 15 * 60 * 1000,
Modsilent	= '@',
Modperm		= '=',
Modsubord	= '*',
Modonly		= '>',
Redirsilent	= 1<<0,
Redirperm	= 1<<1,
Redirsubord	= 1<<2,
Redironly	= 1<<3,
};
struct HSPriv
{
char		*remotesys;
char		*remoteserv;
};
extern	int		logall[3];
extern	char*		HTTPLOG;
extern	char*		webroot;
extern	char*		netdir;
#define 		STRLEN(s)	(sizeof(s)-1)
char			*estrdup(char*);
void*			ezalloc(ulong);
int			authcheck(HConnect *c);
int			checkreq(HConnect *c, HContent *type, HContent *enc, long mtime, char *etag);
int			etagmatch(int, HETag*, char*);
HRange			*fixrange(HRange *h, long length);
int			sendfd(HConnect *c, int fd, Dir *dir, HContent *type, HContent *enc);
void			contentinit(void);
HContents		dataclass(HConnect *, char*, int);
int			updateQid(int, Qid*);
HContents		uriclass(HConnect *, char*);
void			anonymous(HConnect*);
void			hintprint(HConnect *hc, Hio*, char *, int, int);
void			statsinit(void);
void			urlcanon(char *url);
void			urlinit(void);
HConnect*		init(int, char**);
vlong			Bfilelen(void*);
void			redirectinit(void);
char*			redirect(HConnect *hc, char*, uint *);
char*			masquerade(char*);
char*			authrealm(HConnect *hc, char *path);
char			*undecorated(char *repl);
void			logit(HConnect*, char*, ...);
#pragma	varargck	argpos	logit	2
void			writelog(HConnect*, char*, ...);
#pragma	varargck	argpos	writelog	2
int authorize(HConnect*, char*);
char *webroot;