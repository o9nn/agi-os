typedef struct {
char	*t;
int	fd;
int	cfd;
char	*id;
char	response[128];
char	error[128];
int	fax;
char	phase;
char	ftsi[128];
long	fdcs[8];
long	fpts[8];
long	fet;
long	fhng;
int	pageno;
char	pageid[128];
int	pagefd;
int	valid;
long	time;
int	pid;
char	ibuf[1024];
char	*iptr;
long	icount;
Biobuf	*bp;
long	wd;
long	vr;
long	ln;
long	df;
} Modem;
enum {
Rok		= 0,
Rconnect,
Rring,
Rfailure,
Rrerror,
Rcontinue,
Rhangup,
Rnoise,
};
enum {
Eok	= 0,
Eattn,
Enoresponse,
Enoanswer,
Enofax,
Eincompatible,
Esys,
Eproto,
};
enum {
Vfdcs		= 0x0001,
Vftsi		= 0x0002,
Vfpts		= 0x0004,
Vfet		= 0x0008,
Vfhng		= 0x0010,
Vwd		= 0x4000,
Vtype		= 0x8000,
};
extern int initfaxmodem(Modem*);
extern int fcon(Modem*);
extern int ftsi(Modem*);
extern int fdcs(Modem*);
extern int fcfr(Modem*);
extern int fpts(Modem*);
extern int fet(Modem*);
extern int fhng(Modem*);
extern int faxreceive(Modem*, char*);
extern int faxsend(Modem*, int, char*[]);
extern int setflow(Modem*, int);
extern int setspeed(Modem*, int);
extern int rawmchar(Modem*, char*);
extern int getmchar(Modem*, char*, long);
extern int putmchar(Modem*, char*);
extern int command(Modem*, char*);
extern int response(Modem*, int);
extern void initmodem(Modem*, int, int, char*, char*);
extern void xonoff(Modem*, int);
extern void setpageid(char*, char*, long, int, int);
extern int createfaxfile(Modem*, char*);
extern int openfaxfile(Modem*, char*);
extern void verbose(char*, ...);
extern void error(char*, ...);
extern int seterror(Modem*, int);
extern void faxrlog(Modem*, int);
extern void faxxlog(Modem*, int);
extern int vflag;