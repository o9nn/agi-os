#ifdef PLAN9
#pragma src "/sys/src/libauthsrv"
#pragma lib "libauthsrv.a"
#endif
typedef struct Ticket Ticket;
typedef struct Ticketreq Ticketreq;
typedef struct Authenticator Authenticator;
typedef struct Nvrsafe Nvrsafe;
typedef struct Passwordreq Passwordreq;
typedef struct OChapreply OChapreply;
typedef struct OMSchapreply OMSchapreply;
enum
{
ANAMELEN= 28,
AERRLEN= 64,
DOMLEN= 48,
DESKEYLEN= 7,
CHALLEN= 8,
NETCHLEN= 16,
CONFIGLEN= 14,
SECRETLEN= 32,
KEYDBOFF= 8,
OKEYDBLEN= ANAMELEN+DESKEYLEN+4+2,
KEYDBLEN= OKEYDBLEN+SECRETLEN,
OMD5LEN= 16,
};
enum
{
AuthTreq=1,
AuthChal=2,
AuthPass=3,
AuthOK=4,
AuthErr=5,
AuthMod=6,
AuthApop=7,
AuthOKvar=9,
AuthChap=10,
AuthMSchap=11,
AuthCram=12,
AuthHttp=13,
AuthVNC=14,
AuthTs=64,
AuthTc,
AuthAs,
AuthAc,
AuthTp,
AuthHr,
};
struct Ticketreq
{
char type;
char authid[ANAMELEN];
char authdom[DOMLEN];
char chal[CHALLEN];
char hostid[ANAMELEN];
char uid[ANAMELEN];
};
#define TICKREQLEN (3*ANAMELEN+CHALLEN+DOMLEN+1)
struct Ticket
{
char num;
char chal[CHALLEN];
char cuid[ANAMELEN];
char suid[ANAMELEN];
char key[DESKEYLEN];
};
#define TICKETLEN (CHALLEN+2*ANAMELEN+DESKEYLEN+1)
struct Authenticator
{
char num;
char chal[CHALLEN];
ulong id;
};
#define AUTHENTLEN (CHALLEN+4+1)
struct Passwordreq
{
char num;
char old[ANAMELEN];
char new[ANAMELEN];
char changesecret;
char secret[SECRETLEN];
};
#define PASSREQLEN (2*ANAMELEN+1+1+SECRETLEN)
struct OChapreply
{
uchar id;
char uid[ANAMELEN];
char resp[OMD5LEN];
};
struct OMSchapreply
{
char uid[ANAMELEN];
char LMresp[24];
char NTresp[24];
};
extern int convT2M(Ticket*, char*, char*);
extern void convM2T(char*, Ticket*, char*);
extern void convM2Tnoenc(char*, Ticket*);
extern int convA2M(Authenticator*, char*, char*);
extern void convM2A(char*, Authenticator*, char*);
extern int convTR2M(Ticketreq*, char*);
extern void convM2TR(char*, Ticketreq*);
extern int convPR2M(Passwordreq*, char*, char*);
extern void convM2PR(char*, Passwordreq*, char*);
extern int opasstokey(char*, char*);
extern int passtokey(char*, char*);
enum {
NVwrite = 1<<0,
NVwriteonerr = 1<<1,
};
struct Nvrsafe
{
char machkey[DESKEYLEN];
uchar machsum;
char authkey[DESKEYLEN];
uchar authsum;
char config[CONFIGLEN];
uchar configsum;
char authid[ANAMELEN];
uchar authidsum;
char authdom[DOMLEN];
uchar authdomsum;
};
extern uchar nvcsum(void*, int);
extern int readnvram(Nvrsafe*, int);
extern int authdial(char *netroot, char *authdom);
extern int _asgetticket(int, char*, char*);
extern int _asrdresp(int, char*, int);
extern int sslnegotiate(int, Ticket*, char**, char**);
extern int srvsslnegotiate(int, Ticket*, char**, char**);