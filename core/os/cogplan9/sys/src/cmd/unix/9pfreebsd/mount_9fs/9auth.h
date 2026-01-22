typedef struct Ticket Ticket;
typedef struct Ticketreq Ticketreq;
typedef struct Authenticator Authenticator;
typedef struct Nvrsafe Nvrsafe;
typedef struct Passwordreq Passwordreq;
typedef struct Chalstate Chalstate;
typedef struct Apopchalstate Apopchalstate;
typedef struct Chapreply Chapreply;
typedef struct MSchapreply MSchapreply;
enum
{
DOMLEN= 48,
U9AUTH_DESKEYLEN= 7,
CHALLEN= 8,
NETCHLEN= 16,
CONFIGLEN= 14,
SECRETLEN= 32,
APOPCHLEN= 256,
MD5LEN= 16,
KEYDBOFF= 8,
OKEYDBLEN= U9FS_NAMELEN+U9AUTH_DESKEYLEN+4+2,
KEYDBLEN= OKEYDBLEN+SECRETLEN,
U9AUTH_TCPPORT= 567,
U9AUTH_ILPORT= 566,
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
AuthTs=64,
AuthTc,
AuthAs,
AuthAc,
AuthTp,
};
struct Ticketreq
{
char type;
char authid[U9FS_NAMELEN];
char authdom[DOMLEN];
char chal[CHALLEN];
char hostid[U9FS_NAMELEN];
char uid[U9FS_NAMELEN];
};
#define TICKREQLEN (3*U9FS_NAMELEN+CHALLEN+DOMLEN+1)
struct Ticket
{
char num;
char chal[CHALLEN];
char cuid[U9FS_NAMELEN];
char suid[U9FS_NAMELEN];
char key[U9AUTH_DESKEYLEN];
};
#define TICKETLEN (CHALLEN+2*U9FS_NAMELEN+U9AUTH_DESKEYLEN+1)
struct Authenticator
{
char num;
char chal[CHALLEN];
u_long id;
};
#define AUTHENTLEN (CHALLEN+4+1)
struct Passwordreq
{
char num;
char old[U9FS_NAMELEN];
char new[U9FS_NAMELEN];
char changesecret;
char secret[SECRETLEN];
};
#define PASSREQLEN (2*U9FS_NAMELEN+1+1+SECRETLEN)
struct Nvrsafe
{
char machkey[U9AUTH_DESKEYLEN];
u_char machsum;
char authkey[U9AUTH_DESKEYLEN];
u_char authsum;
char config[CONFIGLEN];
u_char configsum;
char authid[U9FS_NAMELEN];
u_char authidsum;
char authdom[DOMLEN];
u_char authdomsum;
};
struct Chalstate
{
int afd;
int asfd;
char chal[NETCHLEN];
};
struct Apopchalstate
{
int afd;
int asfd;
char chal[APOPCHLEN];
};
struct Chapreply
{
u_char id;
char uid[U9FS_NAMELEN];
char resp[MD5LEN];
};
struct MSchapreply
{
char uid[U9FS_NAMELEN];
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
extern u_char nvcsum(void*, int);
extern int opasstokey(char*, char*);
extern int passtokey(char*, char*);
extern int authenticate(int, int);
extern int newns(char*, char*);
extern int addns(char*, char*);
extern int authdial(void);
extern int auth(int);
extern int srvauth(int, char*);
extern int nauth(int, Ticket*);
extern int nsrvauth(int, char*, Ticket*);
extern int getchal(Chalstate*, char*);
extern int chalreply(Chalstate*, char*);
extern int amount(int, char*, int, char*);
extern int apopchal(Apopchalstate*);
extern int apopreply(Apopchalstate*, char*, char*);
extern int login(char*, char*, char*);
extern int sslnegotiate(int, Ticket*, char**, char**);
extern int srvsslnegotiate(int, Ticket*, char**, char**);