#pragma src "/sys/src/libauth"
#pragma lib "libauth.a"
typedef struct AuthInfo AuthInfo;
typedef struct Chalstate Chalstate;
typedef struct Chapreply Chapreply;
typedef struct MSchapreply MSchapreply;
typedef struct UserPasswd UserPasswd;
typedef struct AuthRpc AuthRpc;
enum
{
MAXCHLEN= 256,
MAXNAMELEN= 256,
MD5LEN= 16,
ARok = 0,
ARdone,
ARerror,
ARneedkey,
ARbadkey,
ARwritenext,
ARtoosmall,
ARtoobig,
ARrpcfailure,
ARphase,
AuthRpcMax = 4096,
};
struct AuthRpc
{
int afd;
char ibuf[AuthRpcMax+1];
char obuf[AuthRpcMax];
char *arg;
uint narg;
};
struct AuthInfo
{
char *cuid;
char *suid;
char *cap;
int nsecret;
uchar *secret;
};
struct Chalstate
{
char *user;
char chal[MAXCHLEN];
int nchal;
void *resp;
int nresp;
int afd;
AuthRpc *rpc;
char userbuf[MAXNAMELEN];
int userinchal;
};
struct Chapreply
{
uchar id;
char resp[MD5LEN];
};
struct MSchapreply
{
char LMresp[24];
char NTresp[24];
};
struct UserPasswd
{
char *user;
char *passwd;
};
extern int newns(char*, char*);
extern int addns(char*, char*);
extern int noworld(char*);
extern int amount(int, char*, int, char*);
extern int login(char*, char*, char*);
extern int httpauth(char*, char*);
typedef struct Attr Attr;
enum {
AttrNameval,
AttrQuery,
AttrDefault,
};
struct Attr
{
int type;
Attr *next;
char *name;
char *val;
};
typedef int AuthGetkey(char*);
int _attrfmt(Fmt*);
Attr *_copyattr(Attr*);
Attr *_delattr(Attr*, char*);
Attr *_findattr(Attr*, char*);
void _freeattr(Attr*);
Attr *_mkattr(int, char*, char*, Attr*);
Attr *_parseattr(char*);
char *_strfindattr(Attr*, char*);
#pragma varargck type "A" Attr*
extern AuthInfo* fauth_proxy(int, AuthRpc *rpc, AuthGetkey *getkey, char *params);
extern AuthInfo* auth_proxy(int fd, AuthGetkey *getkey, char *fmt, ...);
extern int auth_getkey(char*);
extern int (*amount_getkey)(char*);
extern void auth_freeAI(AuthInfo *ai);
extern int auth_chuid(AuthInfo *ai, char *ns);
extern Chalstate *auth_challenge(char*, ...);
extern AuthInfo* auth_response(Chalstate*);
extern int auth_respond(void*, uint, char*, uint, void*, uint, AuthGetkey *getkey, char*, ...);
extern void auth_freechal(Chalstate*);
extern AuthInfo* auth_userpasswd(char *user, char *passwd);
extern UserPasswd* auth_getuserpasswd(AuthGetkey *getkey, char*, ...);
extern AuthInfo* auth_getinfo(AuthRpc *rpc);
extern AuthRpc* auth_allocrpc(int afd);
extern Attr* auth_attr(AuthRpc *rpc);
extern void auth_freerpc(AuthRpc *rpc);
extern uint auth_rpc(AuthRpc *rpc, char *verb, void *a, int n);
extern int auth_wep(char*, char*, ...);
#pragma varargck argpos auth_proxy 3
#pragma varargck argpos auth_challenge 1
#pragma varargck argpos auth_respond 8
#pragma varargck argpos auth_getuserpasswd 2