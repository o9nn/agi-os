typedef struct VtAuth VtAuth;
enum {
VtRError = 1,
VtQPing,
VtRPing,
VtQHello,
VtRHello,
VtQGoodbye,
VtRGoodbye,
VtQAuth0,
VtRAuth0,
VtQAuth1,
VtRAuth1,
VtQRead,
VtRRead,
VtQWrite,
VtRWrite,
VtQSync,
VtRSync,
VtMaxOp
};
enum {
VtStateAlloc,
VtStateConnected,
VtStateClosed,
};
enum {
VtAuthHello,
VtAuth0,
VtAuth1,
VtAuthOK,
VtAuthFailed,
};
struct VtAuth {
int state;
uchar client[VtScoreSize];
uchar sever[VtScoreSize];
};
struct VtSession {
VtLock *lk;
VtServerVtbl *vtbl;
int cstate;
int fd;
char fderror[ERRMAX];
VtAuth auth;
VtSha1 *inHash;
VtLock *inLock;
Packet *part;
VtSha1 *outHash;
VtLock *outLock;
int debug;
int version;
int ref;
char *uid;
char *sid;
int cryptoStrength;
int compression;
int crypto;
int codec;
};