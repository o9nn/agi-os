#define LOG "secstore"
#define SECSTORE_DIR "/adm/secstore"
enum {
MAXFILESIZE = 10*1024*1024,
};
enum {
Enabled = 1<<0,
STA = 1<<1,
};
typedef struct PW {
char *id;
ulong expire;
ushort status;
ushort failed;
char *other;
mpint *Hi;
} PW;
void freePW(PW*);
PW *getPW(char*, int);
char *getpassm(char*);
int putPW(PW*);
char *validatefile(char*f);
int PAKclient(SConn*, char*, char*, char**);
int PAKserver(SConn*, char*, char*, PW**);
char* PAK_Hi(char*, char*, mpint*, mpint*);