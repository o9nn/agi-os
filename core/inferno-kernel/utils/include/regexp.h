#pragma src "/usr/inferno/libregexp"
#pragma lib "libregexp.a"
typedef struct Resub Resub;
typedef struct Reclass Reclass;
typedef struct Reinst Reinst;
typedef struct Reprog Reprog;
struct Resub{
union
{
char *sp;
Rune *rsp;
}s;
union
{
char *ep;
Rune *rep;
}e;
};
struct Reclass{
Rune *end;
Rune spans[64];
};
struct Reinst{
int type;
union {
Reclass *cp;
Rune r;
int subid;
Reinst *right;
}u1;
union {
Reinst *left;
Reinst *next;
}u2;
};
struct Reprog{
Reinst *startinst;
Reclass class[16];
Reinst firstinst[5];
};
extern Reprog *regcomp(char*);
extern Reprog *regcomplit(char*);
extern Reprog *regcompnl(char*);
extern void regerror(char*);
extern int regexec(Reprog*, char*, Resub*, int);
extern void regsub(char*, char*, Resub*, int);
extern int rregexec(Reprog*, Rune*, Resub*, int);
extern void rregsub(Rune*, Rune*, Resub*, int);