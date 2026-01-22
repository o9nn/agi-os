#ifndef _PAX_REGEXP_H
#define _PAX_REGEXP_H
#define NSUBEXP  10
typedef struct regexp {
char *startp[NSUBEXP];
char *endp[NSUBEXP];
char regstart;
char reganch;
char *regmust;
int regmlen;
char program[1];
} regexp;
#define	MAGIC	0234
extern regexp *regcomp();
extern int regexec();
extern void regsub();
extern void regerror();
#endif