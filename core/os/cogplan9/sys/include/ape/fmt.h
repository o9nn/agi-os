#ifndef _PLAN9_SOURCE
This header file is an extension to ANSI/POSIX
#endif
#ifndef __FMT_H_
#define __FMT_H_
#pragma src "/sys/src/ape/lib/fmt"
#pragma lib "/$M/lib/ape/libfmt.a"
#include <u.h>
#include <stdarg.h>
#include <utf.h>
typedef struct Fmt Fmt;
struct Fmt{
unsigned char runes;
void *start;
void *to;
void *stop;
int (*flush)(Fmt *);
void *farg;
int nfmt;
va_list args;
int r;
int width;
int prec;
unsigned long flags;
};
enum{
FmtWidth = 1,
FmtLeft = FmtWidth << 1,
FmtPrec = FmtLeft << 1,
FmtSharp = FmtPrec << 1,
FmtSpace = FmtSharp << 1,
FmtSign = FmtSpace << 1,
FmtZero = FmtSign << 1,
FmtUnsigned = FmtZero << 1,
FmtShort = FmtUnsigned << 1,
FmtLong = FmtShort << 1,
FmtVLong = FmtLong << 1,
FmtComma = FmtVLong << 1,
FmtByte = FmtComma << 1,
FmtLDouble = FmtByte << 1,
FmtFlag = FmtLDouble << 1
};
#ifdef __cplusplus
extern "C" {
#endif
extern int print(char*, ...);
extern char* seprint(char*, char*, char*, ...);
extern char* vseprint(char*, char*, char*, va_list);
extern int snprint(char*, int, char*, ...);
extern int vsnprint(char*, int, char*, va_list);
extern char* smprint(char*, ...);
extern char* vsmprint(char*, va_list);
extern int sprint(char*, char*, ...);
extern int fprint(int, char*, ...);
extern int vfprint(int, char*, va_list);
extern int runesprint(Rune*, char*, ...);
extern int runesnprint(Rune*, int, char*, ...);
extern int runevsnprint(Rune*, int, char*, va_list);
extern Rune* runeseprint(Rune*, Rune*, char*, ...);
extern Rune* runevseprint(Rune*, Rune*, char*, va_list);
extern Rune* runesmprint(char*, ...);
extern Rune* runevsmprint(char*, va_list);
extern int fmtfdinit(Fmt*, int, char*, int);
extern int fmtfdflush(Fmt*);
extern int fmtstrinit(Fmt*);
extern char* fmtstrflush(Fmt*);
extern int runefmtstrinit(Fmt*);
extern int quotestrfmt(Fmt *f);
extern void quotefmtinstall(void);
extern int (*fmtdoquote)(int);
extern int fmtinstall(int, int (*)(Fmt*));
extern int dofmt(Fmt*, char*);
extern int fmtprint(Fmt*, char*, ...);
extern int fmtvprint(Fmt*, char*, va_list);
extern int fmtrune(Fmt*, int);
extern int fmtstrcpy(Fmt*, char*);
extern double fmtstrtod(const char *, char **);
extern double fmtcharstod(int(*)(void*), void*);
extern void werrstr(const char*, ...);
#ifdef __cplusplus
}
#endif
#endif