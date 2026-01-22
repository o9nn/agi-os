#ifndef _UTFH_
#define _UTFH_ 1
typedef unsigned int Rune;
enum
{
UTFmax = 4,
Runesync = 0x80,
Runeself = 0x80,
Runeerror = 0xFFFD,
Runemax = 0x10FFFF,
Runemask = 0x1FFFFF,
};
extern int runetochar(char*, Rune*);
extern int chartorune(Rune*, char*);
extern int runelen(long);
extern int runenlen(Rune*, int);
extern int fullrune(char*, int);
extern int utflen(char*);
extern int utfnlen(char*, long);
extern char* utfrune(char*, long);
extern char* utfrrune(char*, long);
extern char* utfutf(char*, char*);
extern char* utfecpy(char*, char*, char*);
extern Rune* runestrcat(Rune*, Rune*);
extern Rune* runestrchr(Rune*, Rune);
extern int runestrcmp(Rune*, Rune*);
extern Rune* runestrcpy(Rune*, Rune*);
extern Rune* runestrncpy(Rune*, Rune*, long);
extern Rune* runestrecpy(Rune*, Rune*, Rune*);
extern Rune* runestrdup(Rune*);
extern Rune* runestrncat(Rune*, Rune*, long);
extern int runestrncmp(Rune*, Rune*, long);
extern Rune* runestrrchr(Rune*, Rune);
extern long runestrlen(Rune*);
extern Rune* runestrstr(Rune*, Rune*);
extern Rune tolowerrune(Rune);
extern Rune totitlerune(Rune);
extern Rune toupperrune(Rune);
extern int isalpharune(Rune);
extern int islowerrune(Rune);
extern int isspacerune(Rune);
extern int istitlerune(Rune);
extern int isupperrune(Rune);
#endif