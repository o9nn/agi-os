#define _BSD_EXTENSION
#define _POSIX_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <limits.h>
#include <ctype.h>
#include <string.h>
#define NROFF (!TROFF)
#ifndef TMACDIR
#define TMACDIR "lib/tmac/tmac."
#endif
#ifndef FONTDIR
#define FONTDIR "lib/font"
#endif
#ifndef NTERMDIR
#define NTERMDIR "lib/term/tab."
#endif
#ifndef TDEVNAME
#define TDEVNAME "post"
#endif
#ifndef NDEVNAME
#define NDEVNAME "37"
#endif
#ifndef TEXHYPHENS
#define TEXHYPHENS "/usr/lib/tex/macros/hyphen.tex"
#endif
#ifndef ALTHYPHENS
#define ALTHYPHENS "lib/tmac/hyphen.tex"
#endif
typedef unsigned char Uchar;
typedef unsigned short Ushort;
typedef long Tchar;
typedef struct Blockp Blockp;
typedef struct Diver Diver;
typedef struct Stack Stack;
typedef struct Divsiz Divsiz;
typedef struct Contab Contab;
typedef struct Numtab Numtab;
typedef struct Numerr Numerr;
typedef struct Env Env;
typedef struct Term Term;
typedef struct Chwid Chwid;
typedef struct Font Font;
typedef struct Spnames Spnames;
typedef struct Wcache Wcache;
typedef struct Tbuf Tbuf;
#ifdef BSD4_2
#define OUT (obufp += strlen(sprintf(obufp,
#define PUT ))) > obuf+BUFSIZ ? flusho() : 1
#else
#define OUT (obufp += sprintf(obufp,
#define PUT )) > obuf+BUFSIZ ? flusho() : 1
#endif
#define oputs(a) OUT "%s", a PUT
#define oput(c) ( *obufp++ = (c), obufp > obuf+BUFSIZ ? flusho() : 1 )
extern char errbuf[];
#define ERROR sprintf(errbuf,
#define WARN ), errprint()
#define FATAL ), errprint(), exit(1)
#define PS 10
#define FT 1
#define ULFONT 2
#define BDFONT 3
#define BIFONT 4
#define LL (unsigned) 65*INCH/10
#define VS ((12*INCH)/72)
#define EMPTS(pts) (((long)Inch*(pts) + 36) / 72)
#define EM (TROFF? EMPTS(pts): t.Em)
#define INCH (TROFF? Inch: 240)
#define HOR (TROFF? Hor: t.Adj)
#define VERT (TROFF? Vert: t.Vert)
#define PO (TROFF? Inch: 0)
#define SPS (TROFF? EMPTS(pts)/3: INCH/10)
#define SS (TROFF? 12: INCH/10)
#define ICS (TROFF? EMPTS(pts): 2*INCH/10)
#define DTAB (TROFF? (INCH/2): 0)
#define LEADER 001
#define IMP 004
#define TAB 011
#define RPT 014
#define CHARHT 015
#define SLANT 016
#define DRAWFCN 017
# define DRAWLINE 'l'
# define DRAWCIRCLE 'c'
# define DRAWELLIPSE 'e'
# define DRAWARC 'a'
# define DRAWSPLINE '~'
# define DRAWBUILD 'b'
#define LEFT 020
#define RIGHT 021
#define FILLER 022
#define XON 023
#define OHC 024
#define CONT 025
#define PRESC 026
#define UNPAD 027
#define XPAR 030
#define FLSS 031
#define WORDSP 032
#define ESC 033
#define XOFF 034
#define HX 035
#define MOTCH 036
#define HYPHEN c_hyphen
#define EMDASH c_emdash
#define RULE c_rule
#define MINUS c_minus
#define LIG_FI c_fi
#define LIG_FL c_fl
#define LIG_FF c_ff
#define LIG_FFI c_ffi
#define LIG_FFL c_ffl
#define ACUTE c_acute
#define GRAVE c_grave
#define UNDERLINE c_under
#define ROOTEN c_rooten
#define BOXRULE c_boxrule
#define LEFTHAND c_lefthand
#define DAGGER c_dagger
#define HYPHALG 1
#define MAXFONTS 99
#define NM 90
#define NN NNAMES
#define NNAMES 15
#define NIF 15
#define NS 128
#define NTM 1024
#define NEV 3
#define EVLSZ 10
#define STACKSIZE (12*1024)
#define NHYP 10
#define NHEX 512
#define NTAB 100
#define NSO 5
#define NMF 5
#define WDSIZE 500
#define LNSIZE 4000
#define OLNSIZE 5000
#define NDI 5
#define ALPHABET alphabet
#define NCHARS (8*1024)
#define NROFFCHARS 1024
#define NTRTAB NCHARS
#define NWIDCACHE NCHARS
#define NTRAP 20
#define NPN 20
#define FBUFSZ 512
#define IBUFSZ 4096
#define NC 1024
#define NOV 10
#define NPP 10
#define MOT (01uL << 16)
#define VMOT (01uL << 30)
#define NMOT (01uL << 29)
#define MAXMOT 0xFFFF
#define ismot(n) ((n) & MOT)
#define isvmot(n) (((n) & (MOT|VMOT)) == (MOT|VMOT))
#define isnmot(n) (((n) & (MOT|NMOT)) == (MOT|NMOT))
#define absmot(n) ((n) & 0xFFFF)
#define ZBIT (01uL << 31)
#define iszbit(n) ((n) & ZBIT)
#define FSHIFT 17
#define SSHIFT (FSHIFT+7)
#define SMASK (0177uL << SSHIFT)
#define FMASK (0177uL << FSHIFT)
#define SFMASK (SMASK|FMASK)
#define sbits(n) (((n) >> SSHIFT) & 0177)
#define fbits(n) (((n) >> FSHIFT) & 0177)
#define sfbits(n) (((n) & SFMASK) >> FSHIFT)
#define cbits(n) ((n) & 0x1FFFF)
extern int realcbits(Tchar);
#define setsbits(n,s) n = (n & ~SMASK) | (Tchar)(s) << SSHIFT
#define setfbits(n,f) n = (n & ~FMASK) | (Tchar)(f) << FSHIFT
#define setsfbits(n,sf) n = (n & ~SFMASK) | (Tchar)(sf) << FSHIFT
#define setcbits(n,c) n = (n & ~0xFFFFuL | (c))
#define BYTEMASK 0377
#define BYTE 8
#define SHORTMASK 0XFFFF
#define SHORT 16
#define TABMASK ((unsigned) INT_MAX >> 1)
#define RTAB ((TABMASK << 1) & ~TABMASK)
#define CTAB (RTAB << 1)
#define TABBIT 02
#define LDRBIT 04
#define FCBIT 010
#define PAIR(A,B) (A|(B<<SHORT))
extern int Inch, Hor, Vert, Unitwidth;
struct Spnames
{
int *n;
char *v;
};
extern Spnames spnames[];
#define NBLIST 2048
#define BLK 128
#define rbf0(o) (blist[bindex(o)].bp[boffset(o)])
#define bindex(o) ((o) / BLK)
#define boffset(o) ((o) & (BLK-1))
#define pastend(o) (((o) & (BLK-1)) == 0)
#define incoff(o) ( (((o)+1) & (BLK-1)) ? o+1 : blist[bindex(o)].nextoff )
#define skipline(f) while (getc(f) != '\n')
#define is(s) (strcmp(cmd, s) == 0)
#define eq(s1, s2) (strcmp(s1, s2) == 0)
typedef unsigned long Offset;
struct Blockp {
Tchar *bp;
Offset nextoff;
};
extern Blockp *blist;
#define RD_OFFSET (1 * BLK)
struct Diver {
Offset op;
int dnl;
int dimac;
int ditrap;
int ditf;
int alss;
int blss;
int nls;
int mkline;
int maxl;
int hnl;
int curd;
};
struct Stack {
int nargs;
Stack *pframe;
Offset pip;
int pnchar;
Tchar prchar;
int ppendt;
Tchar pch;
Tchar *lastpbp;
int mname;
};
extern Stack s;
struct Divsiz {
int dix;
int diy;
};
struct Contab {
unsigned int rq;
Contab *link;
void (*f)(void);
Offset mx;
Offset emx;
Divsiz *divsiz;
};
#define C(a,b) {a, 0, b, 0, 0}
extern Contab contab[NM];
struct Numtab {
unsigned int r;
int val;
short fmt;
short inc;
Numtab *link;
};
extern Numtab numtab[NN];
#define PN 0
#define NL 1
#define YR 2
#define HP 3
#define CT 4
#define DN 5
#define MO 6
#define DY 7
#define DW 8
#define LN 9
#define DL 10
#define ST 11
#define SB 12
#define CD 13
#define PID 14
struct Wcache {
short fontpts;
short width;
};
struct Tbuf {
Tchar *_bufp;
unsigned int _size;
};
#define ics envp->_ics
#define sps envp->_sps
#define spacesz envp->_spacesz
#define lss envp->_lss
#define lss1 envp->_lss1
#define ll envp->_ll
#define ll1 envp->_ll1
#define lt envp->_lt
#define lt1 envp->_lt1
#define ic envp->_ic
#define icf envp->_icf
#define chbits envp->_chbits
#define spbits envp->_spbits
#define nmbits envp->_nmbits
#define apts envp->_apts
#define apts1 envp->_apts1
#define pts envp->_pts
#define pts1 envp->_pts1
#define font envp->_font
#define font1 envp->_font1
#define ls envp->_ls
#define ls1 envp->_ls1
#define ad envp->_ad
#define nms envp->_nms
#define ndf envp->_ndf
#define nmwid envp->_nmwid
#define fi envp->_fi
#define cc envp->_cc
#define c2 envp->_c2
#define ohc envp->_ohc
#define tdelim envp->_tdelim
#define hyf envp->_hyf
#define hyoff envp->_hyoff
#define hyphalg envp->_hyphalg
#define un1 envp->_un1
#define tabc envp->_tabc
#define dotc envp->_dotc
#define adsp envp->_adsp
#define adrem envp->_adrem
#define lastl envp->_lastl
#define nel envp->_nel
#define admod envp->_admod
#define wordp envp->_wordp
#define spflg envp->_spflg
#define linep envp->_linep
#define wdend envp->_wdend
#define wdstart envp->_wdstart
#define wne envp->_wne
#define ne envp->_ne
#define nc envp->_nc
#define nb envp->_nb
#define lnmod envp->_lnmod
#define nwd envp->_nwd
#define nn envp->_nn
#define ni envp->_ni
#define ul envp->_ul
#define cu envp->_cu
#define ce envp->_ce
#define in envp->_in
#define in1 envp->_in1
#define un envp->_un
#define wch envp->_wch
#define pendt envp->_pendt
#define pendw envp->_pendw
#define pendnf envp->_pendnf
#define spread envp->_spread
#define it envp->_it
#define itmac envp->_itmac
#define hyptr envp->_hyptr
#define tabtab envp->_tabtab
#define line envp->_line._bufp
#define lnsize envp->_line._size
#define word envp->_word._bufp
#define wdsize envp->_word._size
#define oline _oline._bufp
#define olnsize _oline._size
struct Env {
int _ics;
int _sps;
int _spacesz;
int _lss;
int _lss1;
int _ll;
int _ll1;
int _lt;
int _lt1;
Tchar _ic;
int _icf;
Tchar _chbits;
Tchar _spbits;
Tchar _nmbits;
int _apts;
int _apts1;
int _pts;
int _pts1;
int _font;
int _font1;
int _ls;
int _ls1;
int _ad;
int _nms;
int _ndf;
int _nmwid;
int _fi;
int _cc;
int _c2;
int _ohc;
int _tdelim;
int _hyf;
int _hyoff;
int _hyphalg;
int _un1;
int _tabc;
int _dotc;
int _adsp;
int _adrem;
int _lastl;
int _nel;
int _admod;
Tchar *_wordp;
int _spflg;
Tchar *_linep;
Tchar *_wdend;
Tchar *_wdstart;
int _wne;
int _ne;
int _nc;
int _nb;
int _lnmod;
int _nwd;
int _nn;
int _ni;
int _ul;
int _cu;
int _ce;
int _in;
int _in1;
int _un;
int _wch;
int _pendt;
Tchar *_pendw;
int _pendnf;
int _spread;
int _it;
int _itmac;
Tchar *_hyptr[NHYP];
long _tabtab[NTAB];
Tbuf _line;
Tbuf _word;
};
extern Env env[];
extern Env *envp;
enum { MBchar = 'U', Troffchar = 'C', Number = 'N', Install = 'i', Lookup = 'l' };
struct Chwid {
Ushort num;
Ushort code;
char *str;
Uchar wid;
Uchar kern;
};
struct Font {
int name;
char longname[64];
char *truename;
int nchars;
char specfont;
int spacewidth;
int defaultwidth;
Chwid *wp;
char ligfont;
};
#define LFF 01
#define LFI 02
#define LFL 04
#define LFFI 010
#define LFFL 020
#define TRNARGS 01
#define TRREQ 02
#define TRMAC 04
#define RQERR 01
extern Term t;
struct Term {
int bset;
int breset;
int Hor;
int Vert;
int Newline;
int Char;
int Em;
int Halfline;
int Adj;
char *twinit;
char *twrest;
char *twnl;
char *hlr;
char *hlf;
char *flr;
char *bdon;
char *bdoff;
char *iton;
char *itoff;
char *ploton;
char *plotoff;
char *up;
char *down;
char *right;
char *left;
Font tfont;
};
extern Term t;
struct Numerr {
char type;
char esc;
char escarg;
unsigned int req;
};