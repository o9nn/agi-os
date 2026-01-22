#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"../port/error.h"
enum {
Spec=		0xF800,
PF=		Spec|0x20,
View=		Spec|0x00,
KF=		0xF000,
Shift=		Spec|0x60,
Break=		Spec|0x61,
Ctrl=		Spec|0x62,
Latin=		Spec|0x63,
Caps=		Spec|0x64,
Num=		Spec|0x65,
Middle=		Spec|0x66,
Altgr=		Spec|0x67,
Kmouse=		Spec|0x100,
No=		0x00,
Home=		KF|13,
Up=		KF|14,
Pgup=		KF|15,
Print=		KF|16,
Left=		KF|17,
Right=		KF|18,
End=		KF|24,
Down=		View,
Pgdown=		KF|19,
Ins=		KF|20,
Del=		0x7F,
Scroll=		KF|21,
Nscan=	128,
Int=	0,
Ext,
Nscans,
};
Rune kbtab[Nscan] =
{
[0x00]	No,	0x1b,	'1',	'2',	'3',	'4',	'5',	'6',
[0x08]	'7',	'8',	'9',	'0',	'-',	'=',	'\b',	'\t',
[0x10]	'q',	'w',	'e',	'r',	't',	'y',	'u',	'i',
[0x18]	'o',	'p',	'[',	']',	'\n',	Ctrl,	'a',	's',
[0x20]	'd',	'f',	'g',	'h',	'j',	'k',	'l',	';',
[0x28]	'\'',	'`',	Shift,	'\\',	'z',	'x',	'c',	'v',
[0x30]	'b',	'n',	'm',	',',	'.',	'/',	Shift,	'*',
[0x38]	Latin,	' ',	Ctrl,	KF|1,	KF|2,	KF|3,	KF|4,	KF|5,
[0x40]	KF|6,	KF|7,	KF|8,	KF|9,	KF|10,	Num,	Scroll,	'7',
[0x48]	'8',	'9',	'-',	'4',	'5',	'6',	'+',	'1',
[0x50]	'2',	'3',	'0',	'.',	No,	No,	No,	KF|11,
[0x58]	KF|12,	No,	No,	No,	No,	No,	No,	No,
[0x60]	No,	No,	No,	No,	No,	No,	No,	No,
[0x68]	No,	No,	No,	No,	No,	No,	No,	No,
[0x70]	No,	No,	No,	No,	No,	No,	No,	No,
[0x78]	No,	View,	No,	Up,	No,	No,	No,	No,
};
Rune kbtabshift[Nscan] =
{
[0x00]	No,	0x1b,	'!',	'@',	'#',	'$',	'%',	'^',
[0x08]	'&',	'*',	'(',	')',	'_',	'+',	'\b',	'\t',
[0x10]	'Q',	'W',	'E',	'R',	'T',	'Y',	'U',	'I',
[0x18]	'O',	'P',	'{',	'}',	'\n',	Ctrl,	'A',	'S',
[0x20]	'D',	'F',	'G',	'H',	'J',	'K',	'L',	':',
[0x28]	'"',	'~',	Shift,	'|',	'Z',	'X',	'C',	'V',
[0x30]	'B',	'N',	'M',	'<',	'>',	'?',	Shift,	'*',
[0x38]	Latin,	' ',	Ctrl,	KF|1,	KF|2,	KF|3,	KF|4,	KF|5,
[0x40]	KF|6,	KF|7,	KF|8,	KF|9,	KF|10,	Num,	Scroll,	'7',
[0x48]	'8',	'9',	'-',	'4',	'5',	'6',	'+',	'1',
[0x50]	'2',	'3',	'0',	'.',	No,	No,	No,	KF|11,
[0x58]	KF|12,	No,	No,	No,	No,	No,	No,	No,
[0x60]	No,	No,	No,	No,	No,	No,	No,	No,
[0x68]	No,	No,	No,	No,	No,	No,	No,	No,
[0x70]	No,	No,	No,	No,	No,	No,	No,	No,
[0x78]	No,	Up,	No,	Up,	No,	No,	No,	No,
};
Rune kbtabesc1[Nscan] =
{
[0x00]	No,	No,	No,	No,	No,	No,	No,	No,
[0x08]	No,	No,	No,	No,	No,	No,	No,	No,
[0x10]	No,	No,	No,	No,	No,	No,	No,	No,
[0x18]	No,	No,	No,	No,	'\n',	Ctrl,	No,	No,
[0x20]	No,	No,	No,	No,	No,	No,	No,	No,
[0x28]	No,	No,	Shift,	No,	No,	No,	No,	No,
[0x30]	No,	No,	No,	No,	No,	'/',	No,	Print,
[0x38]	Altgr,	No,	No,	No,	No,	No,	No,	No,
[0x40]	No,	No,	No,	No,	No,	No,	Break,	Home,
[0x48]	Up,	Pgup,	No,	Left,	No,	Right,	No,	End,
[0x50]	Down,	Pgdown,	Ins,	Del,	No,	No,	No,	No,
[0x58]	No,	No,	No,	No,	No,	No,	No,	No,
[0x60]	No,	No,	No,	No,	No,	No,	No,	No,
[0x68]	No,	No,	No,	No,	No,	No,	No,	No,
[0x70]	No,	No,	No,	No,	No,	No,	No,	No,
[0x78]	No,	Up,	No,	No,	No,	No,	No,	No,
};
Rune kbtabaltgr[Nscan] =
{
[0x00]	No,	No,	No,	No,	No,	No,	No,	No,
[0x08]	No,	No,	No,	No,	No,	No,	No,	No,
[0x10]	No,	No,	No,	No,	No,	No,	No,	No,
[0x18]	No,	No,	No,	No,	'\n',	Ctrl,	No,	No,
[0x20]	No,	No,	No,	No,	No,	No,	No,	No,
[0x28]	No,	No,	Shift,	No,	No,	No,	No,	No,
[0x30]	No,	No,	No,	No,	No,	'/',	No,	Print,
[0x38]	Altgr,	No,	No,	No,	No,	No,	No,	No,
[0x40]	No,	No,	No,	No,	No,	No,	Break,	Home,
[0x48]	Up,	Pgup,	No,	Left,	No,	Right,	No,	End,
[0x50]	Down,	Pgdown,	Ins,	Del,	No,	No,	No,	No,
[0x58]	No,	No,	No,	No,	No,	No,	No,	No,
[0x60]	No,	No,	No,	No,	No,	No,	No,	No,
[0x68]	No,	No,	No,	No,	No,	No,	No,	No,
[0x70]	No,	No,	No,	No,	No,	No,	No,	No,
[0x78]	No,	Up,	No,	No,	No,	No,	No,	No,
};
Rune kbtabctrl[Nscan] =
{
[0x00]	No,	'', 	'', 	'', 	'', 	'', 	'', 	'',
[0x08]	'', 	'', 	'', 	'', 	'
', 	'', 	'\b',	'\t',
[0x10]	'', 	'', 	'', 	'', 	'', 	'', 	'', 	'\t',
[0x18]	'', 	'', 	'', 	'', 	'\n',	Ctrl,	'', 	'',
[0x20]	'', 	'', 	'', 	'\b',	'\n',	'', 	'', 	'',
[0x28]	'', 	No, 	Shift,	'', 	'', 	'', 	'', 	'',
[0x30]	'', 	'', 	'
', 	'', 	'', 	'', 	Shift,	'\n',
[0x38]	Latin,	No, 	Ctrl,	'', 	'', 	'', 	'', 	'',
[0x40]	'', 	'', 	'', 	'
', 	'', 	'', 	'', 	'',
[0x48]	'', 	'', 	'
', 	'', 	'', 	'', 	'', 	'',
[0x50]	'', 	'', 	'', 	'', 	No,	No,	No,	'',
[0x58]	'', 	No,	No,	No,	No,	No,	No,	No,
[0x60]	No,	No,	No,	No,	No,	No,	No,	No,
[0x68]	No,	No,	No,	No,	No,	No,	No,	No,
[0x70]	No,	No,	No,	No,	No,	No,	No,	No,
[0x78]	No,	'', 	No,	'\b',	No,	No,	No,	No,
};
int mouseshifted;
void (*kbdmouse)(int);
static int kdebug;
typedef struct Kbscan Kbscan;
struct Kbscan {
int	esc1;
int	esc2;
int	alt;
int	altgr;
int	caps;
int	ctl;
int	num;
int	shift;
int	collecting;
int	nk;
Rune	kc[5];
int	buttons;
};
Kbscan kbscans[Nscans];
void
kbdputsc(int c, int external)
{
int i, keyup;
Kbscan *kbscan;
if(external)
kbscan = &kbscans[Ext];
else
kbscan = &kbscans[Int];
if(kdebug)
print("sc %x ms %d\n", c, mouseshifted);
if(c == 0xe0){
kbscan->esc1 = 1;
return;
} else if(c == 0xe1){
kbscan->esc2 = 2;
return;
}
keyup = c & 0x80;
c &= 0x7f;
if(c > sizeof kbtab){
c |= keyup;
if(c != 0xFF)
print("unknown key %ux\n", c);
return;
}
if(kbscan->esc1){
c = kbtabesc1[c];
kbscan->esc1 = 0;
} else if(kbscan->esc2){
kbscan->esc2--;
return;
} else if(kbscan->shift)
c = kbtabshift[c];
else if(kbscan->altgr)
c = kbtabaltgr[c];
else if(kbscan->ctl)
c = kbtabctrl[c];
else
c = kbtab[c];
if(kbscan->caps && c<='z' && c>='a')
c += 'A' - 'a';
if(keyup){
switch(c){
case Latin:
kbscan->alt = 0;
break;
case Shift:
kbscan->shift = 0;
mouseshifted = 0;
if(kdebug)
print("shiftclr\n");
break;
case Ctrl:
kbscan->ctl = 0;
break;
case Altgr:
kbscan->altgr = 0;
break;
case Kmouse|1:
case Kmouse|2:
case Kmouse|3:
case Kmouse|4:
case Kmouse|5:
kbscan->buttons &= ~(1<<(c-Kmouse-1));
if(kbdmouse)
kbdmouse(kbscan->buttons);
break;
}
return;
}
if(!(c & (Spec|KF))){
if(kbscan->ctl)
if(kbscan->alt && c == Del)
exit(0);
if(!kbscan->collecting){
kbdputc(kbdq, c);
return;
}
kbscan->kc[kbscan->nk++] = c;
c = latin1(kbscan->kc, kbscan->nk);
if(c < -1)
return;
if(c != -1)
kbdputc(kbdq, c);
else
for(i=0; i<kbscan->nk; i++)
kbdputc(kbdq, kbscan->kc[i]);
kbscan->nk = 0;
kbscan->collecting = 0;
return;
} else {
switch(c){
case Caps:
kbscan->caps ^= 1;
return;
case Num:
kbscan->num ^= 1;
return;
case Shift:
kbscan->shift = 1;
if(kdebug)
print("shift\n");
mouseshifted = 1;
return;
case Latin:
kbscan->alt = 1;
if(!kbscan->ctl){
kbscan->collecting = 1;
kbscan->nk = 0;
}
return;
case Ctrl:
kbscan->ctl = 1;
return;
case Altgr:
kbscan->altgr = 1;
return;
case Kmouse|1:
case Kmouse|2:
case Kmouse|3:
case Kmouse|4:
case Kmouse|5:
kbscan->buttons |= 1<<(c-Kmouse-1);
if(kbdmouse)
kbdmouse(kbscan->buttons);
return;
case KF|11:
print("kbd debug on, F12 turns it off\n");
kdebug = 1;
break;
case KF|12:
kdebug = 0;
break;
}
}
kbdputc(kbdq, c);
}
void
kbdenable(void)
{
#ifdef notdef
kbdq = qopen(4*1024, 0, 0, 0);
if(kbdq == nil)
panic("kbdinit");
qnoblock(kbdq, 1);
#endif
kbscans[Int].num = 0;
}
void
kbdputmap(ushort m, ushort scanc, Rune r)
{
if(scanc >= Nscan)
error(Ebadarg);
switch(m) {
default:
error(Ebadarg);
case 0:
kbtab[scanc] = r;
break;
case 1:
kbtabshift[scanc] = r;
break;
case 2:
kbtabesc1[scanc] = r;
break;
case 3:
kbtabaltgr[scanc] = r;
break;
case 4:
kbtabctrl[scanc] = r;
break;
}
}
int
kbdgetmap(uint offset, int *t, int *sc, Rune *r)
{
if ((int)offset < 0)
error(Ebadarg);
*t = offset/Nscan;
*sc = offset%Nscan;
switch(*t) {
default:
return 0;
case 0:
*r = kbtab[*sc];
return 1;
case 1:
*r = kbtabshift[*sc];
return 1;
case 2:
*r = kbtabesc1[*sc];
return 1;
case 3:
*r = kbtabaltgr[*sc];
return 1;
case 4:
*r = kbtabctrl[*sc];
return 1;
}
}