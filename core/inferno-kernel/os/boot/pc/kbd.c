#include	"u.h"
#include	"lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
enum {
Data=		0x60,
Status=		0x64,
Inready=	0x01,
Outbusy=	0x02,
Sysflag=	0x04,
Cmddata=	0x08,
Inhibit=	0x10,
Minready=	0x20,
Rtimeout=	0x40,
Parity=	0x80,
Cmd=		0x64,
Spec=	0x80,
PF=	Spec|0x20,
View=	Spec|0x00,
KF=	Spec|0x40,
Shift=	Spec|0x60,
Break=	Spec|0x61,
Ctrl=	Spec|0x62,
Latin=	Spec|0x63,
Caps=	Spec|0x64,
Num=	Spec|0x65,
No=	Spec|0x7F,
Home=	KF|13,
Up=	KF|14,
Pgup=	KF|15,
Print=	KF|16,
Left=	View,
Right=	View,
End=	'\r',
Down=	View,
Pgdown=	View,
Ins=	KF|20,
Del=	0x7F,
};
uchar kbtab[] =
{
[0x00]	No,	0x1b,	'1',	'2',	'3',	'4',	'5',	'6',
[0x08]	'7',	'8',	'9',	'0',	'-',	'=',	'\b',	'\t',
[0x10]	'q',	'w',	'e',	'r',	't',	'y',	'u',	'i',
[0x18]	'o',	'p',	'[',	']',	'\n',	Ctrl,	'a',	's',
[0x20]	'd',	'f',	'g',	'h',	'j',	'k',	'l',	';',
[0x28]	'\'',	'`',	Shift,	'\\',	'z',	'x',	'c',	'v',
[0x30]	'b',	'n',	'm',	',',	'.',	'/',	Shift,	No,
[0x38]	Latin,	' ',	Caps,	KF|1,	KF|2,	KF|3,	KF|4,	KF|5,
[0x40]	KF|6,	KF|7,	KF|8,	KF|9,	KF|10,	Num,	KF|12,	Home,
[0x48]	No,	No,	No,	No,	No,	No,	No,	No,
[0x50]	No,	No,	No,	No,	No,	No,	No,	KF|11,
[0x58]	KF|12,	No,	No,	No,	No,	No,	No,	No,
};
uchar kbtabshift[] =
{
[0x00]	No,	0x1b,	'!',	'@',	'#',	'$',	'%',	'^',
[0x08]	'&',	'*',	'(',	')',	'_',	'+',	'\b',	'\t',
[0x10]	'Q',	'W',	'E',	'R',	'T',	'Y',	'U',	'I',
[0x18]	'O',	'P',	'{',	'}',	'\n',	Ctrl,	'A',	'S',
[0x20]	'D',	'F',	'G',	'H',	'J',	'K',	'L',	':',
[0x28]	'"',	'~',	Shift,	'|',	'Z',	'X',	'C',	'V',
[0x30]	'B',	'N',	'M',	'<',	'>',	'?',	Shift,	No,
[0x38]	Latin,	' ',	Caps,	KF|1,	KF|2,	KF|3,	KF|4,	KF|5,
[0x40]	KF|6,	KF|7,	KF|8,	KF|9,	KF|10,	Num,	KF|12,	Home,
[0x48]	No,	No,	No,	No,	No,	No,	No,	No,
[0x50]	No,	No,	No,	No,	No,	No,	No,	KF|11,
[0x58]	KF|12,	No,	No,	No,	No,	No,	No,	No,
};
uchar kbtabesc1[] =
{
[0x00]	No,	No,	No,	No,	No,	No,	No,	No,
[0x08]	No,	No,	No,	No,	No,	No,	No,	No,
[0x10]	No,	No,	No,	No,	No,	No,	No,	No,
[0x18]	No,	No,	No,	No,	No,	Ctrl,	No,	No,
[0x20]	No,	No,	No,	No,	No,	No,	No,	No,
[0x28]	No,	No,	No,	No,	No,	No,	No,	No,
[0x30]	No,	No,	No,	No,	No,	No,	No,	Print,
[0x38]	Latin,	No,	No,	No,	No,	No,	No,	No,
[0x40]	No,	No,	No,	No,	No,	No,	Break,	Home,
[0x48]	Up,	Pgup,	No,	Down,	No,	Right,	No,	End,
[0x50]	Left,	Pgdown,	Ins,	Del,	No,	No,	No,	No,
[0x58]	No,	No,	No,	No,	No,	No,	No,	No,
};
struct latin
{
uchar	l;
char	c[2];
}latintab[] = {
L'¡',	"!!",
L'¢',	"c|",
L'¢',	"c$",
L'£',	"l$",
L'¤',	"g$",
L'¥',	"y$",
L'¥',	"j$",
L'¦',	"||",
L'§',	"SS",
L'¨',	"\"\"",
L'©',	"cr",
L'©',	"cO",
L'ª',	"sa",
L'«',	"<<",
L'¬',	"no",
L'­',	"--",
L'®',	"rg",
L'¯',	"__",
L'°',	"s0",
L'±',	"+-",
L'²',	"s2",
L'³',	"s3",
L'´',	"''",
L'µ',	"mu",
L'¶',	"pg",
L'·',	"..",
L'¸',	",,",
L'¹',	"s1",
L'º',	"so",
L'»',	">>",
L'¼',	"14",
L'½',	"12",
L'¾',	"34",
L'¿',	"??",
L'À',	"A`",
L'Á',	"A'",
L'Â',	"A^",
L'Ã',	"A~",
L'Ä',	"A\"",
L'Ä',	"A:",
L'Å',	"Ao",
L'Å',	"AO",
L'Æ',	"Ae",
L'Æ',	"AE",
L'Ç',	"C,",
L'È',	"E`",
L'É',	"E'",
L'Ê',	"E^",
L'Ë',	"E\"",
L'Ë',	"E:",
L'Ì',	"I`",
L'Í',	"I'",
L'Î',	"I^",
L'Ï',	"I\"",
L'Ï',	"I:",
L'Ð',	"D-",
L'Ñ',	"N~",
L'Ò',	"O`",
L'Ó',	"O'",
L'Ô',	"O^",
L'Õ',	"O~",
L'Ö',	"O\"",
L'Ö',	"O:",
L'Ö',	"OE",
L'Ö',	"Oe",
L'×',	"xx",
L'Ø',	"O/",
L'Ù',	"U`",
L'Ú',	"U'",
L'Û',	"U^",
L'Ü',	"U\"",
L'Ü',	"U:",
L'Ü',	"UE",
L'Ü',	"Ue",
L'Ý',	"Y'",
L'Þ',	"P|",
L'Þ',	"Th",
L'Þ',	"TH",
L'ß',	"ss",
L'à',	"a`",
L'á',	"a'",
L'â',	"a^",
L'ã',	"a~",
L'ä',	"a\"",
L'ä',	"a:",
L'å',	"ao",
L'æ',	"ae",
L'ç',	"c,",
L'è',	"e`",
L'é',	"e'",
L'ê',	"e^",
L'ë',	"e\"",
L'ë',	"e:",
L'ì',	"i`",
L'í',	"i'",
L'î',	"i^",
L'ï',	"i\"",
L'ï',	"i:",
L'ð',	"d-",
L'ñ',	"n~",
L'ò',	"o`",
L'ó',	"o'",
L'ô',	"o^",
L'õ',	"o~",
L'ö',	"o\"",
L'ö',	"o:",
L'ö',	"oe",
L'÷',	"-:",
L'ø',	"o/",
L'ù',	"u`",
L'ú',	"u'",
L'û',	"u^",
L'ü',	"u\"",
L'ü',	"u:",
L'ü',	"ue",
L'ý',	"y'",
L'þ',	"th",
L'þ',	"p|",
L'ÿ',	"y\"",
L'ÿ',	"y:",
0,	0,
};
enum
{
Cscs1=		(1<<6),
Cmousedis=	(1<<5),
Ckbddis=	(1<<4),
Csf=		(1<<2),
Cmouseint=	(1<<1),
Ckbdint=	(1<<0),
};
static uchar ccc;
int
latin1(int k1, int k2)
{
struct latin *l;
for(l=latintab; l->l; l++)
if(k1==l->c[0] && k2==l->c[1])
return l->l;
return 0;
}
static int
outready(void)
{
int tries;
for(tries = 0; (inb(Status) & Outbusy); tries++){
if(tries > 500)
return -1;
delay(2);
}
return 0;
}
static int
inready(void)
{
int tries;
for(tries = 0; !(inb(Status) & Inready); tries++){
if(tries > 500)
return -1;
delay(2);
}
return 0;
}
void
i8042a20(void)
{
outready();
outb(Cmd, 0xD1);
outready();
outb(Data, 0xDF);
outready();
}
void
i8042reset(void)
{
int i, x;
#ifdef notdef
ushort *s = (ushort*)(KZERO|0x472);
*s = 0x1234;
#endif
outready();
outb(Cmd, 0xFE);
outready();
x = 0xDF;
for(i = 0; i < 5; i++){
x ^= 1;
outready();
outb(Cmd, 0xD1);
outready();
outb(Data, x);
delay(100);
}
}
static void
i8042intr(Ureg*, void*)
{
int s, c;
static int esc1, esc2;
static int alt, caps, ctl, num, shift;
static int lstate, k1, k2;
int keyup;
s = inb(Status);
if(!(s&Inready))
return;
c = inb(Data);
if(s & Minready)
return;
if(c == 0xe0){
esc1 = 1;
return;
} else if(c == 0xe1){
esc2 = 2;
return;
}
keyup = c&0x80;
c &= 0x7f;
if(c > sizeof kbtab){
c |= keyup;
if(c != 0xFF)
print("unknown key %ux\n", c);
return;
}
if(esc1){
c = kbtabesc1[c];
esc1 = 0;
} else if(esc2){
esc2--;
return;
} else if(shift)
c = kbtabshift[c];
else
c = kbtab[c];
if(caps && c<='z' && c>='a')
c += 'A' - 'a';
if(keyup){
switch(c){
case Latin:
alt = 0;
break;
case Shift:
shift = 0;
break;
case Ctrl:
ctl = 0;
break;
}
return;
}
if(!(c & Spec)){
if(ctl){
if(alt && c == Del)
warp86("\nCtrl-Alt-Del\n", 0);
c &= 0x1f;
}
switch(lstate){
case 1:
k1 = c;
lstate = 2;
return;
case 2:
k2 = c;
lstate = 0;
c = latin1(k1, k2);
if(c == 0){
kbdchar(k1);
c = k2;
}
default:
break;
}
} else {
switch(c){
case Caps:
caps ^= 1;
return;
case Num:
num ^= 1;
return;
case Shift:
shift = 1;
return;
case Latin:
alt = 1;
lstate = 1;
return;
case Ctrl:
ctl = 1;
return;
}
}
kbdchar(c);
}
static char *initfailed = "kbd init failed\n";
void
i8042init(void)
{
int c;
while((c = inb(Status)) & (Outbusy | Inready))
if(c & Inready)
inb(Data);
outb(Cmd, 0x20);
if(inready() < 0){
print("kbdinit: can't read ccc\n");
ccc = 0;
} else
ccc = inb(Data);
ccc &= ~Ckbddis;
ccc |= Csf | Ckbdint | Cscs1;
if(outready() < 0)
print(initfailed);
outb(Cmd, 0x60);
if(outready() < 0)
print(initfailed);
outb(Data, ccc);
if(outready() < 0)
print(initfailed);
setvec(VectorKBD, i8042intr, 0);
}