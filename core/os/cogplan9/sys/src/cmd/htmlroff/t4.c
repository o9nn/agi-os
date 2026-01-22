#include "a.h"
int
e_space(void)
{
return 0xA0;
}
int
e_amp(void)
{
return Uempty;
}
int
e_c(void)
{
getrune();
bol = 1;
return 0;
}
void
r_br(int argc, Rune **argv)
{
USED(argc);
USED(argv);
br();
}
void
r_fi(int argc, Rune **argv)
{
USED(argc);
USED(argv);
nr(L(".fi"), 1);
}
void
r_nf(int argc, Rune **argv)
{
USED(argc);
USED(argv);
nr(L(".fi"), 0);
}
void
r_ad(int argc, Rune **argv)
{
int c, n;
nr(L(".j"), getnr(L(".j"))|1);
if(argc < 2)
return;
c = argv[1][0];
switch(c){
default:
fprint(2, "%L: bad adjust %C\n", c);
return;
case 'r':
n = 2*2|1;
break;
case 'l':
n = 0;
break;
case 'c':
n = 1*2|1;
break;
case 'b':
case 'n':
n = 0*2|1;
break;
case '0':
case '1':
case '2':
case '3':
case '4':
case '5':
n = c-'0';
break;
}
nr(L(".j"), n);
}
void
r_na(int argc, Rune **argv)
{
USED(argc);
USED(argv);
nr(L(".j"), getnr(L(".j"))&~1);
}
void
r_ce(int argc, Rune **argv)
{
if(argc < 2)
nr(L(".ce"), 1);
else
nr(L(".ce"), eval(argv[1]));
}
void
t4init(void)
{
nr(L(".fi"), 1);
nr(L(".j"), 1);
addreq(L("br"), r_br, 0);
addreq(L("fi"), r_fi, 0);
addreq(L("nf"), r_nf, 0);
addreq(L("ad"), r_ad, -1);
addreq(L("na"), r_na, 0);
addreq(L("ce"), r_ce, -1);
addesc(' ', e_space, 0);
addesc('p', e_warn, 0);
addesc('&', e_amp, 0);
addesc('c', e_c, 0);
}