#include "a.h"
void
r_ec(int argc, Rune **argv)
{
if(argc == 1)
backslash = '\\';
else
backslash = argv[1][0];
}
void
r_eo(int argc, Rune **argv)
{
USED(argc);
USED(argv);
backslash = -2;
}
void
g_uf(int argc, Rune **argv)
{
USED(argc);
USED(argv);
}
void
r_cc(int argc, Rune **argv)
{
if(argc == 1)
dot = '.';
else
dot = argv[1][0];
}
void
r_c2(int argc, Rune **argv)
{
if(argc == 1)
tick = '\'';
else
tick = argv[1][0];
}
int
e_bang(void)
{
Rune *line;
line = readline(CopyMode);
out(line);
outrune('\n');
free(line);
return 0;
}
int
e_X(void)
{
int c;
while((c = getrune()) >= 0 && c != '\'' && c != '\n')
outrune(c);
if(c == '\n'){
warn("newline in %CX'...'", backslash);
outrune(c);
}
if(c < 0)
warn("eof in %CX'...'", backslash);
return 0;
}
int
e_quote(void)
{
int c;
if(inputmode&ArgMode){
ungetrune('"');
return '\\';
}
while((c = getrune()) >= 0 && c != '\n')
;
return '\n';
}
int
e_newline(void)
{
return 0;
}
int
e_e(void)
{
return backslash;
}
void
r_comment(Rune *name)
{
int c;
USED(name);
while((c = getrune()) >= 0 && c != '\n')
;
}
void
t10init(void)
{
addreq(L("ec"), r_ec, -1);
addreq(L("eo"), r_eo, 0);
addreq(L("lg"), r_nop, -1);
addreq(L("cc"), r_cc, -1);
addreq(L("c2"), r_c2, -1);
addreq(L("tr"), r_warn, -1);
addreq(L("ul"), r_nop, -1);
addraw(L("\\\""), r_comment);
addesc('!', e_bang, 0);
addesc('X', e_X, 0);
addesc('\"', e_quote, CopyMode|ArgMode);
addesc('\n', e_newline, CopyMode|ArgMode|HtmlMode);
addesc('e', e_e, 0);
}