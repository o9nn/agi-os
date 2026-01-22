#include <u.h>
#include <libc.h>
#include <bio.h>
#include "String.h"
extern char *
s_getline(Biobuf *fp, String *to)
{
int c;
int len=0;
s_terminate(to);
if ((c = Bgetc(fp)) < 0)
return 0;
for(;;) {
while(c==' ' || c=='\t' || c=='\n' || c=='\r')
c = Bgetc(fp);
if(c < 0)
return 0;
if(c == '#'){
do {
c = Bgetc(fp);
if(c < 0)
return 0;
} while(c != '\n');
continue;
}
break;
}
for(;;) {
len++;
switch(c) {
case -1:
s_terminate(to);
return len ? to->ptr-len : 0;
case '\\':
c = Bgetc(fp);
if (c != '\n') {
s_putc(to, '\\');
s_putc(to, c);
}
break;
case '\n':
s_terminate(to);
return len ? to->ptr-len : 0;
default:
s_putc(to, c);
break;
}
c = Bgetc(fp);
}
}