#include	"mk.h"
static	int	bquote(Biobuf*, Bufblock*);
int
assline(Biobuf *bp, Bufblock *buf)
{
int c;
int lastc;
buf->current=buf->start;
while ((c = nextrune(bp, 1)) >= 0){
switch(c)
{
case '\r':
continue;
case '\n':
if (buf->current != buf->start) {
insert(buf, 0);
return 1;
}
break;
case '\\':
case '\'':
case '"':
rinsert(buf, c);
if (escapetoken(bp, buf, 1, c) == 0)
Exit();
break;
case '`':
if (bquote(bp, buf) == 0)
Exit();
break;
case '#':
lastc = '#';
while ((c = Bgetc(bp)) != '\n') {
if (c < 0)
goto eof;
if(c != '\r')
lastc = c;
}
mkinline++;
if (lastc == '\\')
break;
if (buf->current != buf->start) {
insert(buf, 0);
return 1;
}
break;
default:
rinsert(buf, c);
break;
}
}
eof:
insert(buf, 0);
return *buf->start != 0;
}
static int
bquote(Biobuf *bp, Bufblock *buf)
{
int c, line, term;
int start;
line = mkinline;
while((c = Bgetrune(bp)) == ' ' || c == '\t')
;
if(c == '{'){
term = '}';
while((c = Bgetrune(bp)) == ' ' || c == '\t')
;
} else
term = '`';
start = buf->current-buf->start;
for(;c > 0; c = nextrune(bp, 0)){
if(c == term){
insert(buf, '\n');
insert(buf,0);
buf->current = buf->start+start;
execinit();
execsh(0, buf->current, buf, envy);
return 1;
}
if(c == '\n')
break;
if(c == '\'' || c == '"' || c == '\\'){
insert(buf, c);
if(!escapetoken(bp, buf, 1, c))
return 0;
continue;
}
rinsert(buf, c);
}
SYNERR(line);
fprint(2, "missing closing %c after `\n", term);
return 0;
}
int
nextrune(Biobuf *bp, int elide)
{
int c;
for (;;) {
c = Bgetrune(bp);
if (c == '\\') {
if (Bgetrune(bp) == '\n') {
mkinline++;
if (elide)
continue;
return ' ';
}
Bungetrune(bp);
}
if (c == '\n')
mkinline++;
return c;
}
}