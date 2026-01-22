#include "mk.h"
static Word *subsub(Word*, char*, char*);
static Word *expandvar(char**);
static Bufblock *varname(char**);
static Word *extractpat(char*, char**, char*, char*);
static int submatch(char*, Word*, Word*, int*, char**);
static Word *varmatch(char *, char**);
Word *
varsub(char **s)
{
Bufblock *b;
Word *w;
if(**s == '{')
return expandvar(s);
b = varname(s);
if(b == 0)
return 0;
w = varmatch(b->start, s);
freebuf(b);
return w;
}
static Bufblock*
varname(char **s)
{
Bufblock *b;
char *cp;
Rune r;
int n;
b = newbuf();
cp = *s;
for(;;){
n = chartorune(&r, cp);
if (!WORDCHR(r))
break;
rinsert(b, r);
cp += n;
}
if (b->current == b->start){
SYNERR(-1);
fprint(2, "missing variable name <%s>\n", *s);
freebuf(b);
return 0;
}
*s = cp;
insert(b, 0);
return b;
}
static Word*
varmatch(char *name, char **s)
{
Word *w;
Symtab *sym;
char *cp;
sym = symlook(name, S_VAR, 0);
if(sym){
for (w = (Word*)sym->value; w; w = w->next)
if(w->s && *w->s)
return wdup(w);
}
for(cp = *s; *cp == ' ' || *cp == '\t'; cp++)
;
*s = cp;
return 0;
}
static Word*
expandvar(char **s)
{
Word *w;
Bufblock *buf;
Symtab *sym;
char *cp, *begin, *end;
begin = *s;
(*s)++;
buf = varname(s);
if (buf == 0)
return 0;
cp = *s;
if (*cp == '}') {
(*s)++;
w = varmatch(buf->start, s);
freebuf(buf);
return w;
}
if (*cp != ':') {
SYNERR(-1);
fprint(2, "bad variable name <%s>\n", buf->start);
freebuf(buf);
return 0;
}
cp++;
end = charin(cp , "}");
if(end == 0){
SYNERR(-1);
fprint(2, "missing '}': %s\n", begin);
Exit();
}
*end = 0;
*s = end+1;
sym = symlook(buf->start, S_VAR, 0);
if(sym == 0 || sym->value == 0)
w = newword(buf->start);
else
w = subsub((Word*) sym->value, cp, end);
freebuf(buf);
return w;
}
static Word*
extractpat(char *s, char **r, char *term, char *end)
{
int save;
char *cp;
Word *w;
cp = charin(s, term);
if(cp){
*r = cp;
if(cp == s)
return 0;
save = *cp;
*cp = 0;
w = stow(s);
*cp = save;
} else {
*r = end;
w = stow(s);
}
return w;
}
static Word*
subsub(Word *v, char *s, char *end)
{
int nmid;
Word *head, *tail, *w, *h;
Word *a, *b, *c, *d;
Bufblock *buf;
char *cp, *enda;
a = extractpat(s, &cp, "=%&", end);
b = c = d = 0;
if(PERCENT(*cp))
b = extractpat(cp+1, &cp, "=", end);
if(*cp == '=')
c = extractpat(cp+1, &cp, "&%", end);
if(PERCENT(*cp))
d = stow(cp+1);
else if(*cp)
d = stow(cp);
head = tail = 0;
buf = newbuf();
for(; v; v = v->next){
h = w = 0;
if(submatch(v->s, a, b, &nmid, &enda)){
if(c){
h = w = wdup(c);
while(w->next)
w = w->next;
}
if(PERCENT(*cp) && nmid > 0){
if(w){
bufcpy(buf, w->s, strlen(w->s));
bufcpy(buf, enda, nmid);
insert(buf, 0);
free(w->s);
w->s = strdup(buf->start);
} else {
bufcpy(buf, enda, nmid);
insert(buf, 0);
h = w = newword(buf->start);
}
buf->current = buf->start;
}
if(d && *d->s){
if(w){
bufcpy(buf, w->s, strlen(w->s));
bufcpy(buf, d->s, strlen(d->s));
insert(buf, 0);
free(w->s);
w->s = strdup(buf->start);
w->next = wdup(d->next);
while(w->next)
w = w->next;
buf->current = buf->start;
} else
h = w = wdup(d);
}
}
if(w == 0)
h = w = newword(v->s);
if(head == 0)
head = h;
else
tail->next = h;
tail = w;
}
freebuf(buf);
delword(a);
delword(b);
delword(c);
delword(d);
return head;
}
static int
submatch(char *s, Word *a, Word *b, int *nmid, char **enda)
{
Word *w;
int n;
char *end;
n = 0;
for(w = a; w; w = w->next){
n = strlen(w->s);
if(strncmp(s, w->s, n) == 0)
break;
}
if(a && w == 0)
return 0;
*enda = s+n;
*nmid = strlen(s)-n;
end = *enda+*nmid;
for(w = b; w; w = w->next){
n = strlen(w->s);
if(strcmp(w->s, end-n) == 0){
*nmid -= n;
break;
}
}
if(b && w == 0)
return 0;
return 1;
}