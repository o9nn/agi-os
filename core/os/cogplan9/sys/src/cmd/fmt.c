#include <u.h>
#include <libc.h>
#include <bio.h>
#include <ctype.h>
int extraindent = 0;
int indent = 0;
int length = 70;
int join = 1;
int maxtab = 8;
Biobuf bin;
Biobuf bout;
typedef struct Word Word;
struct Word{
int	bol;
int	indent;
char	text[1];
};
void	fmt(void);
void
usage(void)
{
fprint(2, "usage: %s [-j] [-i indent] [-l length] [file...]\n", argv0);
exits("usage");
}
void
main(int argc, char **argv)
{
int i, f;
char *s, *err;
ARGBEGIN{
case 'i':
extraindent = atoi(EARGF(usage()));
break;
case 'j':
join = 0;
break;
case 'w':
case 'l':
length = atoi(EARGF(usage()));
break;
default:
usage();
}ARGEND
if(length <= indent){
fprint(2, "%s: line length<=indentation\n", argv0);
exits("length");
}
s=getenv("tabstop");
if(s!=nil && atoi(s)>0)
maxtab=atoi(s);
err = nil;
Binit(&bout, 1, OWRITE);
if(argc <= 0){
Binit(&bin, 0, OREAD);
fmt();
}else{
for(i=0; i<argc; i++){
f = open(argv[i], OREAD);
if(f < 0){
fprint(2, "%s: can't open %s: %r\n", argv0, argv[i]);
err = "open";
}else{
Binit(&bin, f, OREAD);
fmt();
Bterm(&bin);
if(i != argc-1)
Bputc(&bout, '\n');
}
}
}
exits(err);
}
int
indentof(char **linep)
{
int i, ind;
char *line;
ind = 0;
line = *linep;
for(i=0; line[i]; i++)
switch(line[i]){
default:
*linep = line;
return ind;
case ' ':
ind++;
break;
case '\t':
ind += maxtab;
ind -= ind%maxtab;
break;
}
*linep = "";
return indent;
}
Word**
addword(Word **words, int *nwordp, char *s, int l, int indent, int bol)
{
Word *w;
w = malloc(sizeof(Word)+l+1);
memmove(w->text, s, l);
w->text[l] = '\0';
w->indent = indent;
w->bol = bol;
words = realloc(words, (*nwordp+1)*sizeof(Word*));
words[(*nwordp)++] = w;
return words;
}
Word**
parseline(char *line, Word **words, int *nwordp)
{
int ind, l, bol;
ind = indentof(&line);
indent = ind;
bol = 1;
for(;;){
while(*line==' ' || *line=='\t')
line++;
if(*line == '\0'){
if(bol)
return addword(words, nwordp, "", 0, -1, bol);
break;
}
for(l=0; line[l]; l++)
if(line[l]==' ' || line[l]=='\t')
break;
words = addword(words, nwordp, line, l, indent, bol);
bol = 0;
line += l;
}
return words;
}
void
printindent(int w)
{
while(w >= maxtab){
Bputc(&bout, '\t');
w -= maxtab;
}
while(w > 0){
Bputc(&bout, ' ');
w--;
}
}
int
nspaceafter(char *s)
{
int n;
n = strlen(s);
if(n < 2)
return 1;
if(isupper(s[0]) && n < 4)
return 1;
if(strchr(".!?", s[n-1]) != nil)
return 2;
return 1;
}
void
printwords(Word **w, int nw)
{
int i, j, n, col, nsp;
for(i=0; i<nw; ){
if(w[i]->indent == -1){
Bputc(&bout, '\n');
if(++i == nw)
break;
}
col = extraindent+w[i]->indent;
printindent(col);
for(n=0;; n++){
Bprint(&bout, "%s", w[i]->text);
col += utflen(w[i]->text);
if(++i == nw)
break;
if(w[i]->indent != w[i-1]->indent)
break;
nsp = nspaceafter(w[i-1]->text);
if(col+nsp+utflen(w[i]->text) > extraindent+length)
break;
if(!join && w[i]->bol)
break;
for(j=0; j<nsp; j++)
Bputc(&bout, ' ');
col += nsp;
}
Bputc(&bout, '\n');
}
}
void
fmt(void)
{
char *s;
int i, nw;
Word **w;
nw = 0;
w = nil;
while((s = Brdstr(&bin, '\n', 1)) != nil){
w = parseline(s, w, &nw);
free(s);
}
printwords(w, nw);
for(i=0; i<nw; i++)
free(w[i]);
free(w);
}