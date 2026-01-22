#include "common.h"
#include <ctype.h>
Biobuf in;
Biobuf out;
typedef struct Mtype Mtype;
typedef struct Hdef Hdef;
typedef struct Hline Hline;
typedef struct Part Part;
static int badfile(char *name);
static int badtype(char *type);
static void ctype(Part*, Hdef*, char*);
static void cencoding(Part*, Hdef*, char*);
static void cdisposition(Part*, Hdef*, char*);
static int decquoted(char *out, char *in, char *e);
static char* getstring(char *p, String *s, int dolower);
static void init_hdefs(void);
static int isattribute(char **pp, char *attr);
static int latin1toutf(char *out, char *in, char *e);
static String* mkboundary(void);
static Part* part(Part *pp);
static Part* passbody(Part *p, int dobound);
static void passnotheader(void);
static void passunixheader(void);
static Part* problemchild(Part *p);
static void readheader(Part *p);
static Hline* readhl(void);
static void readmtypes(void);
static int save(Part *p, char *file);
static void setfilename(Part *p, char *name);
static char* skiptosemi(char *p);
static char* skipwhite(char *p);
static String* tokenconvert(String *t);
static void writeheader(Part *p, int);
enum
{
Enone= 0,
Ebase64,
Equoted,
Dnone= 0,
Dinline,
Dfile,
Dignore,
PAD64= '=',
};
struct Part
{
Part *pp;
Hline *hl;
int disposition;
int encoding;
int badfile;
int badtype;
String *boundary;
int blen;
String *charset;
String *type;
String *filename;
Biobuf *tmpbuf;
};
struct Hline
{
Hline *next;
String *s;
};
struct Hdef
{
char *type;
void (*f)(Part*, Hdef*, char*);
int len;
};
Hdef hdefs[] =
{
{ "content-type:", ctype, },
{ "content-transfer-encoding:", cencoding, },
{ "content-disposition:", cdisposition, },
{ 0, },
};
struct Mtype {
Mtype *next;
char *ext;
char *gtype;
char *stype;
char class;
};
Mtype *mtypes;
int justreject;
char *savefile;
void
usage(void)
{
fprint(2, "usage: upas/vf [-r] [-s savefile]\n");
exits("usage");
}
void
main(int argc, char **argv)
{
ARGBEGIN{
case 'r':
justreject = 1;
break;
case 's':
savefile = EARGF(usage());
break;
default:
usage();
}ARGEND
if(argc)
usage();
Binit(&in, 0, OREAD);
Binit(&out, 1, OWRITE);
init_hdefs();
readmtypes();
passunixheader();
part(nil);
exits(0);
}
void
refuse(char *reason)
{
char *full;
static char msg[] =
"mail refused: we don't accept executable attachments";
full = smprint("%s: %s", msg, reason);
postnote(PNGROUP, getpid(), full);
exits(full);
}
static Part*
part(Part *pp)
{
Part *p, *np;
p = mallocz(sizeof *p, 1);
p->pp = pp;
readheader(p);
if(p->boundary != nil){
writeheader(p, 1);
np = passbody(p, 1);
if(np != p)
return np;
for(;;){
np = part(p);
if(np != p)
return np;
}
} else {
if(p->type && cistrcmp(s_to_c(p->type), "message/rfc822") == 0){
writeheader(p, 1);
passnotheader();
return part(p);
} else {
if(p->badtype || p->badfile){
if(p->badfile == 2){
if(savefile != nil)
save(p, savefile);
syslog(0, "vf", "vf rejected %s %s",
p->type? s_to_c(p->type): "?",
p->filename?s_to_c(p->filename):"?");
fprint(2, "The mail contained an executable attachment.\n");
fprint(2, "We refuse all mail containing such.\n");
refuse(nil);
}
np = problemchild(p);
if(np != p)
return np;
}
writeheader(p, 1);
return passbody(p, 1);
}
}
}
static void
readheader(Part *p)
{
Hline *hl, **l;
Hdef *hd;
l = &p->hl;
for(;;){
hl = readhl();
if(hl == nil)
break;
*l = hl;
l = &hl->next;
for(hd = hdefs; hd->type != nil; hd++){
if(cistrncmp(s_to_c(hl->s), hd->type, hd->len) == 0){
(*hd->f)(p, hd, s_to_c(hl->s));
break;
}
}
}
}
static Hline*
readhl(void)
{
Hline *hl;
String *s;
char *p;
int n;
p = Brdline(&in, '\n');
if(p == nil)
return nil;
n = Blinelen(&in);
if(memchr(p, ':', n) == nil){
Bseek(&in, -n, 1);
return nil;
}
s = s_nappend(s_new(), p, n);
for(;;){
p = Brdline(&in, '\n');
if(p == nil)
break;
n = Blinelen(&in);
if(*p != ' ' && *p != '\t'){
Bseek(&in, -n, 1);
break;
}
s = s_nappend(s, p, n);
}
hl = malloc(sizeof *hl);
hl->s = s;
hl->next = nil;
return hl;
}
static void
writeheader(Part *p, int xfree)
{
Hline *hl, *next;
for(hl = p->hl; hl != nil; hl = next){
Bprint(&out, "%s", s_to_c(hl->s));
if(xfree)
s_free(hl->s);
next = hl->next;
if(xfree)
free(hl);
}
if(xfree)
p->hl = nil;
}
static Part*
passbody(Part *p, int dobound)
{
Part *pp;
Biobuf *b;
char *cp;
for(;;){
if(p->tmpbuf){
b = p->tmpbuf;
cp = Brdline(b, '\n');
if(cp == nil){
Bterm(b);
p->tmpbuf = nil;
goto Stdin;
}
}else{
Stdin:
b = &in;
cp = Brdline(b, '\n');
}
if(cp == nil)
return nil;
for(pp = p; pp != nil; pp = pp->pp)
if(pp->boundary != nil
&& strncmp(cp, s_to_c(pp->boundary), pp->blen) == 0){
if(dobound)
Bwrite(&out, cp, Blinelen(b));
else
Bseek(b, -Blinelen(b), 1);
return pp;
}
Bwrite(&out, cp, Blinelen(b));
}
}
static vlong bodyoff;
static int
save(Part *p, char *file)
{
int fd;
char *cp;
Bterm(&out);
memset(&out, 0, sizeof(out));
fd = open(file, OWRITE);
if(fd < 0)
return -1;
seek(fd, 0, 2);
Binit(&out, fd, OWRITE);
cp = ctime(time(0));
cp[28] = 0;
Bprint(&out, "From virusfilter %s\n", cp);
writeheader(p, 0);
bodyoff = Boffset(&out);
passbody(p, 1);
Bprint(&out, "\n");
Bterm(&out);
close(fd);
memset(&out, 0, sizeof out);
Binit(&out, 1, OWRITE);
return 0;
}
static char*
savetmp(Part *p)
{
char *name;
int fd;
name = mktemp(smprint("%s/vf.XXXXXXXXXXX", UPASTMP));
if((fd = create(name, OWRITE|OEXCL, 0666)) < 0){
fprint(2, "%s: error creating temporary file: %r\n", argv0);
refuse("can't create temporary file");
}
close(fd);
if(save(p, name) < 0){
fprint(2, "%s: error saving temporary file: %r\n", argv0);
refuse("can't write temporary file");
}
if(p->tmpbuf){
fprint(2, "%s: error in savetmp: already have tmp file!\n",
argv0);
refuse("already have temporary file");
}
p->tmpbuf = Bopen(name, OREAD|ORCLOSE);
if(p->tmpbuf == nil){
fprint(2, "%s: error reading temporary file: %r\n", argv0);
refuse("error reading temporary file");
}
Bseek(p->tmpbuf, bodyoff, 0);
return name;
}
static int
runchecker(Part *p)
{
int pid;
char *name;
Waitmsg *w;
if(access("/mail/lib/validateattachment", AEXEC) < 0)
return 0;
name = savetmp(p);
fprint(2, "run checker %s\n", name);
switch(pid = fork()){
case -1:
sysfatal("fork: %r");
case 0:
dup(2, 1);
execl("/mail/lib/validateattachment", "validateattachment",
name, nil);
_exits("exec failed");
}
w = wait();
if(w == nil){
syslog(0, "mail", "vf wait failed: %r");
return 0;
}
if(w->pid != pid){
syslog(0, "mail", "vf wrong pid %d != %d", w->pid, pid);
return 0;
}
if(p->filename) {
free(name);
name = strdup(s_to_c(p->filename));
}
if(strstr(w->msg, "discard")){
syslog(0, "mail", "vf validateattachment rejected %s", name);
refuse("rejected by validateattachment");
}
if(strstr(w->msg, "accept")){
syslog(0, "mail", "vf validateattachment accepted %s", name);
return 1;
}
free(w);
free(name);
return 0;
}
static Part*
problemchild(Part *p)
{
Part *np;
Hline *hl;
String *boundary;
char *cp;
if(runchecker(p) > 0)
return p;
if(justreject)
return p;
fprint(2, "x\n");
syslog(0, "mail", "vf wrapped %s %s", p->type?s_to_c(p->type):"?",
p->filename?s_to_c(p->filename):"?");
fprint(2, "x\n");
boundary = mkboundary();
fprint(2, "x\n");
for(hl = p->hl; hl != nil; hl = hl->next)
if(cistrncmp(s_to_c(hl->s), "content-", 8) != 0)
Bprint(&out, "%s", s_to_c(hl->s));
fprint(2, "x\n");
Bprint(&out, "Content-Type: multipart/mixed;\n");
Bprint(&out, "\tboundary=\"%s\"\n", s_to_c(boundary));
Bprint(&out, "Content-Disposition: inline\n");
Bprint(&out, "\n");
Bprint(&out, "This is a multi-part message in MIME format.\n");
Bprint(&out, "--%s\n", s_to_c(boundary));
Bprint(&out, "Content-Disposition: inline\n");
Bprint(&out, "Content-Type: text/plain; charset=\"US-ASCII\"\n");
Bprint(&out, "Content-Transfer-Encoding: 7bit\n");
Bprint(&out, "\n");
Bprint(&out, "from postmaster@%s:\n", sysname());
Bprint(&out, "The following attachment had content that we can't\n");
Bprint(&out, "prove to be harmless.  To avoid possible automatic\n");
Bprint(&out, "execution, we changed the content headers.\n");
Bprint(&out, "The original header was:\n\n");
for(hl = p->hl; hl != nil; hl = hl->next)
if(cistrncmp(s_to_c(hl->s), "content-", 8) == 0)
Bprint(&out, "\t%s", s_to_c(hl->s));
Bprint(&out, "--%s\n", s_to_c(boundary));
if(p->filename)
s_append(p->filename, ".suspect");
else
p->filename = s_copy("file.suspect");
Bprint(&out, "Content-Type: application/octet-stream\n");
Bprint(&out, "Content-Disposition: attachment; filename=\"%s\"\n", s_to_c(p->filename));
switch(p->encoding){
case Enone:
break;
case Ebase64:
Bprint(&out, "Content-Transfer-Encoding: base64\n");
break;
case Equoted:
Bprint(&out, "Content-Transfer-Encoding: quoted-printable\n");
break;
}
fprint(2, "z\n");
np = passbody(p, 0);
fprint(2, "w\n");
Bprint(&out, "--%s--\n", s_to_c(boundary));
if(np && np->boundary){
cp = Brdline(&in, '\n');
Bwrite(&out, cp, Blinelen(&in));
}
fprint(2, "a %p\n", np);
return np;
}
static int
isattribute(char **pp, char *attr)
{
char *p;
int n;
n = strlen(attr);
p = *pp;
if(cistrncmp(p, attr, n) != 0)
return 0;
p += n;
while(*p == ' ')
p++;
if(*p++ != '=')
return 0;
while(*p == ' ')
p++;
*pp = p;
return 1;
}
static void
ctype(Part *p, Hdef *h, char *cp)
{
String *s;
cp += h->len;
cp = skipwhite(cp);
p->type = s_new();
cp = getstring(cp, p->type, 1);
if(badtype(s_to_c(p->type)))
p->badtype = 1;
while(*cp){
if(isattribute(&cp, "boundary")){
s = s_new();
cp = getstring(cp, s, 0);
p->boundary = s_reset(p->boundary);
s_append(p->boundary, "--");
s_append(p->boundary, s_to_c(s));
p->blen = s_len(p->boundary);
s_free(s);
} else if(cistrncmp(cp, "multipart", 9) == 0){
} else if(isattribute(&cp, "name")){
setfilename(p, cp);
} else if(isattribute(&cp, "charset")){
if(p->charset == nil)
p->charset = s_new();
cp = getstring(cp, s_reset(p->charset), 0);
}
cp = skiptosemi(cp);
}
}
static void
cencoding(Part *m, Hdef *h, char *p)
{
p += h->len;
p = skipwhite(p);
if(cistrncmp(p, "base64", 6) == 0)
m->encoding = Ebase64;
else if(cistrncmp(p, "quoted-printable", 16) == 0)
m->encoding = Equoted;
}
static void
cdisposition(Part *p, Hdef *h, char *cp)
{
cp += h->len;
cp = skipwhite(cp);
while(*cp){
if(cistrncmp(cp, "inline", 6) == 0){
p->disposition = Dinline;
} else if(cistrncmp(cp, "attachment", 10) == 0){
p->disposition = Dfile;
} else if(cistrncmp(cp, "filename=", 9) == 0){
cp += 9;
setfilename(p, cp);
}
cp = skiptosemi(cp);
}
}
static void
setfilename(Part *p, char *name)
{
if(p->filename == nil)
p->filename = s_new();
getstring(name, s_reset(p->filename), 0);
p->filename = tokenconvert(p->filename);
p->badfile = badfile(s_to_c(p->filename));
}
static char*
skipwhite(char *p)
{
while(isspace(*p))
p++;
return p;
}
static char*
skiptosemi(char *p)
{
while(*p && *p != ';')
p++;
while(*p == ';' || isspace(*p))
p++;
return p;
}
static char*
getstring(char *p, String *s, int dolower)
{
s = s_reset(s);
p = skipwhite(p);
if(*p == '"'){
p++;
for(;*p && *p != '"'; p++)
if(dolower)
s_putc(s, tolower(*p));
else
s_putc(s, *p);
if(*p == '"')
p++;
s_terminate(s);
return p;
}
for(; *p && !isspace(*p) && *p != ';'; p++)
if(dolower)
s_putc(s, tolower(*p));
else
s_putc(s, *p);
s_terminate(s);
return p;
}
static void
init_hdefs(void)
{
Hdef *hd;
static int already;
if(already)
return;
already = 1;
for(hd = hdefs; hd->type != nil; hd++)
hd->len = strlen(hd->type);
}
static String*
mkboundary(void)
{
char buf[32];
int i;
static int already;
if(already == 0){
srand((time(0)<<16)|getpid());
already = 1;
}
strcpy(buf, "upas-");
for(i = 5; i < sizeof(buf)-1; i++)
buf[i] = 'a' + nrand(26);
buf[i] = 0;
return s_copy(buf);
}
static void
passnotheader(void)
{
char *cp;
int i, n;
while((cp = Brdline(&in, '\n')) != nil){
n = Blinelen(&in);
for(i = 0; i < n-1; i++)
if(cp[i] != ' ' && cp[i] != '\t' && cp[i] != '\r'){
Bseek(&in, -n, 1);
return;
}
Bwrite(&out, cp, n);
}
}
static void
passunixheader(void)
{
char *p;
int n;
while((p = Brdline(&in, '\n')) != nil){
n = Blinelen(&in);
if(strncmp(p, "From ", 5) != 0){
Bseek(&in, -n, 1);
break;
}
Bwrite(&out, p, n);
}
}
static void
readmtypes(void)
{
Biobuf *b;
char *p;
char *f[6];
Mtype *m;
Mtype **l;
b = Bopen("/sys/lib/mimetype", OREAD);
if(b == nil)
return;
l = &mtypes;
while((p = Brdline(b, '\n')) != nil){
if(*p == '#')
continue;
p[Blinelen(b)-1] = 0;
if(tokenize(p, f, nelem(f)) < 5)
continue;
m = mallocz(sizeof *m, 1);
if(m == nil)
goto err;
m->ext = strdup(f[0]);
if(m->ext == 0)
goto err;
m->gtype = strdup(f[1]);
if(m->gtype == 0)
goto err;
m->stype = strdup(f[2]);
if(m->stype == 0)
goto err;
m->class = *f[4];
*l = m;
l = &(m->next);
}
Bterm(b);
return;
err:
if(m == nil)
return;
free(m->ext);
free(m->gtype);
free(m->stype);
free(m);
Bterm(b);
}
static int
badfile(char *name)
{
char *p;
Mtype *m;
int rv;
p = strrchr(name, '.');
if(p == nil)
return 0;
for(m = mtypes; m != nil; m = m->next)
if(cistrcmp(p, m->ext) == 0){
switch(m->class){
case 'm':
case 'y':
return 0;
case 'p':
*p = 0;
rv = badfile(name);
*p = '.';
return rv;
case 'r':
return 2;
}
}
return 1;
}
static int
badtype(char *type)
{
Mtype *m;
char *s, *fix;
int rv = 1;
fix = s = strchr(type, '/');
if(s != nil)
*s++ = 0;
else
s = "-";
for(m = mtypes; m != nil; m = m->next){
if(cistrcmp(type, m->gtype) != 0)
continue;
if(cistrcmp(s, m->stype) != 0)
continue;
switch(m->class){
case 'y':
case 'p':
case 'm':
rv = 0;
break;
}
break;
}
if(fix != nil)
*fix = '/';
return rv;
}
typedef struct Charset Charset;
struct Charset {
char *name;
int len;
int convert;
} charsets[] =
{
{ "us-ascii", 8, 1, },
{ "utf-8", 5, 0, },
{ "iso-8859-1", 10, 1, },
};
static String*
tokenconvert(String *t)
{
String *s;
char decoded[1024];
char utfbuf[2*1024];
int i, len;
char *e;
char *token;
token = s_to_c(t);
len = s_len(t);
if(token[0] != '=' || token[1] != '?' ||
token[len-2] != '?' || token[len-1] != '=')
goto err;
e = token+len-2;
token += 2;
for(i = 0; i < nelem(charsets); i++)
if(cistrncmp(charsets[i].name, token, charsets[i].len) == 0)
if(token[charsets[i].len] == '?'){
token += charsets[i].len + 1;
break;
}
if(i >= nelem(charsets))
goto err;
if(strlen(token) > sizeof(decoded)-1)
goto err;
if(cistrncmp(token, "b?", 2) == 0){
token += 2;
len = dec64((uchar*)decoded, sizeof(decoded), token, e-token);
decoded[len] = 0;
} else if(cistrncmp(token, "q?", 2) == 0){
token += 2;
len = decquoted(decoded, token, e);
if(len > 0 && decoded[len-1] == '\n')
len--;
decoded[len] = 0;
} else
goto err;
s = nil;
switch(charsets[i].convert){
case 0:
s = s_copy(decoded);
break;
case 1:
s = s_new();
latin1toutf(utfbuf, decoded, decoded+len);
s_append(s, utfbuf);
break;
}
return s;
err:
return s_clone(t);
}
enum
{
Self= 1,
Hex= 2,
};
uchar tableqp[256];
static void
initquoted(void)
{
int c;
memset(tableqp, 0, 256);
for(c = ' '; c <= '<'; c++)
tableqp[c] = Self;
for(c = '>'; c <= '~'; c++)
tableqp[c] = Self;
tableqp['\t'] = Self;
tableqp['='] = Hex;
}
static int
hex2int(int x)
{
if(x >= '0' && x <= '9')
return x - '0';
if(x >= 'A' && x <= 'F')
return (x - 'A') + 10;
if(x >= 'a' && x <= 'f')
return (x - 'a') + 10;
return 0;
}
static char*
decquotedline(char *out, char *in, char *e)
{
int c, soft;
while(e >= in && (*e == ' ' || *e == '\t' || *e == '\r' || *e == '\n'))
e--;
if(*e == '='){
soft = 1;
e--;
} else
soft = 0;
while(in <= e){
c = (*in++) & 0xff;
switch(tableqp[c]){
case Self:
*out++ = c;
break;
case Hex:
c = hex2int(*in++)<<4;
c |= hex2int(*in++);
*out++ = c;
break;
}
}
if(!soft)
*out++ = '\n';
*out = 0;
return out;
}
static int
decquoted(char *out, char *in, char *e)
{
char *p, *nl;
if(tableqp[' '] == 0)
initquoted();
p = out;
while((nl = strchr(in, '\n')) != nil && nl < e){
p = decquotedline(p, in, nl);
in = nl + 1;
}
if(in < e)
p = decquotedline(p, in, e-1);
if(*(p-1) != '\n'){
*p++ = '\n';
*p = 0;
}
return p - out;
}
static int
latin1toutf(char *out, char *in, char *e)
{
Rune r;
char *p;
p = out;
for(; in < e; in++){
r = (*in) & 0xff;
p += runetochar(p, &r);
}
*p = 0;
return p - out;
}