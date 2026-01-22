#include	<stdio.h>
#include	<signal.h>
#include	<stdlib.h>
#include	<string.h>
#include	"pic.h"
#include	"y.tab.h"
char	*version = "version July 5, 1993";
obj	**objlist = 0;
int	nobjlist = 0;
int	nobj	= 0;
Attr	*attr;
int	nattrlist = 0;
int	nattr	= 0;
Text	*text	= 0;
int	ntextlist = 0;
int	ntext	= 0;
int	ntext1	= 0;
double	curx	= 0;
double	cury	= 0;
int	hvmode	= R_DIR;
int	codegen	= 0;
char	*PEstring;
double	deltx	= 6;
double	delty	= 6;
int	dbg	= 0;
int	lineno	= 0;
char	*filename	= "-";
int	synerr	= 0;
int	anyerr	= 0;
char	*cmdname;
double	xmin	= 30000;
double	ymin	= 30000;
double	xmax	= -30000;
double	ymax	= -30000;
void	fpecatch(int);
void	getdata(void), setdefaults(void);
void	setfval(char *, double);
int	getpid(void);
main(int argc, char *argv[])
{
char buf[20];
signal(SIGFPE, fpecatch);
cmdname = argv[0];
while (argc > 1 && *argv[1] == '-') {
switch (argv[1][1]) {
case 'd':
dbg = atoi(&argv[1][2]);
if (dbg == 0)
dbg = 1;
fprintf(stderr, "%s\n", version);
break;
case 'V':
fprintf(stderr, "%s\n", version);
return 0;
}
argc--;
argv++;
}
setdefaults();
objlist = (obj **) grow((char *)objlist, "objlist", nobjlist += 1000, sizeof(obj *));
text = (Text *) grow((char *)text, "text", ntextlist += 1000, sizeof(Text));
attr = (Attr *) grow((char *)attr, "attr", nattrlist += 100, sizeof(Attr));
sprintf(buf, "/%d/", getpid());
pushsrc(String, buf);
definition("pid");
curfile = infile;
pushsrc(File, curfile->fname);
if (argc <= 1) {
curfile->fin = stdin;
curfile->fname = tostring("-");
getdata();
} else
while (argc-- > 1) {
if ((curfile->fin = fopen(*++argv, "r")) == NULL) {
fprintf(stderr, "%s: can't open %s\n", cmdname, *argv);
exit(1);
}
curfile->fname = tostring(*argv);
getdata();
fclose(curfile->fin);
free(curfile->fname);
}
return anyerr;
}
void fpecatch(int n)
{
ERROR "floating point exception %d", n FATAL;
}
char *grow(char *ptr, char *name, int num, int size)
{
char *p;
if (ptr == NULL)
p = malloc(num * size);
else
p = realloc(ptr, num * size);
if (p == NULL)
ERROR "can't grow %s to %d", name, num * size FATAL;
return p;
}
static struct {
char *name;
double val;
short scalable;
} defaults[] ={
"scale", SCALE, 1,
"lineht", HT, 1,
"linewid", HT, 1,
"moveht", HT, 1,
"movewid", HT, 1,
"dashwid", HT10, 1,
"boxht", HT, 1,
"boxwid", WID, 1,
"circlerad", HT2, 1,
"arcrad", HT2, 1,
"ellipseht", HT, 1,
"ellipsewid", WID, 1,
"arrowht", HT5, 1,
"arrowwid", HT10, 1,
"arrowhead", 2, 0,
"textht", 0.0, 1,
"textwid", 0.0, 1,
"maxpsht", MAXHT, 0,
"maxpswid", MAXWID, 0,
"fillval", 0.7, 0,
NULL, 0, 0
};
void setdefaults(void)
{
int i;
YYSTYPE v;
for (i = 0; defaults[i].name != NULL; i++) {
v.f = defaults[i].val;
makevar(tostring(defaults[i].name), VARNAME, v);
}
}
void resetvar(void)
{
int i, j;
if (nattr == 0) {
setdefaults();
return;
}
for (i = 0; i < nattr; i++) {
for (j = 0; defaults[j].name != NULL; j++)
if (strcmp(defaults[j].name, attr[i].a_val.p) == 0) {
setfval(defaults[j].name, defaults[j].val);
free(attr[i].a_val.p);
break;
}
}
}
void checkscale(char *s)
{
int i;
double scale;
if (strcmp(s, "scale") == 0) {
scale = getfval("scale");
for (i = 1; defaults[i].name != NULL; i++)
if (defaults[i].scalable)
setfval(defaults[i].name, defaults[i].val * scale);
}
}
void getdata(void)
{
char *p, buf[1000], buf1[100];
int ln;
void reset(void), openpl(char *), closepl(char *), print(void);
int yyparse(void);
curfile->lineno = 0;
printlf(1, curfile->fname);
while (fgets(buf, sizeof buf, curfile->fin) != NULL) {
curfile->lineno++;
if (*buf == '.' && *(buf+1) == 'P' && *(buf+2) == 'S') {
for (p = &buf[3]; *p == ' '; p++)
;
if (*p++ == '<') {
Infile svfile;
svfile = *curfile;
sscanf(p, "%s", buf1);
if ((curfile->fin=fopen(buf1, "r")) == NULL)
ERROR "can't open %s", buf1 FATAL;
curfile->fname = tostring(buf1);
getdata();
fclose(curfile->fin);
free(curfile->fname);
*curfile = svfile;
printlf(curfile->lineno, curfile->fname);
continue;
}
reset();
yyparse();
anyerr += synerr;
deltx = (xmax - xmin) / getfval("scale");
delty = (ymax - ymin) / getfval("scale");
if (buf[3] == ' ') {
if (sscanf(&buf[4],"%lf %lf", &deltx, &delty) < 2)
delty = deltx * (ymax-ymin) / (xmax-xmin);
}
dprintf("deltx = %g, delty = %g\n", deltx, delty);
if (codegen && !synerr) {
openpl(&buf[3]);
printlf(curfile->lineno+1, NULL);
print();
closepl(PEstring);
free(PEstring);
}
printlf(curfile->lineno+1, NULL);
fflush(stdout);
} else if (buf[0] == '.' && buf[1] == 'l' && buf[2] == 'f') {
if (sscanf(buf+3, "%d %s", &ln, buf1) == 2) {
free(curfile->fname);
printlf(curfile->lineno = ln, curfile->fname = tostring(buf1));
} else
printlf(curfile->lineno = ln, NULL);
} else
fputs(buf, stdout);
}
}
void reset(void)
{
obj *op;
int i;
extern int nstack;
extern	void freesymtab(struct symtab *);
for (i = 0; i < nobj; i++) {
op = objlist[i];
if (op->o_type == BLOCK)
freesymtab(op->o_symtab);
free((char *)objlist[i]);
}
nobj = 0;
nattr = 0;
for (i = 0; i < ntext; i++)
if (text[i].t_val)
free(text[i].t_val);
ntext = ntext1 = 0;
codegen = synerr = 0;
nstack = 0;
curx = cury = 0;
PEstring = 0;
hvmode = R_DIR;
xmin = ymin = 30000;
xmax = ymax = -30000;
}