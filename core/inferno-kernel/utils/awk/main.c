char	*version = "version 20001115";
#define DEBUG
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include "awk.h"
#include "ytab.h"
extern	char	**environ;
extern	int	nfields;
int	dbg	= 0;
char	*cmdname;
extern	FILE	*yyin;
char	*lexprog;
extern	int errorflag;
int	compile_time = 2;
char	*pfile[20];
int	npfile = 0;
int	curpfile = 0;
int	safe	= 0;
int main(int argc, char *argv[])
{
char *fs = NULL, *marg;
int temp;
cmdname = argv[0];
if (argc == 1) {
fprintf(stderr, "Usage: %s [-f programfile | 'program'] [-Ffieldsep] [-v var=value] [files]\n", cmdname);
exit(1);
}
signal(SIGFPE, fpecatch);
yyin = NULL;
symtab = makesymtab(NSYMTAB);
while (argc > 1 && argv[1][0] == '-' && argv[1][1] != '\0') {
if (strcmp(argv[1], "--") == 0) {
argc--;
argv++;
break;
}
switch (argv[1][1]) {
case 's':
if (strcmp(argv[1], "-safe") == 0)
safe = 1;
break;
case 'f':
argc--;
argv++;
if (argc <= 1)
FATAL("no program filename");
pfile[npfile++] = argv[1];
break;
case 'F':
if (argv[1][2] != 0) {
if (argv[1][2] == 't' && argv[1][3] == 0)
fs = "\t";
else if (argv[1][2] != 0)
fs = &argv[1][2];
} else {
argc--; argv++;
if (argc > 1 && argv[1][0] == 't' && argv[1][1] == 0)
fs = "\t";
else if (argc > 1 && argv[1][0] != 0)
fs = &argv[1][0];
}
if (fs == NULL || *fs == '\0')
WARNING("field separator FS is empty");
break;
case 'v':
if (argv[1][2] == '\0' && --argc > 1 && isclvar((++argv)[1]))
setclvar(argv[1]);
break;
case 'm':
marg = argv[1];
if (argv[1][3])
temp = atoi(&argv[1][3]);
else {
argv++; argc--;
temp = atoi(&argv[1][0]);
}
switch (marg[2]) {
case 'r':	recsize = temp; break;
case 'f':	nfields = temp; break;
default: FATAL("unknown option %s\n", marg);
}
break;
case 'd':
dbg = atoi(&argv[1][2]);
if (dbg == 0)
dbg = 1;
printf("awk %s\n", version);
break;
case 'V':
printf("awk %s\n", version);
exit(0);
break;
default:
WARNING("unknown option %s ignored", argv[1]);
break;
}
argc--;
argv++;
}
if (npfile == 0) {
if (argc <= 1) {
if (dbg)
exit(0);
FATAL("no program given");
}
dprintf( ("program = |%s|\n", argv[1]) );
lexprog = argv[1];
argc--;
argv++;
}
recinit(recsize);
syminit();
compile_time = 1;
argv[0] = cmdname;
dprintf( ("argc=%d, argv[0]=%s\n", argc, argv[0]) );
arginit(argc, argv);
if (!safe)
envinit(environ);
yyparse();
if (fs)
*FS = qstring(fs, '\0');
dprintf( ("errorflag=%d\n", errorflag) );
if (errorflag == 0) {
compile_time = 0;
run(winner);
} else
bracecheck();
return(errorflag);
}
int pgetc(void)
{
int c;
for (;;) {
if (yyin == NULL) {
if (curpfile >= npfile)
return EOF;
if (strcmp(pfile[curpfile], "-") == 0)
yyin = stdin;
else if ((yyin = fopen(pfile[curpfile], "r")) == NULL)
FATAL("can't open file %s", pfile[curpfile]);
lineno = 1;
}
if ((c = getc(yyin)) != EOF)
return c;
if (yyin != stdin)
fclose(yyin);
yyin = NULL;
curpfile++;
}
}
char *cursource(void)
{
if (npfile > 0)
return pfile[curpfile];
else
return NULL;
}