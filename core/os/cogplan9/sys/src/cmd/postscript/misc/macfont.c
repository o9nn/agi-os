#include <stdio.h>
#include <signal.h>
#define OFF 0
#define ON 1
#define NON_FATAL 0
#define FATAL 1
#define FALSE 0
#define TRUE 1
char **argv;
int argc;
char *prog_name;
int x_stat;
int debug = OFF;
int ignore = OFF;
FILE *fp_in = stdin;
FILE *fp_out = stdout;
main(agc, agv)
int agc;
char *agv[];
{
argc = agc;
argv = agv;
prog_name = argv[0];
options();
arguments();
exit(x_stat);
}
options()
{
int ch;
char *names = "DI";
extern char *optarg;
extern int optind;
while ( (ch = getopt(argc, argv, names)) != EOF ) {
switch ( ch ) {
case 'D':
debug = ON;
break;
case 'I':
ignore = ON;
break;
case '?':
error(FATAL, "");
break;
default:
error(FATAL, "missing case for option %c\n", ch);
break;
}
}
argc -= optind;
argv += optind;
}
arguments()
{
if ( argc < 1 )
conv();
else
while ( argc > 0 ) {
if ( strcmp(*argv, "-") == 0 )
fp_in = stdin;
else if ( (fp_in = fopen(*argv, "r")) == NULL )
error(FATAL, "can't open %s", *argv);
conv();
if ( fp_in != stdin )
fclose(fp_in);
argc--;
argv++;
}
}
conv()
{
int blocksize;
int blocktype;
while ( 1 ) {
blocksize = getint(fp_in);
blocktype = getc(fp_in);
getc(fp_in);
if ( debug == ON )
fprintf(stderr, "blocktype = %d, blocksize = %d\n", blocktype, blocksize);
switch ( blocktype ) {
case 0:
fseek(fp_in, (long) blocksize - 6, 1);
break;
case 1:
asciitext(blocksize - 2);
break;
case 2:
hexdata(blocksize - 2);
break;
case 3:
case 4:
error(FATAL, "resource type %d not implemented", blocktype);
break;
case 5:
return;
default:
error(FATAL, "unknown resource type %d", blocktype);
}
}
}
asciitext(count)
int count;
{
int ch;
int i = 0;
for ( i = 0; i < count; i++ ) {
if ( (ch = getc(fp_in)) == '\r' )
ch = '\n';
putc(ch, fp_out);
}
}
hexdata(count)
int count;
{
int i;
int n;
for ( i = 0, n = 0; i < count; i++ ) {
fprintf(fp_out, "%.2X", getc(fp_in));
if ( (++n % 40) == 0 )
putc('\n', fp_out);
}
}
getint()
{
int val;
int i;
for ( i = 0, val = (getc(fp_in) & 0377); i < 3; i++ )
val = (val << 8) | (getc(fp_in) & 0377);
return(val);
}
error(kind, mesg, a1, a2, a3)
int kind;
char *mesg;
unsigned a1, a2, a3;
{
if ( mesg != NULL && *mesg != '\0' ) {
fprintf(stderr, "%s: ", prog_name);
fprintf(stderr, mesg, a1, a2, a3);
putc('\n', stderr);
}
if ( kind == FATAL && ignore == OFF )
exit(x_stat | 01);
}