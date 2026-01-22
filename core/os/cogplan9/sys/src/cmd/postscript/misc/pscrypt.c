#include <stdio.h>
#include <ctype.h>
#define ENCRYPT		0
#define DECRYPT		1
#define NOTSET		-1
#define BINARY		0
#define HEX		1
#define LINELENGTH	40
#define CHARSTRING	4330
#define EEXEC		55665
#define MAGIC1		52845
#define MAGIC2		22719
int	argc;
char	**argv;
int	mode = DECRYPT;
int	input = NOTSET;
int	output = NOTSET;
int	outoffset = NOTSET;
int	inoffset = NOTSET;
int	cryptkey = 0;
int	linelength = LINELENGTH;
int	lastchar = 0;
unsigned long	seed = EEXEC;
unsigned long	key;
FILE	*fp_in = stdin;
main(agc, agv)
int		agc;
char	*agv[];
{
argc = agc;
argv = agv;
options();
initialize();
arguments();
exit(0);
}
options()
{
int		ch;
char	*names = "bde:l:os:xBSX";
extern char	*optarg;
extern int	optind;
while ( (ch = getopt(argc, argv, names)) != EOF )
switch ( ch ) {
case 'b':
input = BINARY;
break;
case 'd':
mode = DECRYPT;
break;
case 'e':
mode = ENCRYPT;
if ( *optarg == '0' && *optarg == 'x' )
optarg += 2;
sscanf(optarg, "%8x", &cryptkey);
break;
case 'l':
linelength = atoi(optarg);
break;
case 'o':
outoffset = 0;
break;
case 's':
if ( *optarg == 'e' )
seed = EEXEC;
else if ( *optarg == 's' )
seed = CHARSTRING;
else if ( *optarg == '0' && *(optarg+1) == 'x' )
sscanf(optarg+2, "%x", &seed);
else if ( *optarg == '0' )
sscanf(optarg, "%o", &seed);
else sscanf(optarg, "%d", &seed);
break;
case 'x':
input = HEX;
break;
case 'B':
output = BINARY;
break;
case 'X':
output = HEX;
break;
case '?':
fprintf(stderr, "bad option -%c\n", ch);
exit(1);
break;
default:
fprintf(stderr, "missing case for option -%c\n", ch);
exit(1);
break;
}
argc -= optind;
argv += optind;
}
initialize()
{
key = seed;
if ( mode == DECRYPT ) {
input = (input == NOTSET) ? HEX : input;
output = (output == NOTSET) ? BINARY : output;
inoffset = (inoffset == NOTSET) ? 0 : inoffset;
outoffset = (outoffset == NOTSET) ? -4 : outoffset;
} else {
input = (input == NOTSET) ? BINARY : input;
output = (output == NOTSET) ? HEX : output;
inoffset = (inoffset == NOTSET) ? 4 : inoffset;
outoffset = (outoffset == NOTSET) ? 0 : outoffset;
}
if ( linelength <= 0 )
linelength = LINELENGTH;
}
arguments()
{
if ( argc < 1 )
crypt();
else
while ( argc > 0 ) {
if ( strcmp(*argv, "-") == 0 )
fp_in = stdin;
else if ( (fp_in = fopen(*argv, "r")) == NULL ) {
fprintf(stderr, "can't open %s\n", *argv);
exit(1);
}
crypt();
if ( fp_in != stdin )
fclose(fp_in);
argc--;
argv++;
}
}
crypt()
{
unsigned int	cypher;
unsigned int	clear;
while ( lastchar != EOF ) {
cypher = nextbyte();
clear = ((key >> 8) ^ cypher) & 0xFF;
key = (key + (mode == DECRYPT ? cypher : clear)) * MAGIC1 + MAGIC2;
if ( ++outoffset > 0 && lastchar != EOF ) {
if ( output == HEX ) {
printf("%.2X", clear);
if ( linelength > 0 && (outoffset % linelength) == 0 )
putchar('\n');
} else putchar(clear);
}
}
}
nextbyte()
{
int		val = EOF;
if ( inoffset-- > 0 )
val = (cryptkey >> (inoffset*8)) & 0xFF;
else if ( input == HEX ) {
if ( (val = nexthexchar()) != EOF )
val = (val << 4) | nexthexchar();
} else if ( input == BINARY )
val = Getc(fp_in);
return(val);
}
nexthexchar()
{
int		ch;
while ( (ch = Getc(fp_in)) != EOF && ! isxdigit(ch) ) ;
if ( isdigit(ch) )
ch -= '0';
else if ( isupper(ch) )
ch -= 'A' - 10;
else if ( islower(ch) )
ch -= 'a' - 10;
return(ch);
}
Getc(fp)
FILE	*fp;
{
return(lastchar = getc(fp));
}