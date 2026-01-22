#include <stdio.h>
#include <ctype.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/types.h>
#include <errno.h>
#include "ifdef.h"
#include "gen.h"
#include "postio.h"
char	**argv;
int	argc;
char	*prog_name = "";
int	x_stat = 0;
int	debug = OFF;
int	ignore = OFF;
char	*line = NULL;
short	baudrate = BAUDRATE;
Baud	baudtable[] = BAUDTABLE;
int	stopbits = 1;
int	tostdout = FALSE;
int	quiet = FALSE;
int	interactive = FALSE;
char	*postbegin = POSTBEGIN;
int	useslowsend = FALSE;
int	sendctrlC = TRUE;
int	window_size = -1;
char	*block = NULL;
int	blocksize = BLOCKSIZE;
int	head = 0;
int	tail = 0;
int	splitme = FALSE;
int	whatami = READWRITE;
int	canread = TRUE;
int	canwrite = TRUE;
int	otherpid = -1;
int	joinsig = SIGTRAP;
int	writedone = FALSE;
char	mesg[MESGSIZE];
char	sbuf[MESGSIZE];
int	next = 0;
char	*mesgptr = NULL;
char	*endmesg = NULL;
Status	status[] = STATUS;
int	nostatus = NOSTATUS;
int	currentstate = NOTCONNECTED;
int	ttyi = 0;
int	ttyo = 2;
FILE	*fp_log = stderr;
main(agc, agv)
int		agc;
char	*agv[];
{
argc = agc;
argv = agv;
prog_name = argv[0];
init_signals();
options();
initialize();
start();
split();
arguments();
done();
cleanup();
exit(x_stat);
}
init_signals()
{
void	interrupt();
if ( signal(SIGINT, interrupt) == SIG_IGN )  {
signal(SIGINT, SIG_IGN);
signal(SIGQUIT, SIG_IGN);
signal(SIGHUP, SIG_IGN);
} else {
signal(SIGHUP, interrupt);
signal(SIGQUIT, interrupt);
}
signal(SIGTERM, interrupt);
}
options()
{
int		ch;
char	*optnames = "b:cil:qs:tw:B:L:P:R:SDI";
extern char	*optarg;
extern int	optind;
while ( (ch = getopt(argc, argv, optnames)) != EOF )  {
switch ( ch )  {
case 'b':
baudrate = getbaud(optarg);
break;
case 'c':
sendctrlC = FALSE;
break;
case 'i':
interactive = TRUE;
break;
case 'l':
line = optarg;
break;
case 'q':
quiet = TRUE;
break;
case 's':
if ( (stopbits = atoi(optarg)) < 1 || stopbits > 2 )
stopbits = 1;
break;
case 't':
tostdout = TRUE;
break;
case 'w':
window_size = atoi(optarg);
break;
case 'B':
if ( (blocksize = atoi(optarg)) <= 0 )
blocksize = BLOCKSIZE;
break;
case 'L':
if ( (fp_log = fopen(optarg, "w")) == NULL )  {
fp_log = stderr;
error(NON_FATAL, "can't open log file %s", optarg);
}
break;
case 'P':
postbegin = optarg;
break;
case 'R':
if ( atoi(optarg) == 2 )
splitme = TRUE;
else splitme = FALSE;
break;
case 'S':
useslowsend = TRUE;
break;
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
getbaud(rate)
char	*rate;
{
int		i;
for ( i = 0; baudtable[i].rate != NULL; i++ )
if ( strcmp(rate, baudtable[i].rate) == 0 )
return(baudtable[i].val);
error(FATAL, "don't recognize baud rate %s", rate);
}
initialize()
{
whatami = READWRITE;
canread = canwrite = TRUE;
if ( tostdout == TRUE )
splitme = TRUE;
if ( interactive == TRUE )  {
quiet = FALSE;
tostdout = FALSE;
splitme = TRUE;
blocksize = 1;
postbegin = NULL;
useslowsend = FALSE;
nostatus = INTERACTIVE;
setbuf(stdout, NULL);
}
if ( useslowsend == TRUE )  {
quiet = FALSE;
splitme = FALSE;
if ( blocksize > 1024 )
blocksize = 1024;
}
if ( tostdout == TRUE && fp_log == stderr )
fp_log = NULL;
if ( line == NULL && (interactive == TRUE || tostdout == TRUE) )
error(FATAL, "a printer line must be supplied - use the -l option");
if ( (block = malloc(blocksize)) == NULL )
error(FATAL, "no memory");
endmesg = mesg + sizeof mesg - 2;
setupline();
setupstdin(0);
}
start()
{
logit("printer startup\n");
currentstate = START;
clearline();
while ( 1 )
switch ( getstatus(1) )  {
case IDLE:
case INTERACTIVE:
if ( postbegin != NULL && *postbegin != '\0' )
Write(ttyo, postbegin, strlen(postbegin));
clearline();
return;
case BUSY:
if ( sendctrlC == TRUE ) {
Write(ttyo, "\003", 1);
Rest(1);
}
break;
case WAITING:
case ERROR:
case FLUSHING:
Write(ttyo, "\004", 1);
Rest(1);
break;
case PRINTERERROR:
Rest(15);
break;
case DISCONNECT:
error(FATAL, "Disconnected - printer may be offline");
break;
case ENDOFJOB:
case UNKNOWN:
clearline();
break;
default:
Rest(1);
break;
}
}
split()
{
int		pid;
void	interrupt();
if ( splitme == TRUE )
if ( resetline() == TRUE )  {
pid = getpid();
signal(joinsig, interrupt);
if ( (otherpid = fork()) == -1 )
error(FATAL, "can't fork");
else if ( otherpid == 0 )  {
whatami = WRITE;
nostatus = WRITEPROCESS;
otherpid = pid;
setupstdin(1);
} else whatami = READ;
} else if ( interactive == TRUE || tostdout == TRUE )
error(FATAL, "can't create two process - check resetline()");
else error(NON_FATAL, "running as a single process - check resetline()");
canread = (whatami & READ) ? TRUE : FALSE;
canwrite = (whatami & WRITE) ? TRUE : FALSE;
}
arguments()
{
int		fd_in;
if ( canwrite == TRUE )
do
if ( argc < 1 )
send(fileno(stdin), "pipe.end");
else  {
while ( argc > 0 )  {
if ( (fd_in = open(*argv, O_RDONLY)) == -1 )
error(FATAL, "can't open %s", *argv);
send(fd_in, *argv);
close(fd_in);
argc--;
argv++;
}
}
while ( interactive == TRUE );
}
send(fd_in, name)
int		fd_in;
char	*name;
{
if ( interactive == FALSE )
logit("sending file %s\n", name);
currentstate = SEND;
if ( useslowsend == TRUE )  {
slowsend(fd_in);
return;
}
while ( readblock(fd_in) )
switch ( getstatus(0) )  {
case IDLE:
case BUSY:
case WAITING:
case PRINTING:
case ENDOFJOB:
case PRINTERERROR:
case UNKNOWN:
case NOSTATUS:
case WRITEPROCESS:
case INTERACTIVE:
writeblock();
break;
case ERROR:
fprintf(stderr, "%s", mesg);
error(USER_FATAL, "PostScript Error");
break;
case FLUSHING:
error(USER_FATAL, "Flushing Job");
break;
case DISCONNECT:
error(FATAL, "Disconnected - printer may be offline");
break;
}
}
done()
{
int		sleeptime = 15;
if ( canwrite == TRUE )
logit("waiting for end of job\n");
currentstate = DONE;
writedone = (whatami == READWRITE) ? TRUE : FALSE;
while ( 1 )  {
switch ( getstatus(1) )  {
case WRITEPROCESS:
if ( writedone == FALSE )  {
sendsignal(joinsig);
Write(ttyo, "\004", 1);
writedone = TRUE;
sleeptime = 1;
}
Rest(sleeptime++);
break;
case WAITING:
Write(ttyo, "\004", 1);
Rest(1);
sleeptime = 15;
break;
case IDLE:
case ENDOFJOB:
if ( writedone == TRUE )  {
logit("job complete\n");
return;
}
break;
case BUSY:
case PRINTING:
case INTERACTIVE:
sleeptime = 15;
break;
case PRINTERERROR:
Rest(sleeptime++);
break;
case ERROR:
fprintf(stderr, "%s", mesg);
error(USER_FATAL, "PostScript Error");
return;
case FLUSHING:
error(USER_FATAL, "Flushing Job");
return;
case DISCONNECT:
error(FATAL, "Disconnected - printer may be offline");
return;
default:
Rest(1);
break;
}
if ( sleeptime > 60 )
sleeptime = 60;
}
}
cleanup()
{
int		w;
while ( sendsignal(SIGKILL) != -1 && (w = wait((int *)0)) != otherpid && w != -1 ) ;
}
readblock(fd_in)
int		fd_in;
{
static long	blocknum = 1;
if ( head >= tail )  {
if ( (tail = read(fd_in, block, blocksize)) == -1 )
error(FATAL, "error reading input file");
if ( quiet == TRUE && tail > 0 )
logit("%%%%[ status: busy; block: %d ]%%%%\n", blocknum++);
head = 0;
}
return(tail - head);
}
writeblock()
{
int		count;
if ( (count = write(ttyo, &block[head], tail - head)) == -1 )
error(FATAL, "error writing to %s", line);
else if ( count == 0 )
error(FATAL, "printer appears to be offline");
head += count;
return(count);
}
getstatus(t)
int		t;
{
int		gotline = FALSE;
int		state = nostatus;
int		mesgch;
static int	laststate = NOSTATUS;
if ( canread == TRUE && (gotline = readline()) == TRUE )  {
state = parsemesg();
if ( state != laststate || state == UNKNOWN || mesgptr != mesg || debug == ON )
logit("%s", mesg);
if ( tostdout == TRUE && currentstate != START )  {
mesgch = *mesgptr;
*mesgptr = '\0';
fprintf(stdout, "%s", mesg);
fflush(stdout);
*mesgptr = mesgch;
}
return(laststate = state);
}
if ( (quiet == FALSE || currentstate != SEND) &&
(tostdout == FALSE || currentstate == START) && interactive == FALSE )  {
if ( Write(ttyo, "\024", 1) != 1 )
error(FATAL, "printer appears to be offline");
if ( t > 0 ) Rest(t);
}
return(nostatus);
}
parsemesg()
{
char	*e;
char	*key, *val;
char	*p;
int		i;
if ( *(mesgptr = find("%%[ ", mesg)) != '\0' && *(e = find(" ]%%", mesgptr+4)) != '\0' )  {
strcpy(sbuf, mesgptr+4);
sbuf[e-mesgptr-4] = '\0';
for ( key = strtok(sbuf, " :"); key != NULL; key = strtok(NULL, " :") )  {
if ( (val = strtok(NULL, ";")) != NULL && strcmp(key, "status") == 0 )
key = val;
for ( ; *key == ' '; key++ ) ;
for ( p = key; *p; p++ )
if ( *p == ':' )  {
*p = '\0';
break;
} else if ( isupper(*p) ) *p = tolower(*p);
for ( i = 0; status[i].state != NULL; i++ )
if ( strcmp(status[i].state, key) == 0 )
return(status[i].val);
}
} else if ( strcmp(mesg, "CONVERSATION ENDED.\n") == 0 )
return(DISCONNECT);
return(mesgptr == '\0' ? nostatus : UNKNOWN);
}
char *find(str1, str2)
char	*str1;
char	*str2;
{
char	*s1, *s2;
for ( ; *str2 != '\0'; str2++ )  {
for ( s1 = str1, s2 = str2; *s1 != '\0' && *s1 == *s2; s1++, s2++ ) ;
if ( *s1 == '\0' )
break;
}
return(str2);
}
clearline()
{
if ( whatami == READWRITE )
while ( readline() != FALSE ) ;
}
sendsignal(sig)
int		sig;
{
if ( whatami != READWRITE && otherpid > 1 )
return(kill(otherpid, sig));
return(-1);
}
void interrupt(sig)
int		sig;
{
signal(sig, SIG_IGN);
if ( sig != joinsig )  {
x_stat |= FATAL;
if ( canread == TRUE )
if ( interactive == FALSE )
error(NON_FATAL, "signal %d abort", sig);
else error(NON_FATAL, "quitting");
quit(sig);
}
writedone = TRUE;
signal(joinsig, interrupt);
}
logit(mesg, a1, a2, a3)
char	*mesg;
unsigned	a1, a2, a3;
{
if ( mesg != NULL && fp_log != NULL )  {
fprintf(fp_log, mesg, a1, a2, a3);
fflush(fp_log);
}
}
error(kind, mesg, a1, a2, a3)
int		kind;
char	*mesg;
unsigned	a1, a2, a3;
{
FILE	*fp_err;
fp_err = (fp_log != NULL) ? fp_log : stderr;
if ( mesg != NULL && *mesg != '\0' )  {
fprintf(fp_err, "%s: ", prog_name);
fprintf(fp_err, mesg, a1, a2, a3);
putc('\n', fp_err);
}
x_stat |= kind;
if ( kind != NON_FATAL && ignore == OFF )
quit(SIGTERM);
}
quit(sig)
int		sig;
{
int		w;
signal(sig, SIG_IGN);
ignore = ON;
while ( sendsignal(sig) != -1 && (w = wait((int *)0)) != otherpid && w != -1 ) ;
setupstdin(2);
if ( currentstate != NOTCONNECTED ) {
if ( sendctrlC == TRUE ) {
Write(ttyo, "\003", 1);
Rest(1);
}
Write(ttyo, "\004", 1);
}
alarm(0);
Rest(2);
exit(x_stat);
}
Rest(t)
int		t;
{
if ( t > 0 && canwrite == TRUE )
sleep(t);
}
Read(fd, buf, n)
int		fd;
char	*buf;
int		n;
{
int		count;
if ( canread == TRUE )  {
if ( (count = read(fd, buf, n)) == -1 && errno == EINTR )
count = 0;
} else count = 0;
return(count);
}
Write(fd, buf, n)
int		fd;
char	*buf;
int		n;
{
int		count;
if ( canwrite == TRUE )  {
if ( (count = write(fd, buf, n)) == -1 && errno == EINTR )
count = n;
} else count = n;
return(count);
}