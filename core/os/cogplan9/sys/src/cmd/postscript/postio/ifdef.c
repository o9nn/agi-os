#include <stdio.h>
#include <ctype.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/types.h>
#include <errno.h>
#include "ifdef.h"
#include "gen.h"
FILE	*fp_ttyi, *fp_ttyo;
char	*ptr = mesg;
extern int	window_size;
#ifdef SYSV
setupline()
{
struct termio	termio;
#ifdef DKHOST
if ( line != NULL && *line != '/' )  {
if ( strncmp(line, "DK:", 3) == 0 )
line += 3;
dkhost_connect();
#ifdef DKSTREAMS
if ( ioctl(ttyi, I_PUSH, DKSTREAMS) == -1 )
error(FATAL, "ioctl error - %s", DKSTREAMS);
if ( ioctl(ttyi, I_PUSH, "ldterm") == -1 )
error(FATAL, "ioctl error - ldterm");
#endif
} else
#endif
if ( line == NULL )
ttyi = fileno(stdout);
else if ( (ttyi = open(line, O_RDWR)) == -1 )
error(FATAL, "can't open %s", line);
if ( (ttyo = dup(ttyi)) == -1 )
error(FATAL, "can't dup file descriptor for %s", line);
if ( stopbits == 1 )
stopbits = 0;
else stopbits = CSTOPB;
if ( fcntl(ttyi, F_SETFL, O_NDELAY) == -1 )
error(FATAL, "fcntl error - F_SETFL");
if ( ioctl(ttyi, TCGETA, &termio) == -1 )
error(FATAL, "ioctl error - TCGETA");
termio.c_iflag = IXON | IGNCR;
termio.c_oflag = 0;
termio.c_cflag = HUPCL | CREAD | CS8 | stopbits | baudrate;
termio.c_lflag = 0;
termio.c_cc[VMIN] = termio.c_cc[VTIME] = 0;
if ( ioctl(ttyi, TCSETA, &termio) == -1 )
error(FATAL, "ioctl error - TCSETA");
if ( ioctl(ttyi, TCFLSH, 2) == -1 )
error(FATAL, "ioctl error - TCFLSH");
fp_ttyi = fdopen(ttyi, "r");
}
resetline()
{
int			flags;
struct termio	termio;
if ( (flags = fcntl(ttyi, F_GETFL, 0)) == -1 )
error(FATAL, "fcntl error - F_GETFL");
flags &= ~O_NDELAY;
if ( fcntl(ttyi, F_SETFL, flags) == -1 )
error(FATAL, "fcntl error - F_SETFL");
if ( ioctl(ttyi, TCGETA, &termio) == -1 )
error(FATAL, "ioctl error - TCGETA");
termio.c_iflag &= ~IXANY;
termio.c_iflag |= IXON | IXOFF;
termio.c_cc[VMIN] = 1;
termio.c_cc[VTIME] = 0;
if ( ioctl(ttyi, TCSETA, &termio) == -1 )
error(FATAL, "ioctl error - TCSETA");
return(TRUE);
}
setupstdin(mode)
int		mode;
{
struct termio		termio;
static int			saved = FALSE;
static struct termio	oldtermio;
if ( interactive == TRUE )
switch ( mode )  {
case 0:
if ( isatty(0) != 1 )
error(FATAL, "stdin not a terminal - can't run interactive mode");
if ( ioctl(0, TCGETA, &oldtermio) == -1 )
error(FATAL, "can't save terminal settings");
saved = TRUE;
break;
case 1:
termio = oldtermio;
termio.c_lflag &= ~(ICANON | ECHO | ECHOE | ECHOK | ECHONL);
termio.c_cc[VMIN] = 1;
termio.c_cc[VTIME] = 0;
ioctl(0, TCSETA, &termio);
break;
case 2:
if ( saved == TRUE )
ioctl(0, TCSETA, &oldtermio);
break;
}
}
readline()
{
int		n;
int		ch;
static int	tries = 0;
if ( interactive == FALSE )  {
while ( (n = read(ttyi, ptr, 1)) != 0 )  {
if ( n < 0 )
if ( errno == EINTR )
continue;
else error(FATAL, "error reading %s", line);
tries = 0;
if ( *ptr == '\n' || *ptr == '\004' || ptr >= endmesg )  {
*(ptr+1) = '\0';
if ( *ptr == '\004' )
strcpy(ptr, "%%[ status: endofjob ]%%\n");
ptr = mesg;
return(TRUE);
}
ptr++;
}
if ( canread == TRUE && canwrite == FALSE )
if ( ++tries > 100 )
error(FATAL, "printer appears to be offline - shutting down");
return(FALSE);
}
if ( canwrite == TRUE )
return(FALSE);
while ( (ch = getc(fp_ttyi)) != EOF )
putc(ch, stdout);
return(FALSE);
}
#endif
#ifdef V9
#include <ipc.h>
char	tbuf[256];
char	*nptr = tbuf;
char	*eptr = tbuf;
setupline()
{
struct sgttyb	sgtty;
struct ttydevb	ttydev;
static struct tchars	tchar = { '\377',
'\377',
'\021',
'\023',
'\377',
'\377'
};
if ( line == NULL )  {
ttyi = ttyo = 1;
return;
}
alarm(120);
if ( line[0] == '/' ) {
if ( (ttyi = open(line, O_RDWR)) == -1 )
error(FATAL, "can't open %s", line);
} else if ((ttyi = ipcopen(ipcpath(line, "dk", 0), "")) < 0) {
sleep(5);
if ((ttyi = ipcopen(ipcpath(line, "dk", 0), "")) < 0) {
fprintf(stderr, "%s", errstr);
error(FATAL, "can't ipcopen %s", line);
}
}
alarm(0);
if ( (ttyo = dup(ttyi)) == -1 )
error(FATAL, "can't dup file descriptor for %s", line);
if ( ioctl(ttyi, FIOPUSHLD, &tty_ld) == -1 )
error(FATAL, "ioctl error - FIOPUSHLD");
if ( ioctl(ttyi, TIOCGDEV, &ttydev) == -1 )
error(FATAL, "ioctl error - TIOCGDEV");
if ( ioctl(ttyi, TIOCGETP, &sgtty) == -1 )
error(FATAL, "ioctl error - TIOCGETP");
sgtty.sg_flags &= ~ECHO;
sgtty.sg_flags &= ~CRMOD;
sgtty.sg_flags |= CBREAK;
ttydev.ispeed = baudrate;
ttydev.ospeed = baudrate;
if ( ioctl(ttyi, TIOCSDEV, &ttydev) == -1 )
error(FATAL, "ioctl error - TIOCSDEV");
if ( ioctl(ttyi, TIOCSETP, &sgtty) == -1 )
error(FATAL, "ioctl error - TIOCSETP");
if ( ioctl(ttyi, TIOCSETC, &tchar) == -1 )
error(FATAL, "ioctl error - TIOCSETC");
fp_ttyi = fdopen(ttyi, "r");
}
resetline()
{
struct sgttyb	sgtty;
if ( ioctl(ttyi, TIOCGETP, &sgtty) == -1 )
error(FATAL, "ioctl error - TIOCGETP");
sgtty.sg_flags |= TANDEM;
if ( ioctl(ttyi, TIOCSETP, &sgtty) == -1 )
error(FATAL, "ioctl error - TIOCSETP");
return(TRUE);
}
setupstdin(mode)
int		mode;
{
struct sgttyb		sgtty;
static int			saved = FALSE;
static struct sgttyb	oldsgtty;
if ( interactive == TRUE )
switch ( mode )  {
case 0:
if ( ioctl(0, TIOCGETP, &oldsgtty) == -1 )
error(FATAL, "can't save terminal settings");
saved = TRUE;
break;
case 1:
sgtty = oldsgtty;
sgtty.sg_flags &= ~ECHO;
sgtty.sg_flags |= CBREAK;
ioctl(0, TIOCSETP, &sgtty);
break;
case 2:
if ( saved == TRUE )
ioctl(0, TIOCSETP, &oldsgtty);
break;
}
}
readline()
{
int		n;
int		ch;
if ( interactive == FALSE )  {
while ( 1 )  {
while ( nptr < eptr )  {
*ptr = *nptr++;
if ( *ptr == '\r' ) continue;
if ( *ptr == '\n' || *ptr == '\004' || ptr >= endmesg )  {
*(ptr+1) = '\0';
if ( *ptr == '\004' )
strcpy(ptr, "%%[ status: endofjob ]%%\n");
ptr = mesg;
return(TRUE);
}
++ptr;
}
nptr = eptr = tbuf;
if ( ioctl(ttyi, FIONREAD, &n) < 0 )
if ( errno == EINTR )
continue;
else error(FATAL, "ioctl error - FIONREAD");
if ( n <= 0 )
if ( canwrite == TRUE )
return(FALSE);
n = ((n < 1) ? 1 : ((n < sizeof(tbuf)) ? n : sizeof(tbuf)));
if ( (n = read(ttyi, tbuf, n)) < 0 )
if ( errno == EINTR )
continue;
else error(FATAL, "error reading line %s", line);
else eptr = nptr + n;
}
}
if ( canwrite == TRUE )
return(FALSE);
while ( 1 )  {
if ( ioctl(ttyi, FIONREAD, &n) < 0 )
error(FATAL, "ioctl error - FIONREAD");
n = ((n < 1) ? 1 : ((n < sizeof(tbuf)) ? n : sizeof(tbuf)));
if ( (n = read(ttyi, tbuf, n)) < 0 )
error(FATAL, "error reading line %s", line);
else if ( n == 0 )
error(FATAL, "end of file in interactive mode");
if ( write(1, tbuf, n) != n )
error(FATAL, "error writing to stdout");
}
return(FALSE);
}
#endif
#ifdef BSD4_2
setupline()
{
struct sgttyb	sgtty;
static struct tchars	tchar = { '\377',
'\377',
'\021',
'\023',
'\377',
'\377'
};
long	lmodes;
int		disc = NTTYDISC;
if ( line == NULL )
ttyi = fileno(stdout);
else if ( (ttyi = open(line, O_RDWR)) == -1 )
error(FATAL, "can't open %s", line);
if ( (ttyo = dup(ttyi)) == -1 )
error(FATAL, "can't dup file descriptor for %s", line);
if (ioctl(ttyi, TIOCSETD, &disc) == -1 )
error(FATAL, "ioctl error - TIOCSETD");
if ( ioctl(ttyi, TIOCGETP, &sgtty) == -1 )
error(FATAL, "ioctl error - TIOCGETP");
if ( ioctl(ttyi, TIOCLGET, &lmodes) == -1 )
error(FATAL, "ioctl error - TIOCLGET");
sgtty.sg_flags &= ~ECHO;
sgtty.sg_flags &= ~CRMOD;
sgtty.sg_flags |= CBREAK;
sgtty.sg_ispeed = baudrate;
sgtty.sg_ospeed = baudrate;
lmodes |= LDECCTQ;
if ( ioctl(ttyi, TIOCSETP, &sgtty) == -1 )
error(FATAL, "ioctl error - TIOCSETP");
if ( ioctl(ttyi, TIOCSETC, &tchar) == -1 )
error(FATAL, "ioctl error - TIOCSETC");
if ( ioctl(ttyi, TIOCLSET, &lmodes) == -1 )
error(FATAL, "ioctl error - TIOCLSET");
fp_ttyi = fdopen(ttyi, "r");
}
resetline()
{
struct sgttyb	sgtty;
if ( ioctl(ttyi, TIOCGETP, &sgtty) == -1 )
error(FATAL, "ioctl error - TIOCGETP");
sgtty.sg_flags |= TANDEM;
if ( ioctl(ttyi, TIOCSETP, &sgtty) == -1 )
error(FATAL, "ioctl error - TIOCSETP");
return(TRUE);
}
setupstdin(mode)
int		mode;
{
struct sgttyb		sgtty;
static int			saved = FALSE;
static struct sgttyb	oldsgtty;
if ( interactive == TRUE )
switch ( mode )  {
case 0:
if ( isatty(0) != 1 )
error(FATAL, "stdin not a terminal - can't run interactive mode");
if ( ioctl(0, TIOCGETP, &oldsgtty) == -1 )
error(FATAL, "can't save terminal settings");
saved = TRUE;
break;
case 1:
sgtty = oldsgtty;
sgtty.sg_flags &= ~ECHO;
sgtty.sg_flags |= CBREAK;
ioctl(0, TIOCSETP, &sgtty);
break;
case 2:
if ( saved == TRUE )
ioctl(0, TIOCSETP, &oldsgtty);
break;
}
}
readline()
{
int		n;
int		ch;
if ( interactive == FALSE )  {
while ( 1 )  {
if ( ioctl(ttyi, FIONREAD, &n) < 0 )
if ( errno == EINTR )
continue;
else error(FATAL, "ioctl error - FIONREAD");
if ( n <= 0 )
if ( canwrite == TRUE )
return(FALSE);
else n = 1;
for ( ; n > 0; n-- )  {
if ( (*ptr = getc(fp_ttyi)) == EOF )
if ( errno == EINTR )
continue;
else error(FATAL, "error reading %s", line);
if ( *ptr == '\r' ) continue;
if ( *ptr == '\n' || *ptr == '\004' || ptr >= endmesg )  {
*(ptr+1) = '\0';
if ( *ptr == '\004' )
strcpy(ptr, "%%[ status: endofjob ]%%\n");
ptr = mesg;
return(TRUE);
}
++ptr;
}
}
}
if ( canwrite == TRUE )
return(FALSE);
while ( (ch = getc(fp_ttyi)) != EOF )
putc(ch, stdout);
return(FALSE);
}
int
strspn(string, charset)
char	*string;
register char	*charset;
{
register char *p, *q;
for(q=string; *q != '\0'; ++q) {
for(p=charset; *p != '\0' && *p != *q; ++p)
;
if(*p == '\0')
break;
}
return(q-string);
}
char *
strpbrk(string, brkset)
register char *string, *brkset;
{
register char *p;
do {
for(p=brkset; *p != '\0' && *p != *string; ++p)
;
if(*p != '\0')
return(string);
}
while(*string++);
return((char*)0);
}
extern int strspn();
extern char *strpbrk();
char *
strtok(string, sepset)
char	*string, *sepset;
{
register char	*p, *q, *r;
static char	*savept;
p = (string == (char*)0)? savept: string;
if(p == 0)
return((char*)0);
q = p + strspn(p, sepset);
if(*q == '\0')
return((char*)0);
if((r = strpbrk(q, sepset)) == (char*)0)
savept = 0;
else {
*r = '\0';
savept = ++r;
}
return(q);
}
#endif
#ifdef DKHOST
#ifndef DKSTREAMS
short	dkrmode[3] = {DKR_TIME, 0, 0};
#endif
dkhost_connect()
{
int		ofd;
int		dfd;
int		retrytime = 5;
if ( *line == '\0' )
error(FATAL, "incomplete Datakit line");
if ( fp_log != NULL && fp_log != stderr )  {
ofd = dup(2);
close(2);
dup(fileno(fp_log));
}
while ( (dfd = ttyi = dkdial(line)) < 0 )  {
if ( retrytime < 0 )
error(FATAL, "can't connect to %s", line);
sleep(retrytime++);
if ( retrytime > 60 )
retrytime = 60;
}
if ( fp_log != NULL && fp_log != stderr )  {
close(2);
dup(ofd);
close(ofd);
}
#ifndef DKSTREAMS
if ( ioctl(ttyi, DIOCRMODE, dkrmode) == -1 )
error(FATAL, "ioctl error - DIOCRMODE");
#ifdef DIOURPWD
if ( window_size > 0 ) {
short	dkparm[3];
dkparm[0] = dkminor(ttyi);
dkparm[1] = 1;
dkparm[2] = window_size;
if ( ioctl(ttyi, DIOURPWD, dkparm) < 0 || ioctl(ttyi, DIOCFLUSH, 0) < 0 )
error(NON_FATAL, "WSA failed");
}
#endif
line = dtnamer(dkminor(ttyi));
if ( (ttyi = open(line, O_RDWR)) == -1 )
error(FATAL, "can't open %s", line);
close(dfd);
#endif
}
#endif