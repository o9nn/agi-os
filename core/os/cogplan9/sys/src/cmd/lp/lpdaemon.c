#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdarg.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#ifdef PLAN9
#define LP	"/bin/lp"
#define TMPDIR "/sys/lib/lp/tmp"
#define LPDAEMONLOG	"/sys/lib/lp/log/lpdaemonl"
#endif
#ifdef V10
#define LP	"/usr/bin/lp"
#define TMPDIR "/tmp"
#define LPDAEMONLOG	"/tmp/lpdaemonl"
#endif
#if defined(SYSV) || defined(BSD)
#define LP	"/v/bin/lp"
#define TMPDIR "/tmp"
#define LPDAEMONLOG	"/tmp/lpdaemonl"
#endif
#define ARGSIZ 4096
#define NAMELEN 30
unsigned char argvstr[ARGSIZ];
unsigned char *argvals[ARGSIZ/2+1];
int ascnt = 0, argcnt = 0;
struct jobinfo {
char user[NAMELEN+1];
char host[NAMELEN+1];
} *getjobinfo();
#define MIN(a,b)	((a<b)?a:b)
#define	CPYFIELD(src, dst)	{ while (*(src)!=' ' && *(src)!='\t' && *(src)!='\r' && *(src)!='\n' && *(src)!='\0') *(dst)++ = *(src)++; }
#define	ACK()	write(1, "", 1)
#define NAK()	write(1, "\001", 1)
#define LNBFSZ	4096
unsigned char lnbuf[LNBFSZ];
#define	RDSIZE 512
unsigned char jobbuf[RDSIZE];
int datafd[400], cntrlfd = -1;
int dbgstate = 0;
char *dbgstrings[] = {
"",
"sendack1",
"send",
"rcvack",
"sendack2",
"done"
};
void
error(char *s1, ...)
{
FILE *fp;
long thetime;
char *chartime;
va_list ap;
char *args[8];
int argno = 0;
if((fp=fopen(LPDAEMONLOG, "a"))==NULL) {
fprintf(stderr, "cannot open %s in append mode\n", LPDAEMONLOG);
return;
}
time(&thetime);
chartime = ctime(&thetime);
fprintf(fp, "%.15s [%5.5d] ", &(chartime[4]), getpid());
va_start(ap, s1);
while((args[argno++] = va_arg(ap, char*)) && argno<8)
;
va_end(ap);
fprintf(fp, s1, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);
fclose(fp);
}
void
forklp(int inputfd)
{
int i, cpid;
unsigned char *bp, *cp;
unsigned char logent[LNBFSZ];
cp = logent;
for (i=1; i<argcnt; i++) {
bp = argvals[i];
if (cp+strlen((const char *)bp)+1 < logent+LNBFSZ-1) {
CPYFIELD(bp, cp);
*cp++ = ' ';
}
}
*--cp = '\n';
*++cp = '\0';
error((const char *)logent);
switch((cpid=fork())){
case -1:
error("fork error\n");
exit(2);
case 0:
if (inputfd != 0)
dup2(inputfd, 0);
dup2(1, 2);
lseek(0, 0L, 0);
execvp(LP, (const char **)argvals);
error("exec failed\n");
exit(3);
default:
while(wait((int *)0) != cpid)
;
}
}
int
tempfile(void)
{
static tindx = 0;
char tmpf[sizeof(TMPDIR)+64];
int crtfd, tmpfd;
sprintf(tmpf, "%s/lp%d.%d", TMPDIR, getpid(), tindx++);
if((crtfd=creat(tmpf, 0666)) < 0) {
error("cannot create temp file %s\n", tmpf);
NAK();
exit(3);
}
if((tmpfd=open(tmpf, 2)) < 0) {
error("cannot open temp file %s\n", tmpf);
NAK();
exit(3);
}
close(crtfd);
unlink(tmpf);
return(tmpfd);
}
int
readfile(int outfd, int bsize)
{
int rv;
dbgstate = 1;
alarm(60);
ACK();
dbgstate = 2;
for(; bsize > 0; bsize -= rv) {
alarm(60);
if((rv=read(0, jobbuf, MIN(bsize,RDSIZE))) < 0) {
error("error reading input, %d unread\n", bsize);
exit(4);
} else if (rv == 0) {
error("connection closed prematurely\n");
exit(4);
} else if((write(outfd, jobbuf, rv)) != rv) {
error("error writing temp file, %d unread\n", bsize);
exit(5);
}
}
dbgstate = 3;
alarm(60);
if (((rv=read(0, jobbuf, 1))==1) && (*jobbuf=='\0')) {
alarm(60);
ACK();
dbgstate = 4;
alarm(0);
return(outfd);
}
alarm(0);
error("received bad status <%d> from sender\n", *jobbuf);
error("rv=%d\n", rv);
NAK();
return(-1);
}
int
readline(int inpfd)
{
unsigned char *ap;
int i, rv;
ap = lnbuf;
lnbuf[0] = '\0';
i = 0;
alarm(60);
do {
rv = read(inpfd, ap, 1);
} while (rv==1 && ++i && *ap != '\n' && ap++ && (i < LNBFSZ - 2));
alarm(0);
if (i != 0 && *ap != '\n') {
*++ap = '\n';
i++;
}
*++ap = '\0';
if (rv < 0) {
error("read error; lost connection\n");
if (i==0) i = -(LNBFSZ+1);
else i = -i;
}
return(i);
}
int
getfiles(void)
{
unsigned char *ap;
int filecnt, bsize, rv;
filecnt = 0;
for(;;) {
ap = lnbuf;
if ((rv=readline(0)) < 0) NAK();
if (rv <= 0) {
return(filecnt);
}
switch(*ap++) {
case '\1':
break;
case '\2':
bsize = atoi((const char *)ap);
cntrlfd = tempfile();
if (readfile(cntrlfd, bsize) < 0) {
close(cntrlfd);
NAK();
return(0);
}
break;
case '\3':
bsize = atoi((const char *)ap);
datafd[filecnt] = tempfile();
if (readfile(datafd[filecnt], bsize) < 0) {
close(datafd[filecnt]);
NAK();
return(0);
}
filecnt++;
break;
default:
error("protocol error <%d>\n", *(ap-1));
NAK();
}
}
return(filecnt);
}
struct jobinfo *
getjobinfo(int fd)
{
unsigned char *ap;
int rv;
static struct jobinfo info;
if (fd < 0) error("getjobinfo: bad file descriptor\n");
if (lseek(fd, 0L, 0) < 0) {
error("error seeking in temp file\n");
exit(7);
}
strncpy(info.user, "daemon", NAMELEN);
strncpy(info.host, "nowhere", NAMELEN);
while ((rv=readline(fd)) > 0) {
ap = lnbuf;
ap[rv-1] = '\0';
switch (*ap) {
case 'H':
if (ap[1] == '\0')
strncpy(info.host, "unknown", NAMELEN);
else
strncpy(info.host, (const char *)&ap[1], NAMELEN);
info.host[NAMELEN] = '\0';
break;
case 'P':
if (ap[1] == '\0')
strncpy(info.user, "unknown", NAMELEN);
else
strncpy(info.user, (const char *)&ap[1], NAMELEN);
info.user[NAMELEN] = '\0';
break;
}
}
return(&info);
}
void
alarmhandler(int sig) {
signal(sig, alarmhandler);
error("alarm at %d - %s\n", dbgstate, dbgstrings[dbgstate]);
}
void
main()
{
unsigned char *ap, *bp, *cp, *savbufpnt;
int i, blen, rv, saveflg, savargcnt;
struct jobinfo *jinfop;
signal(SIGHUP, SIG_IGN);
signal(SIGALRM, alarmhandler);
cp = argvstr;
argvals[argcnt++] = cp;
for (bp = (unsigned char *)LP, i = 0; (*bp != '\0') && (i < ARGSIZ-1); *cp++ = *bp++, i++);
*cp++ = '\0';
if ((rv=readline(0)) < 0)
exit(1);
bp = lnbuf;
switch (*bp) {
case '\001':
case '\003':
case '\004':
bp++;
argvals[argcnt++] = cp;
*cp++ = '-'; *cp++ = 'q'; *cp++ = '\0';
argvals[argcnt++] = cp;
*cp++ = '-'; *cp++ = 'd';
CPYFIELD(bp, cp);
*cp++ = '\0';
break;
case '\002':
bp++;
argvals[argcnt++] = cp;
*cp++ = '-'; *cp++ = 'd';
CPYFIELD(bp, cp);
*cp++ = '\0';
ACK();
savargcnt = argcnt;
savbufpnt = cp;
while ((rv=getfiles())) {
jinfop = getjobinfo(cntrlfd);
close(cntrlfd);
argcnt = savargcnt;
cp = savbufpnt;
argvals[argcnt++] = cp;
*cp++ = '-'; *cp++ = 'M';
bp = (unsigned char *)jinfop->host;
CPYFIELD(bp, cp);
*cp++ = '\0';
argvals[argcnt++] = cp;
*cp++ = '-'; *cp++ = 'u';
bp = (unsigned char *)jinfop->user;
CPYFIELD(bp, cp);
*cp++ = '\0';
for(i=0;i<rv;i++)
forklp(datafd[i]);
}
exit(0);
case '\005':
bp++;
argvals[argcnt++] = cp;
*cp++ = '-'; *cp++ = 'k'; *cp++ = '\0';
argvals[argcnt++] = cp;
*cp++ = '-'; *cp++ = 'd';
CPYFIELD(bp, cp);
*cp++ = '\0';
argvals[argcnt++] = cp;
*cp++ = '-'; ap = cp; *cp++ = 'u';
CPYFIELD(bp, cp);
if (ap == (cp-1)) {
ap = (unsigned char *)"none";
CPYFIELD(ap, cp);
}
*cp++ = '\0';
datafd[0] = tempfile();
blen = strlen((const char *)bp);
if (write(datafd[0], bp, blen) != blen) {
error("write error\n");
exit(6);
}
if (write(datafd[0], "\n", 1) != 1) {
error("write error\n");
exit(6);
}
break;
default:
do {
while (*bp==' '||*bp=='\t')
++bp;
if (*bp=='\n') continue;
if (*bp=='-') {
argvals[argcnt++] = cp;
saveflg = 1;
} else
saveflg = 0;
while (*bp!=' ' && *bp!='\t' && *bp!='\n'
&& *bp!='\0') {
*cp = *bp++;
cp += saveflg;
}
*cp = '\0';
cp += saveflg;
} while (*bp!='\n' && *bp!='\0');
if (readline(0) < 0) exit(7);
datafd[0] = tempfile();
if(readfile(datafd[0], atoi((const char *)lnbuf)) < 0) {
error("readfile failed\n");
exit(8);
}
}
forklp(datafd[0]);
exit(0);
}