#ifndef lint
static char *ident = "$Id: ttyio.c,v 1.2 89/02/12 10:06:11 mark Exp $";
static char *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif
#include "pax.h"
#ifdef __STDC__
int open_tty(void)
#else
int open_tty()
#endif
{
int fd;
SIG_T (*intr)();
if ((intr = signal(SIGINT, SIG_IGN)) == SIG_IGN) {
return (-1);
}
signal(SIGINT, intr);
if ((fd = open(TTY, O_RDWR)) < 0) {
return (-1);
}
if (isatty(fd)) {
return (fd);
}
close(fd);
return (-1);
}
#ifdef __STDC__
int nextask(char *msg, char *answer, int limit)
#else
int nextask(msg, answer, limit)
char *msg;
char *answer;
int limit;
#endif
{
int idx;
int got;
char c;
if (ttyf < 0) {
fatal("/dev/tty Unavailable");
}
write(ttyf, msg, (uint) strlen(msg));
idx = 0;
while ((got = read(ttyf, &c, 1)) == 1) {
if (c == '\n') {
break;
} else if (c == ' ' || c == '\t') {
continue;
} else if (idx < limit - 1) {
answer[idx++] = c;
}
}
if (got == 0) {
return(-1);
}
if (got < 0) {
fatal(strerror());
}
answer[idx] = '\0';
return(0);
}
#ifdef __STDC__
int lineget(FILE *stream, char *buf)
#else
int lineget(stream, buf)
FILE *stream;
char *buf;
#endif
{
int c;
for (;;) {
if ((c = getc(stream)) == EOF) {
return (-1);
}
if (c == '\n') {
break;
}
*buf++ = c;
}
*buf = '\0';
return (0);
}
#ifdef __STDC__
void next(int mode)
#else
void next(mode)
int mode;
#endif
{
char msg[200];
char answer[20];
int ret;
close_archive();
sprintf(msg, "%s: Ready for volume %u\n%s: Type \"go\" when ready to proceed (or \"quit\" to abort): \07",
myname, arvolume + 1, myname);
for (;;) {
ret = nextask(msg, answer, sizeof(answer));
if (ret == -1 || strcmp(answer, "quit") == 0) {
fatal("Aborted");
}
if (strcmp(answer, "go") == 0 && open_archive(mode) == 0) {
break;
}
}
warnarch("Continuing", (OFFSET) 0);
}