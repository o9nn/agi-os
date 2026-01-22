#ifndef lint
static char *ident = "$Id: buffer.c,v 1.2 89/02/12 10:04:02 mark Exp $";
static char *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif
#include "pax.h"
#ifdef __STDC__
static int ar_write(int, char *, uint);
static void buf_pad(OFFSET);
static int indata(int, OFFSET, char *);
static void outflush(void);
static void buf_use(uint);
static int buf_in_avail(char **, uint *);
static uint buf_out_avail(char **);
#else
static int ar_write();
static void buf_pad();
static int indata();
static void outflush();
static void buf_use();
static int buf_in_avail();
static uint buf_out_avail();
#endif
#ifdef __STDC__
int inentry(char *name, Stat *asb)
#else
int inentry(name, asb)
char *name;
Stat *asb;
#endif
{
Link *linkp;
int ifd;
int ofd;
time_t tstamp[2];
if ((ofd = openout(name, asb, linkp = linkfrom(name, asb), 0)) > 0) {
if (asb->sb_size || linkp == (Link *)NULL || linkp->l_size == 0) {
close(indata(ofd, asb->sb_size, name));
} else if ((ifd = open(linkp->l_path->p_name, O_RDONLY)) < 0) {
warn(linkp->l_path->p_name, strerror());
} else {
passdata(linkp->l_path->p_name, ifd, name, ofd);
close(ifd);
close(ofd);
}
} else {
return(buf_skip((OFFSET) asb->sb_size) >= 0);
}
tstamp[0] = (!f_pass && f_access_time) ? asb->sb_atime : time((time_t *) 0);
tstamp[1] = f_mtime ? asb->sb_mtime : time((time_t *) 0);
utime(name, tstamp);
return (0);
}
#ifdef __STDC__
void outdata(int fd, char *name, OFFSET size)
#else
void outdata(fd, name, size)
int fd;
char *name;
OFFSET size;
#endif
{
uint chunk;
int got;
int oops;
uint avail;
int pad;
char *buf;
oops = got = 0;
if (pad = (size % BLOCKSIZE)) {
pad = (BLOCKSIZE - pad);
}
while (size) {
avail = buf_out_avail(&buf);
size -= (chunk = size < avail ? (uint) size : avail);
if (oops == 0 && (got = read(fd, buf, (unsigned int) chunk)) < 0) {
oops = -1;
warn(name, strerror());
got = 0;
}
if (got < chunk) {
if (oops == 0) {
oops = -1;
}
warn(name, "Early EOF");
while (got < chunk) {
buf[got++] = '\0';
}
}
buf_use(chunk);
}
close(fd);
if (ar_format == TAR) {
buf_pad((OFFSET) pad);
}
}
#ifdef __STDC__
void write_eot(void)
#else
void write_eot()
#endif
{
OFFSET pad;
char header[M_STRLEN + H_STRLEN + 1];
if (ar_format == TAR) {
pad = 2 * BLOCKSIZE;
} else {
if (pad = (total + M_STRLEN + H_STRLEN + TRAILZ) % BLOCKSIZE) {
pad = BLOCKSIZE - pad;
}
strcpy(header, M_ASCII);
sprintf(header + M_STRLEN, H_PRINT, 0, 0,
0, 0, 0, 1, 0, (time_t) 0, TRAILZ, pad);
outwrite(header, M_STRLEN + H_STRLEN);
outwrite(TRAILER, TRAILZ);
}
buf_pad((OFFSET) pad);
outflush();
}
#ifdef __STDC__
void outwrite(char *idx, uint len)
#else
void outwrite(idx, len)
char *idx;
uint len;
#endif
{
uint have;
uint want;
char *endx;
endx = idx + len;
while (want = endx - idx) {
if (bufend - bufidx < 0) {
fatal("Buffer overlow in out_write\n");
}
if ((have = bufend - bufidx) == 0) {
outflush();
}
if (have > want) {
have = want;
}
memcpy(bufidx, idx, (int) have);
bufidx += have;
idx += have;
total += have;
}
}
#ifdef __STDC__
void passdata(char *from, int ifd, char *to, int ofd)
#else
void passdata(from, ifd, to, ofd)
char *from;
int ifd;
char *to;
int ofd;
#endif
{
int got;
int sparse;
char block[BUFSIZ];
if (ifd) {
lseek(ifd, (OFFSET) 0, 0);
sparse = 0;
while ((got = read(ifd, block, sizeof(block))) > 0
&& (sparse = ar_write(ofd, block, (uint) got)) >= 0) {
total += got;
}
if (got) {
warn(got < 0 ? from : to, strerror());
} else if (sparse > 0
&& (lseek(ofd, (OFFSET)(-sparse), 1) < 0
|| write(ofd, block, (uint) sparse) != sparse)) {
warn(to, strerror());
}
}
close(ofd);
}
#ifdef __STDC__
void buf_allocate(OFFSET size)
#else
void buf_allocate(size)
OFFSET size;
#endif
{
if (size <= 0) {
fatal("invalid value for blocksize");
}
if ((bufstart = malloc((unsigned) size)) == (char *)NULL) {
fatal("Cannot allocate I/O buffer");
}
bufend = bufidx = bufstart;
bufend += size;
}
#ifdef __STDC__
int buf_skip(OFFSET len)
#else
int buf_skip(len)
OFFSET len;
#endif
{
uint chunk;
int corrupt = 0;
while (len) {
if (bufend - bufidx < 0) {
fatal("Buffer overlow in buf_skip\n");
}
while ((chunk = bufend - bufidx) == 0) {
corrupt |= ar_read();
}
if (chunk > len) {
chunk = len;
}
bufidx += chunk;
len -= chunk;
total += chunk;
}
return (corrupt);
}
#ifdef __STDC__
int buf_read(char *dst, uint len)
#else
int buf_read(dst, len)
char *dst;
uint len;
#endif
{
int have;
int want;
int corrupt = 0;
char *endx = dst + len;
while (want = endx - dst) {
if (bufend - bufidx < 0) {
fatal("Buffer overlow in buf_read\n");
}
while ((have = bufend - bufidx) == 0) {
have = 0;
corrupt |= ar_read();
}
if (have > want) {
have = want;
}
memcpy(dst, bufidx, have);
bufidx += have;
dst += have;
total += have;
}
return (corrupt);
}
#ifdef __STDC__
static int indata(int fd, OFFSET size, char *name)
#else
static int indata(fd, size, name)
int fd;
OFFSET size;
char *name;
#endif
{
uint chunk;
char *oops;
int sparse;
int corrupt;
char *buf;
uint avail;
corrupt = sparse = 0;
oops = (char *)NULL;
while (size) {
corrupt |= buf_in_avail(&buf, &avail);
size -= (chunk = size < avail ? (uint) size : avail);
if (oops == (char *)NULL && (sparse = ar_write(fd, buf, chunk)) < 0) {
oops = strerror();
}
buf_use(chunk);
}
if (corrupt) {
warn(name, "Corrupt archive data");
}
if (oops) {
warn(name, oops);
} else if (sparse > 0 && (lseek(fd, (OFFSET) - 1, 1) < 0
|| write(fd, "", 1) != 1)) {
warn(name, strerror());
}
return (fd);
}
#ifdef __STDC__
static void outflush(void)
#else
static void outflush()
#endif
{
char *buf;
int got;
uint len;
for (buf = bufstart; len = bufidx - buf;) {
if ((got = write(archivefd, buf, MIN(len, blocksize))) > 0) {
buf += got;
} else if (got < 0) {
next(AR_WRITE);
}
}
bufend = (bufidx = bufstart) + blocksize;
}
#ifdef __STDC__
int ar_read(void)
#else
int ar_read()
#endif
{
int got;
static int failed;
bufend = bufidx = bufstart;
if (!failed) {
if (areof) {
if (total == 0) {
fatal("No input");
} else {
next(AR_READ);
}
}
while (!failed && !areof && bufstart + blocksize - bufend >= blocksize) {
if ((got = read(archivefd, bufend, (unsigned int) blocksize)) > 0) {
bufend += got;
} else if (got < 0) {
failed = -1;
warnarch(strerror(), (OFFSET) 0 - (bufend - bufidx));
} else {
++areof;
}
}
}
if (failed && bufend == bufstart) {
failed = 0;
for (got = 0; got < blocksize; ++got) {
*bufend++ = '\0';
}
return (-1);
}
return (0);
}
#ifdef __STDC__
static int ar_write(int fd, char *buf, uint len)
#else
static int ar_write(fd, buf, len)
int fd;
char *buf;
uint len;
#endif
{
char *bidx;
char *bend;
bend = (bidx = buf) + len;
while (bidx < bend) {
if (*bidx++) {
return (write(fd, buf, len) == len ? 0 : -1);
}
}
return (lseek(fd, (OFFSET) len, 1) < 0 ? -1 : len);
}
#ifdef __STDC__
static void buf_pad(OFFSET pad)
#else
static void buf_pad(pad)
OFFSET pad;
#endif
{
int idx;
int have;
while (pad) {
if ((have = bufend - bufidx) > pad) {
have = pad;
}
for (idx = 0; idx < have; ++idx) {
*bufidx++ = '\0';
}
total += have;
pad -= have;
if (bufend - bufidx == 0) {
outflush();
}
}
}
#ifdef __STDC__
static void buf_use(uint len)
#else
static void buf_use(len)
uint len;
#endif
{
bufidx += len;
total += len;
}
#ifdef __STDC__
static int buf_in_avail(char **bufp, uint *lenp)
#else
static int buf_in_avail(bufp, lenp)
char **bufp;
uint *lenp;
#endif
{
uint have;
int corrupt = 0;
while ((have = bufend - bufidx) == 0) {
corrupt |= ar_read();
}
*bufp = bufidx;
*lenp = have;
return (corrupt);
}
#ifdef __STDC__
static uint buf_out_avail(char **bufp)
#else
static uint buf_out_avail(bufp)
char **bufp;
#endif
{
int have;
if (bufend - bufidx < 0) {
fatal("Buffer overlow in buf_out_avail\n");
}
if ((have = bufend - bufidx) == 0) {
outflush();
}
*bufp = bufidx;
return (have);
}