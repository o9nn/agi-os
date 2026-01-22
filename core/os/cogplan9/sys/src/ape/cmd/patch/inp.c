#define XTERN extern
#include <common.h>
#include <backupfile.h>
#include <pch.h>
#include <util.h>
#undef XTERN
#define XTERN
#include <inp.h>
static char *i_buffer;
static char const **i_ptr;
static size_t tibufsize;
#ifndef TIBUFSIZE_MINIMUM
#define TIBUFSIZE_MINIMUM (8 * 1024)
#endif
static int tifd = -1;
static char *tibuf[2];
static LINENUM tiline[2] = {-1, -1};
static LINENUM lines_per_buf;
static size_t tireclen;
static size_t last_line_size;
static bool plan_a PARAMS ((char const *));
static void plan_b PARAMS ((char const *));
static void report_revision PARAMS ((int));
static void too_many_lines PARAMS ((char const *)) __attribute__((noreturn));
void
re_input()
{
if (using_plan_a) {
free (i_buffer);
free (i_ptr);
}
else {
close (tifd);
tifd = -1;
free(tibuf[0]);
tibuf[0] = 0;
tiline[0] = tiline[1] = -1;
tireclen = 0;
}
}
void
scan_input(filename)
char *filename;
{
using_plan_a = ! (debug & 16) && plan_a (filename);
if (!using_plan_a)
plan_b(filename);
switch (verbosity)
{
case SILENT:
break;
case VERBOSE:
say ("Patching file `%s' using Plan %s...\n",
filename, using_plan_a ? "A" : "B");
break;
case DEFAULT_VERBOSITY:
say ("patching file `%s'\n", filename);
break;
}
}
static void
report_revision (found_revision)
int found_revision;
{
if (found_revision)
{
if (verbosity == VERBOSE)
say ("Good.  This file appears to be the %s version.\n", revision);
}
else if (force)
{
if (verbosity != SILENT)
say ("Warning: this file doesn't appear to be the %s version -- patching anyway.\n",
revision);
}
else if (batch)
{
fatal ("This file doesn't appear to be the %s version -- aborting.",
revision);
}
else
{
ask ("This file doesn't appear to be the %s version -- patch anyway? [n] ",
revision);
if (*buf != 'y')
fatal ("aborted");
}
}
static void
too_many_lines (filename)
char const *filename;
{
fatal ("File `%s' has too many lines.", filename);
}
void
get_input_file (filename, outname)
char const *filename;
char const *outname;
{
int elsewhere = strcmp (filename, outname);
char const *cs;
char *diffbuf;
char *getbuf;
if (inerrno == -1)
inerrno = stat (inname, &instat) == 0 ? 0 : errno;
if (patch_get
&& invc != 0
&& (inerrno
|| (! elsewhere
&& (
(instat.st_mode & (S_IWUSR|S_IWGRP|S_IWOTH)) == 0
|| ((instat.st_mode & (S_IWGRP|S_IWOTH)) == 0
&& instat.st_uid != geteuid ()))))
&& (invc = !! (cs = (version_controller
(filename, elsewhere,
inerrno ? (struct stat *) 0 : &instat,
&getbuf, &diffbuf))))) {
if (!inerrno) {
if (!elsewhere
&& (instat.st_mode & (S_IWUSR|S_IWGRP|S_IWOTH)) != 0)
fatal ("file `%s' seems to be locked by somebody else under %s",
filename, cs);
if (verbosity == VERBOSE)
say ("Comparing file `%s' to default %s version...\n",
filename, cs);
if (systemic (diffbuf) != 0)
{
say ("warning: patching file `%s', which does not match default %s version\n",
filename, cs);
cs = 0;
}
}
if (cs && version_get (filename, cs, ! inerrno, elsewhere, getbuf,
&instat))
inerrno = 0;
free (getbuf);
free (diffbuf);
} else if (inerrno && !pch_says_nonexistent (reverse))
{
errno = inerrno;
pfatal ("can't find file `%s'", filename);
}
if (inerrno)
{
instat.st_mode = S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH;
instat.st_size = 0;
}
else if (! S_ISREG (instat.st_mode))
fatal ("`%s' is not a regular file -- can't patch", filename);
}
static bool
plan_a(filename)
char const *filename;
{
register char const *s;
register char const *lim;
register char const **ptr;
register char *buffer;
register LINENUM iline;
size_t size = instat.st_size;
if (! (size == instat.st_size
&& (buffer = malloc (size ? size : (size_t) 1))))
return FALSE;
if (size)
{
int ifd = open (filename, O_RDONLY|binary_transput);
size_t buffered = 0, n;
if (ifd < 0)
pfatal ("can't open file `%s'", filename);
while (size - buffered != 0)
{
n = read (ifd, buffer + buffered, size - buffered);
if (n == 0)
{
size = buffered;
break;
}
if (n == (size_t) -1)
{
close (ifd);
free (buffer);
return FALSE;
}
buffered += n;
}
if (close (ifd) != 0)
read_fatal ();
}
lim = buffer + size;
iline = 3;
for (s = buffer; (s = (char *) memchr (s, '\n', lim - s)); s++)
if (++iline < 0)
too_many_lines (filename);
if (! (iline == (size_t) iline
&& (size_t) iline * sizeof *ptr / sizeof *ptr == (size_t) iline
&& (ptr = (char const **) malloc ((size_t) iline * sizeof *ptr))))
{
free (buffer);
return FALSE;
}
iline = 0;
for (s = buffer; ; s++)
{
ptr[++iline] = s;
if (! (s = (char *) memchr (s, '\n', lim - s)))
break;
}
if (size && lim[-1] != '\n')
ptr[++iline] = lim;
input_lines = iline - 1;
if (revision)
{
char const *rev = revision;
int rev0 = rev[0];
int found_revision = 0;
size_t revlen = strlen (rev);
if (revlen <= size)
{
char const *limrev = lim - revlen;
for (s = buffer; (s = (char *) memchr (s, rev0, limrev - s)); s++)
if (memcmp (s, rev, revlen) == 0
&& (s == buffer || ISSPACE ((unsigned char) s[-1]))
&& (s + 1 == limrev || ISSPACE ((unsigned char) s[revlen])))
{
found_revision = 1;
break;
}
}
report_revision (found_revision);
}
i_buffer = buffer;
i_ptr = ptr;
return TRUE;
}
static void
plan_b(filename)
char const *filename;
{
register FILE *ifp;
register int c;
register size_t len;
register size_t maxlen;
register int found_revision;
register size_t i;
register char const *rev;
register size_t revlen;
register LINENUM line = 1;
if (instat.st_size == 0)
filename = NULL_DEVICE;
if (! (ifp = fopen (filename, binary_transput ? "rb" : "r")))
pfatal ("can't open file `%s'", filename);
tifd = create_file (TMPINNAME, O_RDWR | O_BINARY, (mode_t) 0);
i = 0;
len = 0;
maxlen = 1;
rev = revision;
found_revision = !rev;
revlen = rev ? strlen (rev) : 0;
while ((c = getc (ifp)) != EOF)
{
len++;
if (c == '\n')
{
if (++line < 0)
too_many_lines (filename);
if (maxlen < len)
maxlen = len;
len = 0;
}
if (!found_revision)
{
if (i == revlen)
{
found_revision = ISSPACE ((unsigned char) c);
i = (size_t) -1;
}
else if (i != (size_t) -1)
i = rev[i]==c ? i + 1 : (size_t) -1;
if (i == (size_t) -1 && ISSPACE ((unsigned char) c))
i = 0;
}
}
if (revision)
report_revision (found_revision);
Fseek (ifp, (off_t) 0, SEEK_SET);
for (tibufsize = TIBUFSIZE_MINIMUM; tibufsize < maxlen; tibufsize <<= 1)
continue;
lines_per_buf = tibufsize / maxlen;
tireclen = maxlen;
tibuf[0] = xmalloc (2 * tibufsize);
tibuf[1] = tibuf[0] + tibufsize;
for (line = 1; ; line++)
{
char *p = tibuf[0] + maxlen * (line % lines_per_buf);
char const *p0 = p;
if (! (line % lines_per_buf))
if (write (tifd, tibuf[0], tibufsize) != tibufsize)
write_fatal ();
if ((c = getc (ifp)) == EOF)
break;
for (;;)
{
*p++ = c;
if (c == '\n')
{
last_line_size = p - p0;
break;
}
if ((c = getc (ifp)) == EOF)
{
last_line_size = p - p0;
line++;
goto EOF_reached;
}
}
}
EOF_reached:
if (ferror (ifp) || fclose (ifp) != 0)
read_fatal ();
if (line % lines_per_buf != 0)
if (write (tifd, tibuf[0], tibufsize) != tibufsize)
write_fatal ();
input_lines = line - 1;
}
char const *
ifetch (line, whichbuf, psize)
register LINENUM line;
int whichbuf;
size_t *psize;
{
register char const *q;
register char const *p;
if (line < 1 || line > input_lines) {
*psize = 0;
return "";
}
if (using_plan_a) {
p = i_ptr[line];
*psize = i_ptr[line + 1] - p;
return p;
} else {
LINENUM offline = line % lines_per_buf;
LINENUM baseline = line - offline;
if (tiline[0] == baseline)
whichbuf = 0;
else if (tiline[1] == baseline)
whichbuf = 1;
else {
tiline[whichbuf] = baseline;
if (lseek (tifd, (off_t) (baseline/lines_per_buf * tibufsize),
SEEK_SET) == -1
|| read (tifd, tibuf[whichbuf], tibufsize) < 0)
read_fatal ();
}
p = tibuf[whichbuf] + (tireclen*offline);
if (line == input_lines)
*psize = last_line_size;
else {
for (q = p; *q++ != '\n'; )
continue;
*psize = q - p;
}
return p;
}
}