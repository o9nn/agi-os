#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <pwd.h>
#include <syslog.h>
#include <sysexits.h>
#include <paths.h>
#include <argp.h>
#include <hurd.h>
#include <hurd/fd.h>
#include <version.h>
#include <time.h>
#include <sys/time.h>
#include <sys/mman.h>
#define OPT_FILE -5
#define OPT_REMOVE -6
const char *argp_program_version = STANDARD_HURD_VERSION (mail.local);
static const struct argp_option
options[] =
{
{"from",    'f',	"USER",	0, "Record sender as USER"},
{0,         'd',	0,     	OPTION_ALIAS|OPTION_HIDDEN},
{0,         'r',	0,     	OPTION_ALIAS|OPTION_HIDDEN},
{"file",    OPT_FILE, "FILE",	0, "Deliver FILE instead of standard input"},
{"remove",  OPT_REMOVE, 0,   	0, "Remove FILE after successful delivery"},
{"mail-dir",'m',	"DIR", 	0, "Look for mailboxes in DIR"},
{"use-lock-file",'l',	0,     	OPTION_HIDDEN,
"Use a lock file instead of flock for mailboxes"},
{0}
};
static const char args_doc[] = "USER...";
static const char doc[] = "Deliver mail to the local mailboxes of USER...";
#define HDR_PFX "From "
#define ESC_PFX ">"
#define BMAX (64*1024)
struct params
{
char *from;
char *mail_dir;
};
static int
err_to_ex (error_t err)
{
switch (err)
{
case 0:
return 0;
#if defined(EWOULDBLOCK) && (EWOULDBLOCK != EAGAIN)
case EWOULDBLOCK:
#endif
case EAGAIN:
case EDQUOT:
case EBUSY:
case EPROCLIM:
case EUSERS:
case ECONNABORTED:
case ECONNREFUSED:
case ECONNRESET:
case EDEADLK:
case EFBIG:
case EHOSTDOWN:
case EHOSTUNREACH:
case EMFILE:
case ENETDOWN:
case ENETRESET:
case ENETUNREACH:
case ENFILE:
case ENOBUFS:
case ENOMEM:
case ENOSPC:
case EROFS:
case ESTALE:
case ETIMEDOUT:
return EX_TEMPFAIL;
default:
return EX_UNAVAILABLE;
}
}
#define SYSERR(fmt, args...) \
({ syslog (LOG_ERR, fmt ": %m" , ##args); err_to_ex (errno); })
#define ERR(fmt, args...) \
({ syslog (LOG_ERR, fmt , ##args); EX_UNAVAILABLE; })
#define SYSERRX(code, fmt, args...)					      \
({ error_t _code = (code);						      \
syslog (LOG_ERR, fmt ": %s" , ##args , strerror (_code));		      \
err_to_ex (_code); })
static void
bfree (char *blk, size_t blk_len)
{
if (blk_len > 0)
munmap (blk, blk_len);
}
static int
bread (int in, char *in_name, size_t max, char **blk,
mach_msg_type_number_t *blk_len)
{
char *orig_blk = *blk;
mach_msg_type_number_t orig_blk_len = *blk_len;
error_t err = HURD_DPORT_USE (in, io_read (port, blk, blk_len, -1, max));
if (err)
return SYSERRX (err, "%s", in_name);
if (*blk != orig_blk)
bfree (orig_blk, orig_blk_len);
return 0;
}
static int
bwrite (int out, char *out_name, const char *blk, size_t blk_len)
{
while (blk_len > 0)
{
ssize_t wr = write (out, blk, blk_len);
if (wr < 0)
return SYSERR ("%s", out_name);
blk += wr;
blk_len -= wr;
}
return 0;
}
static int
copy (int in, char *in_name, int out, char *out_name)
{
int ex = 0;
char *blk = 0;
mach_msg_type_number_t blk_len = 0;
do
{
ex = bread (in, in_name, BMAX, &blk, &blk_len);
if (! ex)
ex = bwrite (out, out_name, blk, blk_len);
}
while (blk_len > 0 && !ex);
bfree (blk, blk_len);
return ex;
}
static int
write_header (int out, char *out_name, struct params *params)
{
char *hdr;
size_t hdr_len;
struct timeval tv;
time_t time;
int ex = 0;
if (gettimeofday (&tv, 0) < 0)
return SYSERR ("gettimeofday");
time = tv.tv_sec;
hdr_len = asprintf (&hdr, "From %s %s", params->from, ctime (&time));
if (! hdr)
return SYSERRX (ENOMEM, "%s", out_name);
ex = bwrite (out, out_name, hdr, hdr_len);
free (hdr);
return ex;
}
static int
process (int in, char *in_name, int out, char *out_name, struct params *params)
{
char *blk = 0;
mach_msg_type_number_t blk_len = 0;
const char *const nl_match = "\n" HDR_PFX, *const match = nl_match + 1;
ssize_t matched = 0;
int ex = write_header (out, out_name, params);
#define match_len (sizeof HDR_PFX - 1)
if (ex)
return ex;
#define BWRITE(p, p_len)						      \
({ size_t _len = (p_len);						      \
if (_len > 0 && (ex = bwrite (out, out_name, p, _len)))		      \
break; })
do
{
char *start, *end;
ex = bread (in, in_name, BMAX, &blk, &blk_len);
if (matched >= 0)
{
if (blk_len >= match_len - matched
&& memcmp (blk, match + matched, match_len - matched) == 0)
BWRITE (ESC_PFX, sizeof ESC_PFX - 1);
BWRITE (match, matched);
matched = -1;
}
for (start = end = blk; start < blk + blk_len; start = end)
{
end = memmem (start, blk + blk_len - start, nl_match, match_len + 1);
if (end)
{
end++;
BWRITE (start, end - start);
BWRITE (ESC_PFX, sizeof ESC_PFX - 1);
}
else
{
end = blk + blk_len;
break;
}
}
for (matched =
end - start < match_len + 1 ? end - start - 1 : match_len;
matched >= 0;
matched--)
if (memcmp (end - matched - 1, nl_match, matched + 1) == 0)
{
end -= matched;
break;
}
BWRITE (start, end - start);
}
while (blk_len > 0);
if (! ex)
ex = bwrite (out, out_name, "\n", 1);
bfree (blk, blk_len);
return ex;
}
#define D_PROCESS 0x1
#define D_REWIND  0x2
static int
deliver (int msg, char *msg_name, char *rcpt, int flags, struct params *params)
{
char *mbox;
int fd;
struct stat stat;
int ex = 0;
struct passwd *pw = getpwnam (rcpt);
if (! pw)
return ERR ("%s: Unknown user", rcpt);
asprintf (&mbox, "%s/%s", params->mail_dir, rcpt);
if (! mbox)
return SYSERRX (ENOMEM, "%s", rcpt);
do
{
fd = open (mbox, O_WRONLY|O_APPEND|O_NOLINK|O_EXLOCK);
if (fd < 0 && errno == ENOENT)
{
fd = open (mbox, O_WRONLY|O_APPEND|O_CREAT|O_EXCL|O_NOLINK|O_EXLOCK,
S_IRUSR|S_IWUSR);
if (fd >= 0)
{
if (fchown (fd, pw->pw_uid, pw->pw_gid) < 0)
{
close (fd);
fd = -1;
}
}
}
}
while (fd < 0 && errno == EEXIST);
if (fd < 0 || fstat (fd, &stat) < 0)
ex = SYSERR ("%s", mbox);
else if (S_ISLNK (stat.st_mode) || stat.st_nlink != 1)
ex = ERR ("%s: Is linked", mbox);
else
{
if (flags & D_REWIND)
{
if (lseek (msg, 0L, SEEK_SET) < 0)
ex = SYSERR ("%s", msg_name);
}
if (! ex)
{
if (flags & D_PROCESS)
ex = process (msg, msg_name, fd, mbox, params);
else
ex = copy (msg, msg_name, fd, mbox);
}
}
if (fd >= 0)
{
if (fsync (fd) < 0 && !ex)
ex = SYSERR ("%s", mbox);
if (close (fd) < 0 && !ex)
ex = SYSERR ("%s", mbox);
}
free (mbox);
return ex;
}
static int
cache (int in, char *in_name, struct params *params, int *cached)
{
int ex;
error_t err;
file_t file;
int fd;
file_t tmp_dir = file_name_lookup (_PATH_TMP, O_RDONLY, 0);
if (tmp_dir == MACH_PORT_NULL)
return SYSERR ("%s", _PATH_TMP);
err = dir_mkfile (tmp_dir, O_RDWR, 0600, &file);
if (err)
return SYSERRX (err, "%s", _PATH_TMP);
fd = _hurd_intern_fd (file, O_RDWR, 1);
if (fd < 0)
return SYSERR ("%s", _PATH_TMP);
ex = process (in, in_name, fd, _PATH_TMP, params);
if (! ex)
*cached = fd;
else
close (fd);
return ex;
}
int
main (int argc, char **argv)
{
int rcpt = 0;
char *file = 0;
int remove = 0;
int in = 0;
int ex = 0;
struct params params = { from: 0, mail_dir: _PATH_MAILDIR };
error_t parse_opt (int key, char *arg, struct argp_state *state)
{
switch (key)
{
case 'd':
break;
case 'f':
case 'r':
params.from = arg; break;
case OPT_FILE:
file = arg; break;
case OPT_REMOVE:
remove = 1; break;
case 'm':
params.mail_dir = arg; break;
case 'l':
argp_failure (state, EX_USAGE, EINVAL, "-l not supported");
case ARGP_KEY_NO_ARGS:
argp_error (state, "No recipients");
default:
return ARGP_ERR_UNKNOWN;
}
return 0;
}
const struct argp argp = { options, parse_opt, args_doc, doc };
argp_parse (&argp, argc, argv, 0, &rcpt, 0);
openlog ("mail.local", LOG_PERROR, LOG_MAIL);
if (! params.from)
{
struct passwd *pw;
int uid = getuid ();
if (uid == -1)
exit (ERR ("No user id"));
pw = getpwuid (uid);
if (! pw)
exit (ERR ("%d: Unknown uid", uid));
params.from = strdup (pw->pw_name);
}
if (file)
{
in = open (file, O_RDONLY);
if (in < 0)
exit (SYSERR ("%s", file));
}
else
in = 0;
if (rcpt == argc - 1)
ex = deliver (in, file ?: "-", argv[rcpt], D_PROCESS, &params);
else
{
int cached = 0;
ex = cache (in, file ?: "-", &params, &cached);
if (! ex)
while (rcpt < argc)
{
int rex = deliver (cached, "message cache", argv[rcpt++],
D_REWIND, &params);
if (ex != EX_TEMPFAIL)
{
if (rex == EX_TEMPFAIL)
ex = EX_TEMPFAIL;
else if (! ex)
ex = rex;
}
}
}
if (file && remove && !ex)
unlink (file);
exit (ex);
}