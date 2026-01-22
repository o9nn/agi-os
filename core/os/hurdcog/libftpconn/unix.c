#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <errno.h>
#include <pwd.h>
#include <grp.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <libgen.h>
#ifdef HAVE_HURD_HURD_TYPES_H
#include <hurd/hurd_types.h>
#endif
#include <ftpconn.h>
#define DEFAULT_UID 65535
#define DEFAULT_GID 65535
struct ftp_conn_syshooks ftp_conn_unix_syshooks = {
ftp_conn_unix_pasv_addr, ftp_conn_unix_interp_err,
ftp_conn_unix_start_get_stats, ftp_conn_unix_cont_get_stats,
ftp_conn_unix_append_name, ftp_conn_unix_basename
};
error_t
ftp_conn_unix_pasv_addr (struct ftp_conn *conn, const char *txt,
struct sockaddr **addr)
{
unsigned a0, a1, a2, a3;
unsigned p0, p1;
if (sscanf (txt, "%*[^0-9]%d,%d,%d,%d,%d,%d", &a0,&a1,&a2,&a3, &p0,&p1) != 6)
return EGRATUITOUS;
else
{
unsigned char *a, *p;
*addr = malloc (sizeof (struct sockaddr_in));
if (! *addr)
return ENOMEM;
(*addr)->sa_len = sizeof (struct sockaddr_in);
(*addr)->sa_family = AF_INET;
a = (unsigned char *)&((struct sockaddr_in *)*addr)->sin_addr.s_addr;
a[0] = a0 & 0xff;
a[1] = a1 & 0xff;
a[2] = a2 & 0xff;
a[3] = a3 & 0xff;
p = (unsigned char *)&((struct sockaddr_in *)*addr)->sin_port;
p[0] = p0 & 0xff;
p[1] = p1 & 0xff;
return 0;
}
}
static int
strlaxcmp (const char *p, const char *q)
{
for (;;)
{
int ch1, ch2;
while (*p && !isalnum (*p))
p++;
while (*q && !isalnum (*q))
q++;
if (!*p || !*q)
break;
ch1 = tolower (*p);
ch2 = tolower (*q);
if (ch1 != ch2)
break;
p++;
q++;
}
return *p - *q;
}
error_t
ftp_conn_unix_interp_err (struct ftp_conn *conn, const char *txt,
const error_t *poss_errs)
{
const char *p;
const error_t *e;
if (!poss_errs || !poss_errs[0])
return EIO;
p = strrchr (txt, ':');
if (p)
p++;
else
p = txt;
for (e = poss_errs; *e; e++)
if (strlaxcmp (p, strerror (*e)) == 0)
return *e;
return poss_errs[0];
}
struct get_stats_state
{
char *name;
size_t name_len;
size_t name_alloced;
int name_partial;
int contents;
char *searched_name;
int added_slash;
struct stat stat;
int start;
size_t buf_len;
char buf[7000];
};
error_t
ftp_conn_unix_start_get_stats (struct ftp_conn *conn,
const char *name, int contents,
int *fd, void **state)
{
error_t err = 0;
size_t req_len;
char *req = NULL;
struct get_stats_state *s = NULL;
const char *flags = "-A";
const char *slash = strchr (name, '/');
char *searched_name = NULL;
s = (struct get_stats_state *) malloc (sizeof (struct get_stats_state));
if (! s)
{
err = ENOMEM;
goto out;
}
if (! contents)
{
if (! strcmp (name, "/"))
{
err = EINVAL;
}
else
{
searched_name = strdup (basename ((char *) name));
if (! searched_name)
err = ENOMEM;
}
if (err)
goto out;
}
if (strcspn (name, "*? \t\n{}$`\\\"'") < strlen (name))
{
err = EINVAL;
goto out;
}
req_len = strlen (flags) + 2;
if (! contents)
{
char *dirn = dirname (strdupa (name));
int is_root = ! strcmp (dirn, "/");
req_len += strlen (dirn) + (is_root ? 0 : 1);
req = malloc (req_len);
if (! req)
err = ENOMEM;
else
sprintf (req, "%s %s%s", flags, dirn, (is_root ? "" : "/"));
}
else
{
req_len += strlen (name) + (slash ? 0 : 2);
req = malloc (req_len);
if (! req)
err = ENOMEM;
else
sprintf (req, "%s %s%s", flags, slash ? "" : "./", name);
}
if (err)
goto out;
err = ftp_conn_start_dir (conn, req, fd);
out:
free (req);
if (err)
{
free (s);
free (searched_name);
}
else
{
s->contents = contents;
s->searched_name = searched_name;
s->added_slash = !slash;
s->name = 0;
s->name_len = s->name_alloced = 0;
s->name_partial = 0;
s->buf_len = 0;
s->start = 1;
*state = s;
}
return err;
}
static char *months[] =
{
"jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct",
"nov", "dec", 0
};
static error_t
parse_dir_entry (char **line, struct stat *stat)
{
char **m;
struct tm tm;
char *p = *line, *e;
if (strncasecmp (p, "total ", 6) == 0)
return EAGAIN;
memset (stat, 0, sizeof *stat);
#ifdef FSTYPE_FTP
stat->st_fstype = FSTYPE_FTP;
#endif
switch (*p++)
{
case '-': stat->st_mode |= S_IFREG; break;
case 'd': stat->st_mode |= S_IFDIR; break;
case 'c': stat->st_mode |= S_IFCHR; break;
case 'b': stat->st_mode |= S_IFBLK; break;
case 'l': stat->st_mode |= S_IFLNK; break;
case 's': stat->st_mode |= S_IFSOCK; break;
case 'p': stat->st_mode |= S_IFIFO; break;
default: return EGRATUITOUS;
}
switch (*p++)
{
case '-': break;
case 'r': stat->st_mode |= S_IRUSR; break;
default: return EGRATUITOUS;
}
switch (*p++)
{
case '-': break;
case 'w': stat->st_mode |= S_IWUSR; break;
default: return EGRATUITOUS;
}
switch (*p++)
{
case '-': break;
case 'x': stat->st_mode |= S_IXUSR; break;
case 's': stat->st_mode |= S_IXUSR | S_ISUID; break;
case 'S': stat->st_mode |= S_ISUID; break;
default: return EGRATUITOUS;
}
switch (*p++)
{
case '-': break;
case 'r': stat->st_mode |= S_IRGRP; break;
default: return EGRATUITOUS;
}
switch (*p++)
{
case '-': break;
case 'w': stat->st_mode |= S_IWGRP; break;
default: return EGRATUITOUS;
}
switch (*p++)
{
case '-': break;
case 'x': stat->st_mode |= S_IXGRP; break;
case 's': stat->st_mode |= S_IXGRP | S_ISGID; break;
case 'S': stat->st_mode |= S_ISGID; break;
default: return EGRATUITOUS;
}
switch (*p++)
{
case '-': break;
case 'r': stat->st_mode |= S_IROTH; break;
default: return EGRATUITOUS;
}
switch (*p++)
{
case '-': break;
case 'w': stat->st_mode |= S_IWOTH; break;
default: return EGRATUITOUS;
}
switch (*p++)
{
case '-': break;
case 'x': stat->st_mode |= S_IXOTH; break;
case 't': stat->st_mode |= S_IXOTH | S_ISVTX; break;
case 'T': stat->st_mode |= S_ISVTX; break;
default: return EGRATUITOUS;
}
#define SKIP_WS() \
while (isspace (*p)) p++;
#define PARSE_INT() ({ \
unsigned u = strtoul (p, &e, 10); \
if (e == p || isalnum (*e)) \
return EGRATUITOUS; \
p = e; \
u; \
})
SKIP_WS ();
stat->st_nlink = PARSE_INT ();
SKIP_WS ();
if (isdigit (*p))
stat->st_uid = PARSE_INT ();
else
{
struct passwd *pw;
e = p + strcspn (p, " \t\n");
*e++ = '\0';
pw = getpwnam (p);
if (pw)
stat->st_uid = pw->pw_uid;
else
stat->st_uid = DEFAULT_UID;
p = e;
}
#ifdef HAVE_STAT_ST_AUTHOR
stat->st_author = stat->st_uid;
#endif
SKIP_WS ();
if (isdigit (*p))
stat->st_gid = PARSE_INT ();
else
{
struct group *gr;
e = p + strcspn (p, " \t\n");
*e++ = '\0';
gr = getgrnam (p);
if (gr)
stat->st_gid = gr->gr_gid;
else
stat->st_gid = DEFAULT_GID;
p = e;
}
SKIP_WS ();
if (S_ISCHR (stat->st_mode) || S_ISBLK (stat->st_mode))
{
stat->st_dev = PARSE_INT ();
if (*p != ',')
return EGRATUITOUS;
stat->st_dev = (stat->st_dev << 8) | PARSE_INT ();
stat->st_size = 0;
}
else
stat->st_size = PARSE_INT ();
stat->st_blocks = stat->st_size >> 9;
memset (&tm, 0, sizeof tm);
SKIP_WS ();
e = p + strcspn (p, " \t\n");
for (m = months; *m; m++)
if (strncasecmp (*m, p, e - p) == 0)
{
tm.tm_mon = m - months;
break;
}
if (! *m)
return EGRATUITOUS;
p = e;
SKIP_WS ();
tm.tm_mday = PARSE_INT ();
SKIP_WS ();
if (p[1] == ':' || p[2] == ':')
{
struct tm *now_tm;
struct timeval now_tv;
tm.tm_hour = PARSE_INT ();
p++;
tm.tm_min = PARSE_INT ();
if (gettimeofday (&now_tv, 0) != 0)
return errno;
now_tm = localtime (&now_tv.tv_sec);
if (now_tm->tm_mon < tm.tm_mon)
tm.tm_year = now_tm->tm_year - 1;
else
tm.tm_year = now_tm->tm_year;
}
else
tm.tm_year = PARSE_INT () - 1900;
stat->st_mtim.tv_sec = mktime (&tm);
if (stat->st_mtim.tv_sec == (time_t)-1)
return EGRATUITOUS;
stat->st_atim.tv_sec = stat->st_ctim.tv_sec = stat->st_mtim.tv_sec;
stat->st_atim.tv_nsec = stat->st_ctim.tv_nsec = stat->st_mtim.tv_nsec = 0;
SKIP_WS ();
*line = p;
return 0;
}
error_t
ftp_conn_unix_cont_get_stats (struct ftp_conn *conn, int fd, void *state,
ftp_conn_add_stat_fun_t add_stat, void *hook)
{
char *p, *nl;
ssize_t rd;
size_t name_len;
error_t err = 0;
struct get_stats_state *s = state;
int (*icheck) (struct ftp_conn *conn) = conn->hooks->interrupt_check;
rd = read (fd, s->buf + s->buf_len, sizeof (s->buf) - s->buf_len);
if (rd < 0)
{
err = errno;
goto finished;
}
if (icheck && (*icheck) (conn))
{
err = EINTR;
goto finished;
}
if (rd == 0)
if (s->buf_len == 0)
{
if (s->start)
err = ENOENT;
goto finished;
}
else
nl = s->buf + s->buf_len;
else
{
nl = memchr (s->buf + s->buf_len, '\n', rd);
s->buf_len += rd;
}
s->start = 0;
if (!nl && s->buf_len < sizeof (s->buf))
return EAGAIN;
p = s->buf;
do
{
if (! s->name_partial)
{
err = parse_dir_entry (&p, &s->stat);
if (err == EAGAIN)
goto skip_line;
if (err)
goto finished;
}
name_len = (nl ? nl - p : s->buf + s->buf_len - p);
if (name_len > 0 && p[name_len - 1] == '\r')
name_len--;
if (name_len > 0)
{
size_t old_len = s->name_len;
size_t total_len = old_len + name_len + 1;
if (total_len > s->name_alloced)
{
char *new_name = realloc (s->name, total_len);
if (! new_name)
goto enomem;
s->name = new_name;
s->name_alloced = total_len;
}
strncpy (s->name + old_len, p, name_len);
s->name[old_len + name_len] = '\0';
s->name_len = total_len - 1;
}
if (nl)
{
char *name = s->name;
char *symlink_target = 0;
if (S_ISLNK (s->stat.st_mode))
{
symlink_target = strstr (name, " -> ");
if (symlink_target)
{
*symlink_target = '\0';
symlink_target += 4;
}
}
if (strchr (name, '/'))
{
if (s->contents)
{
err = ENOTDIR;
goto finished;
}
else if (s->added_slash)
name += 2;
}
name = basename (name);
if (s->contents || ! strcmp (s->name, s->searched_name))
{
err = (*add_stat) (name, &s->stat, symlink_target, hook);
if (err)
goto finished;
}
s->name_len = 0;
s->name_partial = 0;
skip_line:
p = nl + 1;
nl = memchr (p, '\n', s->buf + s->buf_len - p);
}
else
{
s->name_partial = 1;
p += name_len;
}
}
while (nl);
s->buf_len -= (p - s->buf);
if (s->buf_len > 0)
memmove (s->buf, p, s->buf_len);
return EAGAIN;
enomem:
err = ENOMEM;
finished:
if (s->name)
free (s->name);
if (s->searched_name)
free (s->searched_name);
free (s);
close (fd);
if (err && rd > 0)
ftp_conn_abort (conn);
else if (err)
ftp_conn_finish_transfer (conn);
else
err = ftp_conn_finish_transfer (conn);
return err;
}
error_t
ftp_conn_unix_append_name (struct ftp_conn *conn,
const char *dir, const char *name,
char **composite)
{
char *path = malloc (strlen (dir) + 1 + strlen (name) + 1);
if (! path)
return ENOMEM;
if (name && *name)
if (dir[0] == '/' && dir[1] == '\0')
stpcpy (stpcpy (path, dir), name);
else
stpcpy (stpcpy (stpcpy (path, dir), "/"), name);
else
strcpy (path, dir);
*composite = path;
return 0;
}
error_t
ftp_conn_unix_basename (struct ftp_conn *conn, char **name)
{
*name = basename (*name);
return 0;
}