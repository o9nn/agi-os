#include <syslog.h>
#include <unistd.h>
#include <ttyent.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <errno.h>
#include <error.h>
#include <sys/utsname.h>
#include <stdlib.h>
#include <string.h>
#include <utmp.h>
#include <sys/ioctl.h>
#include <termios.h>
extern char *localhost (void);
#define _PATH_LOGIN "/bin/login"
#define _PATH_ISSUE "/etc/issue"
static void
set_speed (int tty, char *speedstr)
{
error_t err;
struct termios ttystat;
speed_t speed;
char *tail;
errno = 0;
speed = strtoul (speedstr, &tail, 0);
if (errno || *tail)
return;
err = tcgetattr (tty, &ttystat);
if (!err && !cfsetspeed (&ttystat, speed))
tcsetattr (tty, TCSAFLUSH, &ttystat);
}
static char *
load_banner (void)
{
char *buf = NULL, *p;
struct stat st;
int fd = -1;
ssize_t remaining, count;
fd = open (_PATH_ISSUE, O_RDONLY);
if (fd == -1)
goto out;
if (fstat (fd, &st) == -1)
goto out;
buf = malloc (st.st_size + 1);
if (buf == NULL)
goto out;
remaining = st.st_size;
p = buf;
while (remaining > 0)
{
count = read (fd, p, remaining);
if (count == -1)
{
close (fd);
goto out;
}
p += count;
remaining -= count;
}
buf[st.st_size] = '\0';
close (fd);
return buf;
out:
if (fd != -1)
close (fd);
free (buf);
return "\n\\s \\r (\\n) (\\l)\r\n\n";
}
static void
print_banner (int fd, char *ttyname)
{
char *s, *t, *expansion;
struct utsname u;
if (uname (&u))
u.sysname[0] = u.release[0] = '\0';
write (fd, "\r\n", 2);
for (s = load_banner (); *s; s++)
{
for (t = s; *t && *t != '\\'; t++) ;
write (fd, s, t - s);
if (! *t)
return;
switch (*(t + 1))
{
case '\\':
expansion = "\\";
break;
case 's':
expansion = u.sysname;
break;
case 'r':
expansion = u.release;
break;
case 'n':
expansion = localhost () ?: "?";
break;
case 'l':
expansion = basename (ttyname);
break;
default:
expansion = "?";
}
write (fd, expansion, strlen (expansion));
s = t + 1;
}
}
int
main (int argc, char **argv)
{
char *linespec, *ttyname;
int tty;
struct ttyent *tt;
char *arg;
openlog ("getty", LOG_ODELAY|LOG_CONS|LOG_PID, LOG_AUTH);
if (argc != 3)
{
syslog (LOG_ERR, "Bad syntax");
closelog ();
exit (1);
}
linespec = argv[1];
tt = getttynam (argv[2]);
asprintf (&ttyname, "%s/%s", _PATH_DEV, argv[2]);
chown (ttyname, 0, 0);
chmod (ttyname, 0600);
revoke (ttyname);
sleep (2);
do
{
tty = open (ttyname, O_RDWR);
if (tty == -1)
{
syslog (LOG_ERR, "%s: %m", ttyname);
closelog ();
sleep (60);
}
}
while (tty == -1);
set_speed (tty, linespec);
print_banner (tty, ttyname);
if (login_tty (tty) == -1)
syslog (LOG_ERR, "cannot set controlling terminal to %s: %m", ttyname);
asprintf (&arg, "TERM=%s", tt ? tt->ty_type : "unknown");
if (tt && strcmp (tt->ty_type, "dialup") == 0)
execl (_PATH_LOGIN, "login", "-e", arg, NULL);
else
execl (_PATH_LOGIN, "login", "-e", arg, "-aNOAUTH_TIMEOUT", NULL);
syslog (LOG_ERR, "%s: %m", _PATH_LOGIN);
return 1;
}