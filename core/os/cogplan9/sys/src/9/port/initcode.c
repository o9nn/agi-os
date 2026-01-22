#include <u.h>
#include <libc.h>
char cons[] = "#c/cons";
char boot[] = "/boot/boot";
char dev[] = "/dev";
char c[] = "#c";
char e[] = "#e";
char ec[] = "#ec";
char s[] = "#s";
char srv[] = "/srv";
char env[] = "/env";
void
startboot(char *argv0, char **argv)
{
char buf[200];
open(cons, OREAD);
open(cons, OWRITE);
open(cons, OWRITE);
bind(c, dev, MAFTER);
bind(ec, env, MAFTER);
bind(e, env, MCREATE|MAFTER);
bind(s, srv, MREPL|MCREATE);
USED(argv0);
exec(boot, argv);
rerrstr(buf, sizeof buf);
buf[sizeof buf - 1] = '\0';
_exits(buf);
}