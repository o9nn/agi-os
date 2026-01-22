#include "common.h"
#include "dat.h"
Biobuf in;
String *from;
String *sender;
void
usage(void)
{
fprint(2, "usage: %s address-list-file listname\n", argv0);
exits("usage");
}
void
main(int argc, char **argv)
{
String *msg;
char *alfile;
char *listname;
ARGBEGIN{
}ARGEND;
rfork(RFENVG|RFREND);
if(argc < 2)
usage();
alfile = argv[0];
listname = argv[1];
if(Binit(&in, 0, OREAD) < 0)
sysfatal("opening input: %r");
msg = s_new();
if(s_read_line(&in, msg) == nil)
sysfatal("reading input: %r");
if(s_read(&in, s_restart(msg), 128*1024) <= 0)
sysfatal("reading input: %r");
yyinit(s_to_c(msg), s_len(msg));
yyparse();
getaddrs();
if(from == nil)
from = sender;
if(from == nil)
sysfatal("message must contain From: or Sender:");
if(strstr(s_to_c(msg), "remove")||strstr(s_to_c(msg), "unsubscribe"))
writeaddr(alfile, s_to_c(from), 1, listname);
else if(strstr(s_to_c(msg), "subscribe"))
writeaddr(alfile, s_to_c(from), 0, listname);
exits(0);
}