#include "all.h"
#ifndef DATE
#define DATE 1170808167L
#endif
Timet fs_mktime = DATE;
Startsb startsb[] = {
"main", SUPER_ADDR,
"old", SUPER_ADDR,
nil,
};
void
localconfinit(void)
{
conf.nfile = 40000;
conf.nodump = 1;
conf.recovsb = 0;
conf.nlgmsg = 100;
conf.nsmmsg = 500;
}
int (*fsprotocol[])(Msgbuf*) = {
serve9p1,
serve9p2,
nil,
};