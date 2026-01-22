#include "all.h"
#ifndef	DATE
#define	DATE 1170808167L
#endif
Timet	fs_mktime = DATE;
Startsb	startsb[] = {
"main",		810988,
0
};
void
localconfinit(void)
{
conf.nodump = 0;
conf.dumpreread = 1;
conf.firstsb = 0;
conf.recovsb = 0;
conf.nlgmsg = 1100;
conf.nsmmsg = 500;
}
int (*fsprotocol[])(Msgbuf*) = {
serve9p1,
serve9p2,
nil,
};