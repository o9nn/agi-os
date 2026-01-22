#include <u.h>
#include <libc.h>
#include <ctype.h>
int
hangup(int ctl)
{
return write(ctl, "hangup", sizeof("hangup")-1) != sizeof("hangup")-1;
}