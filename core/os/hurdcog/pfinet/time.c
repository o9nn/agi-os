#include <sys/time.h>
void
do_gettimeofday (struct timeval *tp)
{
gettimeofday (tp, 0);
}