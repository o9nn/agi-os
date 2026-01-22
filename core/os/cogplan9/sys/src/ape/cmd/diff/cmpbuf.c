#include "system.h"
#include "cmpbuf.h"
size_t
buffer_lcm (a, b)
size_t a, b;
{
size_t m, n, r;
if (!a)
return b ? b : 8 * 1024;
if (!b)
return a;
for (m = a, n = b; (r = m % n) != 0; m = n, n = r)
continue;
return a/n * b;
}