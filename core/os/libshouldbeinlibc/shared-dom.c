#include <string.h>
char *
shared_domain (char *host1, char *host2)
{
char *shared, *e1, *e2;
if (!host1 || !host2)
return 0;
e2 = host2 + strlen (host2);
e1 = host1 + strlen (host1);
shared = e1;
if (*e1 == '.')
e1--;
if (*e2 == '.')
e2--;
while (e1 > host1 && e2 > host2 && *e2 == *e1)
{
if (*e1 == '.')
shared = e1;
e1--;
e2--;
}
return shared;
}