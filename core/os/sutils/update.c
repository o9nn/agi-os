#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
int
main (int argc, char **argv)
{
int interval;
switch (argc)
{
case 1:
interval = 30;
break;
case 2:
interval = atoi (argv[1]);
break;
default:
fprintf (stderr, "Usage: %s [SECONDS]\n", argv[0]);
exit (1);
}
if (daemon (0, 0))
error (1, errno, "daemon");
for (;;)
{
sync ();
sleep (interval);
}
}