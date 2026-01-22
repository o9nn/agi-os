#include <signal.h>
#include <sys/time.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <error.h>
void
alarm_handler (int signo)
{
printf ("Received alarm\n");
fflush (stdout);
}
int
main(int argc, char *argv[])
{
struct sigaction alarm_sigaction = { 0 };
sigset_t empty_sigset;
struct itimerval real_timer;
sigemptyset (&empty_sigset);
real_timer.it_interval.tv_usec = 0;
real_timer.it_interval.tv_sec = 1;
real_timer.it_value.tv_usec = 0;
real_timer.it_value.tv_sec = 1;
alarm_sigaction.sa_handler = &alarm_handler;
alarm_sigaction.sa_flags = SA_RESTART;
sigaction (SIGALRM, &alarm_sigaction, NULL);
if (setitimer (ITIMER_REAL, &real_timer, 0) < 0)
error (1, errno, "Setting timer");
while (1)
{
int c;
puts ("Pausing for input or one second...");
fflush (stdout);
c = getchar ();
if (ferror (stdin))
error (1, errno, "getchar");
if (c == EOF)
{
puts ("Saw EOF.  Pausing (no input)...");
fflush (stdout);
sigsuspend (&empty_sigset);
}
else
printf ("Saw %.3o\n", c);
}
}