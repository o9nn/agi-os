#include <mach.h>
#include <stdio.h>
#include <hurd.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <hurd/fd.h>
int
main ()
{
int fd;
FILE *fp;
static const char string[] = "Did this get into the file?\n";
int written;
setlinebuf (stdout);
setlinebuf (stderr);
if (unlink ("CREATED") < 0 && errno != ENOENT)
printf ("Error on unlink: %d\n", errno);
fd = open ("CREATED", O_WRITE | O_CREAT, 0666);
if (fd < 0)
printf ("Error on open: %d\n", errno);
{
size_t nbytes = strlen (string);
struct hurd_userlink __dt_ulink;
error_t __result;
struct hurd_fd_user __d = _hurd_fd_get (fd, &__dt_ulink);
if (__d.d == NULL)
__result = EBADF;
else
{
struct hurd_fd *const descriptor = __d.d;
__result = _hurd_fd_write (descriptor, string, &nbytes);
_hurd_fd_free (__d, &__dt_ulink);
}
if (__result)
errno = __result, written = -1;
else
written = nbytes;
}
if (written < 0)
printf ("Error on write: %d\n", errno);
else if (written != strlen (string))
printf ("Short write: %d\n", written);
else if (sync ())
printf ("Error on sync: %d\n", errno);
fp = fopen ("CREATED", "r");
if (! fp)
perror ("fopen");
else
{
char *line = NULL;
size_t len = 0;
ssize_t n = getline (&line, &len, fp);
if (n < 0)
perror ("getline");
else
printf ("Read %d bytes: %.*s", n, n, line);
free (line);
}
printf ("All done.\n");
malloc (0);
return 0;
}