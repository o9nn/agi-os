#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
char *
localhost (void)
{
static char *buf = 0;
static size_t buf_len = 0;
if (! buf)
{
do {
errno = 0;
if (buf) {
char *new;
buf_len += buf_len;
new = realloc (buf, buf_len);
if (! new)
{
free (buf);
buf = 0;
errno = ENOMEM;
return 0;
}
else
buf = new;
} else {
buf_len = 128;
buf = malloc (buf_len);
if (! buf)
{
errno = ENOMEM;
return 0;
}
}
} while ((gethostname(buf, buf_len) == 0 && !memchr (buf, '\0', buf_len))
|| errno == ENAMETOOLONG);
if (errno)
{
free (buf);
buf = 0;
}
}
return buf;
}