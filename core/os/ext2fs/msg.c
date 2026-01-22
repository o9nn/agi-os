#include <stdio.h>
#include <stdarg.h>
#include "ext2fs.h"
pthread_mutex_t printf_lock = PTHREAD_MUTEX_INITIALIZER;
int printf (const char *fmt, ...)
{
va_list arg;
int done;
va_start (arg, fmt);
pthread_mutex_lock (&printf_lock);
done = vprintf (fmt, arg);
pthread_mutex_unlock (&printf_lock);
va_end (arg);
return done;
}
static char error_buf[1024];
void _ext2_error (const char * function, const char * fmt, ...)
{
va_list args;
pthread_mutex_lock (&printf_lock);
va_start (args, fmt);
vsnprintf (error_buf, sizeof (error_buf), fmt, args);
va_end (args);
fprintf (stderr, "ext2fs: %s: %s: %s\n", diskfs_disk_name, function, error_buf);
pthread_mutex_unlock (&printf_lock);
}
void _ext2_panic (const char * function, const char * fmt, ...)
{
va_list args;
pthread_mutex_lock (&printf_lock);
va_start (args, fmt);
vsnprintf (error_buf, sizeof (error_buf), fmt, args);
va_end (args);
fprintf(stderr, "ext2fs: %s: panic: %s: %s\n",
diskfs_disk_name, function, error_buf);
pthread_mutex_unlock (&printf_lock);
exit (1);
}
void ext2_warning (const char * fmt, ...)
{
va_list args;
pthread_mutex_lock (&printf_lock);
va_start (args, fmt);
vsnprintf (error_buf, sizeof (error_buf), fmt, args);
va_end (args);
fprintf (stderr, "ext2fs: %s: warning: %s\n", diskfs_disk_name, error_buf);
pthread_mutex_unlock (&printf_lock);
}