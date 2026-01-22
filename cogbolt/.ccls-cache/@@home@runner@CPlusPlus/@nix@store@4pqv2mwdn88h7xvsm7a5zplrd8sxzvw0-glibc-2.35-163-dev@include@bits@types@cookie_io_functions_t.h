#ifndef __cookie_io_functions_t_defined
#define __cookie_io_functions_t_defined 1
#include <bits/types.h>
typedef __ssize_t cookie_read_function_t (void *__cookie, char *__buf,
size_t __nbytes);
typedef __ssize_t cookie_write_function_t (void *__cookie, const char *__buf,
size_t __nbytes);
typedef int cookie_seek_function_t (void *__cookie, __off64_t *__pos, int __w);
typedef int cookie_close_function_t (void *__cookie);
typedef struct _IO_cookie_io_functions_t
{
cookie_read_function_t *read;
cookie_write_function_t *write;
cookie_seek_function_t *seek;
cookie_close_function_t *close;
} cookie_io_functions_t;
#endif