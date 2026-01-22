#ifdef RCSID
static char rcsid[] = "$Id: util.c,v 1.1 1994/12/14 04:29:37 roland Exp $";
#endif
#include <stddef.h>
int (*unzip_read) (char *buf, size_t maxread);
void (*unzip_write) (const char *buf, size_t nwrite);
void (*unzip_read_error) (void);
void (*unzip_error) (const char *msg);