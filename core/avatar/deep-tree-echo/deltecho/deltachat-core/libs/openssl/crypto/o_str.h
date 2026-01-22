#ifndef HEADER_O_STR_H
# define HEADER_O_STR_H
# include <stddef.h>
int OPENSSL_strcasecmp(const char *str1, const char *str2);
int OPENSSL_strncasecmp(const char *str1, const char *str2, size_t n);
int OPENSSL_memcmp(const void *p1, const void *p2, size_t n);
#endif