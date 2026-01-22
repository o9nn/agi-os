#ifndef	_MIG_STRING_H
#define	_MIG_STRING_H
#include <stdbool.h>
#include <string.h>
typedef char *string_t;
typedef const char *const_string_t;
typedef const_string_t identifier_t;
#define	strNULL		((string_t) 0)
extern string_t strmake(const char *string);
extern string_t strconcat(const_string_t left, const_string_t right);
extern void strfree(string_t string);
#define	streql(a, b)	(strcmp((a), (b)) == 0)
extern const char *strbool(bool v);
extern const char *strstring(const_string_t string);
extern char *strupper(const_string_t string);
#endif