#include <sys/types.h>
#include <stdbool.h>
#include <stdlib.h>
#include <ctype.h>
#include "error.h"
#include "mig_string.h"
string_t
strmake(const char *string)
{
string_t saved;
saved = malloc(strlen(string) + 1);
if (saved == strNULL)
fatal("strmake('%s'): %s", string, unix_error_string(errno));
return strcpy(saved, string);
}
string_t
strconcat(const_string_t left, const_string_t right)
{
string_t saved;
saved = malloc(strlen(left) + strlen(right) + 1);
if (saved == strNULL)
fatal("strconcat('%s', '%s'): %s",
left, right, unix_error_string(errno));
return strcat(strcpy(saved, left), right);
}
void
strfree(string_t string)
{
free(string);
}
const char *
strbool(bool v)
{
if (v)
return "TRUE";
else
return "FALSE";
}
const char *
strstring(const_string_t string)
{
if (string == strNULL)
return "NULL";
else
return string;
}
char *
strupper(const_string_t string)
{
string_t upper = strmake(string);
for (int i=0; i<strlen(upper); i++)
upper[i] = toupper(upper[i]);
return upper;
}