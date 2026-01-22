#if HAVE_CONFIG_H
# include <config.h>
#endif
#include <sys/types.h>
#include <quotearg.h>
size_t
quote_system_arg (quoted, arg)
char *quoted;
char const *arg;
{
char const *a;
size_t len = 0;
for (a = arg; ; a++)
{
char c = *a;
switch (c)
{
case 0:
return len;
case '=':
if (*arg == '-')
break;
case '\t': case '\n': case ' ':
case '!': case '"': case '#': case '$': case '%': case '&': case '\'':
case '(': case ')': case '*': case ';':
case '<': case '>': case '?': case '[': case '\\':
case '^': case '`': case '|': case '~':
{
len = 0;
c = *arg++;
if (c == '-'  &&  arg < a)
{
c = *arg++;
if (quoted)
{
quoted[len] = '-';
quoted[len + 1] = c;
}
len += 2;
if (c == '-')
while (arg < a)
{
c = *arg++;
if (quoted)
quoted[len] = c;
len++;
if (c == '=')
break;
}
c = *arg++;
}
if (quoted)
quoted[len] = '\'';
len++;
for (;  c;  c = *arg++)
{
if (c == '\'')
{
if (quoted)
{
quoted[len] = '\'';
quoted[len + 1] = '\\';
quoted[len + 2] = '\'';
}
len += 3;
}
if (quoted)
quoted[len] = c;
len++;
}
if (quoted)
quoted[len] = '\'';
return len + 1;
}
}
if (quoted)
quoted[len] = c;
len++;
}
}