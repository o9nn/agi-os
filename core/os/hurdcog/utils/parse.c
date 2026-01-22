#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <error.h>
#include "parse.h"
error_t
_parse_strlist (char *arg,
error_t (*add_fn)(const char *str, struct argp_state *state),
error_t (*default_add_fn)(struct argp_state *state),
const char *type_name, struct argp_state *state)
{
if (arg)
while (isspace(*arg))
arg++;
if (arg == NULL || *arg == '\0')
if (default_add_fn)
return (*default_add_fn)(state);
else
{
argp_error (state, "Empty %s list", type_name);
return EINVAL;
}
else
{
error_t err = 0;
char *end = arg;
void mark_end(void)
{
*end++ = '\0';
while (isspace(*end))
end++;
}
error_t parse_element(void)
{
char *cur = arg;
if (*cur == '\0')
{
argp_error (state, "Empty element in %s list", type_name);
return EINVAL;
}
arg = end;
return (*add_fn)(cur, state);
}
while (*end != '\0' && !err)
switch (*end)
{
case ' ': case '\t':
mark_end();
if (*end == ',')
mark_end();
err = parse_element();
break;
case ',':
mark_end();
err = parse_element();
break;
default:
end++;
}
if (! err)
err = parse_element();
return err;
}
}
error_t
parse_strlist (char *arg,
error_t (*add_fn)(const char *str, struct argp_state *state),
const char *(*default_fn)(struct argp_state *state),
const char *type_name, struct argp_state *state)
{
error_t default_str_add (struct argp_state *state)
{ return (*add_fn)((*default_fn)(state), state); }
return _parse_strlist (arg, add_fn, default_str_add, type_name, state);
}
error_t
parse_numlist (char *arg,
error_t (*add_fn)(unsigned num, struct argp_state *state),
int (*default_fn)(struct argp_state *state),
int (*lookup_fn)(const char *str, struct argp_state *state),
const char *type_name, struct argp_state *state)
{
error_t default_num_add(struct argp_state *state)
{
return (*add_fn)((*default_fn)(state), state);
}
error_t add_num_str(const char *str, struct argp_state *state)
{
const char *p;
for (p = str; *p != '\0'; p++)
if (!isdigit(*p))
{
if (lookup_fn)
return (*add_fn)((*lookup_fn)(str, state), state);
else
{
argp_error (state, "%s: Invalid %s", p, type_name);
return EINVAL;
}
return 0;
}
return (*add_fn) (atoi (str), state);
}
return _parse_strlist(arg, add_num_str, default_fn ? default_num_add : 0,
type_name, state);
}
int
parse_enum (const char *arg,
const char *(*choice_fn)(unsigned n),
const char *kind, int allow_mismatches,
struct argp_state *state)
{
const char *choice;
int arglen = strlen (arg);
int n = 0;
int partial_match = -1;
while ((choice = (*choice_fn)(n)) != NULL)
if (strcasecmp (choice, arg) == 0)
return n;
else
{
if (strncasecmp (choice, arg, arglen) == 0)
{
if (partial_match >= 0)
{
argp_error (state, "%s: Ambiguous %s", arg, kind);
return -1;
}
else
partial_match = n;
}
n++;
}
if (partial_match < 0 && !allow_mismatches)
argp_error (state, "%s: Invalid %s", arg, kind);
return partial_match;
}