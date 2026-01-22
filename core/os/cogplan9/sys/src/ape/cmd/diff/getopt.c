#ifndef _NO_PROTO
#define _NO_PROTO
#endif
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#ifndef __STDC__
#ifndef const
#define const
#endif
#endif
#include <stdio.h>
#if defined (_LIBC) || !defined (__GNU_LIBRARY__)
#ifdef	__GNU_LIBRARY__
#include <stdlib.h>
#endif
#include "getopt.h"
char *optarg = NULL;
int optind = 0;
static char *nextchar;
int opterr = 1;
int optopt = '?';
static enum
{
REQUIRE_ORDER, PERMUTE, RETURN_IN_ORDER
} ordering;
static char *posixly_correct;
#ifdef	__GNU_LIBRARY__
#include <string.h>
#define	my_index	strchr
#else
char *getenv ();
static char *
my_index (str, chr)
const char *str;
int chr;
{
while (*str)
{
if (*str == chr)
return (char *) str;
str++;
}
return 0;
}
#ifdef __GNUC__
#ifndef __STDC__
extern int strlen (const char *);
#endif
#endif
#endif
static int first_nonopt;
static int last_nonopt;
static void
exchange (argv)
char **argv;
{
int bottom = first_nonopt;
int middle = last_nonopt;
int top = optind;
char *tem;
while (top > middle && middle > bottom)
{
if (top - middle > middle - bottom)
{
int len = middle - bottom;
register int i;
for (i = 0; i < len; i++)
{
tem = argv[bottom + i];
argv[bottom + i] = argv[top - (middle - bottom) + i];
argv[top - (middle - bottom) + i] = tem;
}
top -= len;
}
else
{
int len = top - middle;
register int i;
for (i = 0; i < len; i++)
{
tem = argv[bottom + i];
argv[bottom + i] = argv[middle + i];
argv[middle + i] = tem;
}
bottom += len;
}
}
first_nonopt += (optind - last_nonopt);
last_nonopt = optind;
}
static const char *
_getopt_initialize (optstring)
const char *optstring;
{
first_nonopt = last_nonopt = optind = 1;
nextchar = NULL;
posixly_correct = getenv ("POSIXLY_CORRECT");
if (optstring[0] == '-')
{
ordering = RETURN_IN_ORDER;
++optstring;
}
else if (optstring[0] == '+')
{
ordering = REQUIRE_ORDER;
++optstring;
}
else if (posixly_correct != NULL)
ordering = REQUIRE_ORDER;
else
ordering = PERMUTE;
return optstring;
}
int
_getopt_internal (argc, argv, optstring, longopts, longind, long_only)
int argc;
char *const *argv;
const char *optstring;
const struct option *longopts;
int *longind;
int long_only;
{
optarg = NULL;
if (optind == 0)
optstring = _getopt_initialize (optstring);
if (nextchar == NULL || *nextchar == '\0')
{
if (ordering == PERMUTE)
{
if (first_nonopt != last_nonopt && last_nonopt != optind)
exchange ((char **) argv);
else if (last_nonopt != optind)
first_nonopt = optind;
while (optind < argc
&& (argv[optind][0] != '-' || argv[optind][1] == '\0'))
optind++;
last_nonopt = optind;
}
if (optind != argc && !strcmp (argv[optind], "--"))
{
optind++;
if (first_nonopt != last_nonopt && last_nonopt != optind)
exchange ((char **) argv);
else if (first_nonopt == last_nonopt)
first_nonopt = optind;
last_nonopt = argc;
optind = argc;
}
if (optind == argc)
{
if (first_nonopt != last_nonopt)
optind = first_nonopt;
return EOF;
}
if ((argv[optind][0] != '-' || argv[optind][1] == '\0'))
{
if (ordering == REQUIRE_ORDER)
return EOF;
optarg = argv[optind++];
return 1;
}
nextchar = (argv[optind] + 1
+ (longopts != NULL && argv[optind][1] == '-'));
}
if (longopts != NULL
&& (argv[optind][1] == '-'
|| (long_only && (argv[optind][2] || !my_index (optstring, argv[optind][1])))))
{
char *nameend;
const struct option *p;
const struct option *pfound = NULL;
int exact = 0;
int ambig = 0;
int indfound;
int option_index;
for (nameend = nextchar; *nameend && *nameend != '='; nameend++)
;
for (p = longopts, option_index = 0; p->name; p++, option_index++)
if (!strncmp (p->name, nextchar, nameend - nextchar))
{
if (nameend - nextchar == strlen (p->name))
{
pfound = p;
indfound = option_index;
exact = 1;
break;
}
else if (pfound == NULL)
{
pfound = p;
indfound = option_index;
}
else
ambig = 1;
}
if (ambig && !exact)
{
if (opterr)
fprintf (stderr, "%s: option `%s' is ambiguous\n",
argv[0], argv[optind]);
nextchar += strlen (nextchar);
optind++;
return '?';
}
if (pfound != NULL)
{
option_index = indfound;
optind++;
if (*nameend)
{
if (pfound->has_arg)
optarg = nameend + 1;
else
{
if (opterr)
{
if (argv[optind - 1][1] == '-')
fprintf (stderr,
"%s: option `--%s' doesn't allow an argument\n",
argv[0], pfound->name);
else
fprintf (stderr,
"%s: option `%c%s' doesn't allow an argument\n",
argv[0], argv[optind - 1][0], pfound->name);
}
nextchar += strlen (nextchar);
return '?';
}
}
else if (pfound->has_arg == 1)
{
if (optind < argc)
optarg = argv[optind++];
else
{
if (opterr)
fprintf (stderr, "%s: option `%s' requires an argument\n",
argv[0], argv[optind - 1]);
nextchar += strlen (nextchar);
return optstring[0] == ':' ? ':' : '?';
}
}
nextchar += strlen (nextchar);
if (longind != NULL)
*longind = option_index;
if (pfound->flag)
{
*(pfound->flag) = pfound->val;
return 0;
}
return pfound->val;
}
if (!long_only || argv[optind][1] == '-'
|| my_index (optstring, *nextchar) == NULL)
{
if (opterr)
{
if (argv[optind][1] == '-')
fprintf (stderr, "%s: unrecognized option `--%s'\n",
argv[0], nextchar);
else
fprintf (stderr, "%s: unrecognized option `%c%s'\n",
argv[0], argv[optind][0], nextchar);
}
nextchar = (char *) "";
optind++;
return '?';
}
}
{
char c = *nextchar++;
char *temp = my_index (optstring, c);
if (*nextchar == '\0')
++optind;
if (temp == NULL || c == ':')
{
if (opterr)
{
if (posixly_correct)
fprintf (stderr, "%s: illegal option -- %c\n", argv[0], c);
else
fprintf (stderr, "%s: invalid option -- %c\n", argv[0], c);
}
optopt = c;
return '?';
}
if (temp[1] == ':')
{
if (temp[2] == ':')
{
if (*nextchar != '\0')
{
optarg = nextchar;
optind++;
}
else
optarg = NULL;
nextchar = NULL;
}
else
{
if (*nextchar != '\0')
{
optarg = nextchar;
optind++;
}
else if (optind == argc)
{
if (opterr)
{
fprintf (stderr, "%s: option requires an argument -- %c\n",
argv[0], c);
}
optopt = c;
if (optstring[0] == ':')
c = ':';
else
c = '?';
}
else
optarg = argv[optind++];
nextchar = NULL;
}
}
return c;
}
}
int
getopt (argc, argv, optstring)
int argc;
char *const *argv;
const char *optstring;
{
return _getopt_internal (argc, argv, optstring,
(const struct option *) 0,
(int *) 0,
0);
}
#endif
#ifdef TEST
int
main (argc, argv)
int argc;
char **argv;
{
int c;
int digit_optind = 0;
while (1)
{
int this_option_optind = optind ? optind : 1;
c = getopt (argc, argv, "abc:d:0123456789");
if (c == EOF)
break;
switch (c)
{
case '0':
case '1':
case '2':
case '3':
case '4':
case '5':
case '6':
case '7':
case '8':
case '9':
if (digit_optind != 0 && digit_optind != this_option_optind)
printf ("digits occur in two different argv-elements.\n");
digit_optind = this_option_optind;
printf ("option %c\n", c);
break;
case 'a':
printf ("option a\n");
break;
case 'b':
printf ("option b\n");
break;
case 'c':
printf ("option c with value `%s'\n", optarg);
break;
case '?':
break;
default:
printf ("?? getopt returned character code 0%o ??\n", c);
}
}
if (optind < argc)
{
printf ("non-option ARGV-elements: ");
while (optind < argc)
printf ("%s ", argv[optind++]);
printf ("\n");
}
exit (0);
}
#endif