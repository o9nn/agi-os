#if has_conf_h
# include <conf.h>
#else
# if HAVE_CONFIG_H
#  include <config.h>
# else
#  ifndef __STDC__
#   define const
#  endif
# endif
# if HAVE_LIMITS_H
#  include <limits.h>
# endif
# ifndef LONG_MIN
# define LONG_MIN (-1-2147483647L)
# endif
# if STDC_HEADERS
#  include <stdlib.h>
# endif
# include <time.h>
# ifdef __STDC__
#  define P(x) x
# else
#  define P(x) ()
# endif
#endif
#include <ctype.h>
#if STDC_HEADERS
# define CTYPE_DOMAIN(c) 1
#else
# define CTYPE_DOMAIN(c) ((unsigned) (c) <= 0177)
#endif
#define ISALNUM(c)	(CTYPE_DOMAIN (c) && isalnum (c))
#define ISALPHA(c)	(CTYPE_DOMAIN (c) && isalpha (c))
#define ISSPACE(c)	(CTYPE_DOMAIN (c) && isspace (c))
#define ISUPPER(c)	(CTYPE_DOMAIN (c) && isupper (c))
#define ISDIGIT(c)	((unsigned) (c) - '0' <= 9)
#include <partime.h>
char const partimeId[] =
"$Id: partime.c,v 5.16 1997/05/19 06:33:53 eggert Exp $";
#define NAME_LENGTH_MAXIMUM 4
struct name_val
{
char name[NAME_LENGTH_MAXIMUM];
int val;
};
static char const *parse_decimal P ((char const *, int, int, int, int, int *, int *));
static char const *parse_fixed P ((char const *, int, int *));
static char const *parse_pattern_letter P ((char const *, int, struct partime *));
static char const *parse_prefix P ((char const *, struct partime *, int *));
static char const *parse_ranged P ((char const *, int, int, int, int *));
static int lookup P ((char const *, struct name_val const[]));
static int merge_partime P ((struct partime *, struct partime const *));
static void undefine P ((struct partime *));
static struct name_val const month_names[] =
{
{"jan", 0},
{"feb", 1},
{"mar", 2},
{"apr", 3},
{"may", 4},
{"jun", 5},
{"jul", 6},
{"aug", 7},
{"sep", 8},
{"oct", 9},
{"nov", 10},
{"dec", 11},
{"", TM_UNDEFINED}
};
static struct name_val const weekday_names[] =
{
{"sun", 0},
{"mon", 1},
{"tue", 2},
{"wed", 3},
{"thu", 4},
{"fri", 5},
{"sat", 6},
{"", TM_UNDEFINED}
};
#define hr60nonnegative(t) ((t)/100 * 60  +  (t)%100)
#define hr60(t) ((t)<0 ? -hr60nonnegative(-(t)) : hr60nonnegative(t))
#define zs(t,s) {s, hr60(t)}
#define zd(t,s,d) zs(t, s),  zs((t)+100, d)
static struct name_val const zone_names[] =
{
zs (-1000, "hst"),
zd (-1000, "hast", "hadt"),
zd (- 900, "akst", "akdt"),
zd (- 800, "pst" , "pdt" ),
zd (- 700, "mst" , "mdt" ),
zd (- 600, "cst" , "cdt" ),
zd (- 500, "est" , "edt" ),
zd (- 400, "ast" , "adt" ),
zd (- 330, "nst" , "ndt" ),
zs (  000, "utc" ),
zs (  000, "uct" ),
zs (  000, "cut" ),
zs (  000, "ut"),
zs (  000, "z"),
zd (  000, "gmt" , "bst" ),
zd (  000, "wet" , "west"),
zd (  100, "cet" , "cest"),
zd (  100, "met" , "mest"),
zd (  100, "mez" , "mesz"),
zd (  200, "eet" , "eest"),
zs (  530, "ist" ),
zd (  900, "jst" , "jdt" ),
zd (  900, "kst" , "kdt" ),
zd ( 1200, "nzst", "nzdt"),
{"lt", 1},
#if 0
zs (-1100, "sst" ),
zd (- 900, "yst" , "ydt" ),
zd (- 500, "ast" , "adt" ),
zd (- 400, "wst" , "wdt" ),
zd (- 400, "cst" , "cdt" ),
zd (- 200, "fst" , "fdt" ),
zs (  000, "wat" ),
zs (  100, "cat" ),
zs (  200, "sat" ),
zd (  200, "ist" , "idt" ),
zs (  300, "eat" ),
zd (  300, "msk" , "msd" ),
zd (  330, "ist" , "idt" ),
zs (  800, "hkt" ),
zs (  800, "sgt" ),
zd (  800, "cst" , "cdt" ),
zd (  800, "wst" , "wst" ),
zd (  930, "cst" , "cst" ),
zs ( 1000, "gst" ),
zd ( 1000, "est" , "est" ),
#endif
{"", -1}
};
static int
lookup (s, table)
char const *s;
struct name_val const table[];
{
int j;
char buf[NAME_LENGTH_MAXIMUM];
for (j = 0; j < NAME_LENGTH_MAXIMUM; j++)
{
unsigned char c = *s++;
if (! ISALPHA (c))
{
buf[j] = '\0';
break;
}
buf[j] = ISUPPER (c) ? tolower (c) : c;
}
for (;; table++)
for (j = 0; ; j++)
if (j == NAME_LENGTH_MAXIMUM  ||  ! table[0].name[j])
return table[0].val;
else if (buf[j] != table[0].name[j])
break;
}
static void
undefine (t)
struct partime *t;
{
t->tm.tm_sec = t->tm.tm_min = t->tm.tm_hour = t->tm.tm_mday = t->tm.tm_mon
= t->tm.tm_year = t->tm.tm_wday = t->tm.tm_yday
= t->ymodulus = t->yweek
= TM_UNDEFINED;
t->zone = TM_UNDEFINED_ZONE;
}
static char const *const patterns[] =
{
"E_n_y", "x",
"E_n", "n_E", "n", "t:m:s_A", "t:m_A", "t_A",
"y/N/D$",
"y-N-D$", "4ND$", "Y-N$",
"RND$", "-R=N$", "-R$", "--N=D$", "N=DT",
"--N$", "---D$", "DT",
"Y-d$", "4d$", "R=d$", "-d$", "dT",
"y-W-X", "yWX", "y=W",
"-r-W-X", "r-W-XT", "-rWX", "rWXT", "-W=X", "W=XT", "-W",
"-w-X", "w-XT", "---X$", "XT", "4$",
"T",
"h:m:s$", "hms$", "h:m$", "hm$", "h$", "-m:s$", "-ms$", "-m$", "--s$",
"Y", "Z",
0
};
static char const *
parse_prefix (str, t, pi)
char const *str;
struct partime *t;
int *pi;
{
int i = *pi;
char const *pat;
unsigned char c;
if (i < 0)
return 0;
while (! ISALNUM (c = *str) && c != '-' && c != '+')
{
if (! c)
{
undefine (t);
*pi = -1;
return str;
}
str++;
}
while ((pat = patterns[i++]) != 0)
{
char const *s = str;
undefine (t);
do
{
if (! (c = *pat++))
{
*pi = i;
return s;
}
}
while ((s = parse_pattern_letter (s, c, t)) != 0);
}
return 0;
}
static char const *
parse_fixed (s, digits, res)
char const *s;
int digits, *res;
{
int n = 0;
char const *lim = s + digits;
while (s < lim)
{
unsigned d = *s++ - '0';
if (9 < d)
return 0;
n = 10 * n + d;
}
*res = n;
return s;
}
static char const *
parse_ranged (s, digits, lo, hi, res)
char const *s;
int digits, lo, hi, *res;
{
s = parse_fixed (s, digits, res);
return s && lo <= *res && *res <= hi ? s : 0;
}
static char const *
parse_decimal (s, digits, lo, hi, resolution, res, fres)
char const *s;
int digits, lo, hi, resolution, *res, *fres;
{
s = parse_fixed (s, digits, res);
if (s && lo <= *res && *res <= hi)
{
int f = 0;
if ((s[0] == ',' || s[0] == '.') && ISDIGIT (s[1]))
{
char const *s1 = ++s;
int num10 = 0, denom10 = 10, product;
while (ISDIGIT (*++s))
{
int d = denom10 * 10;
if (d / 10  !=  denom10)
return 0;
denom10 = d;
}
s = parse_fixed (s1, (int) (s - s1), &num10);
product = num10 * resolution;
f = (product + (denom10 >> 1)) / denom10;
f -= f & (product % denom10  ==  denom10 >> 1);
if (f < 0  ||  product/resolution != num10)
return 0;
}
*fres = f;
return s;
}
return 0;
}
char *
parzone (s, zone)
char const *s;
long *zone;
{
char sign;
int hh, mm, ss;
int minutesEastOfUTC;
long offset, z;
switch (*s)
{
case '-':
case '+':
z = 0;
break;
default:
minutesEastOfUTC = lookup (s, zone_names);
if (minutesEastOfUTC == -1)
return 0;
while (ISALPHA ((unsigned char) *s))
s++;
if (minutesEastOfUTC == 1)
{
*zone = TM_LOCAL_ZONE;
return (char *) s;
}
z = minutesEastOfUTC * 60L;
if ((s[-1] == 'T' || s[-1] == 't')
&& (s[-2] == 'S' || s[-2] == 's')
&& (s[-3] == 'D' || s[-3] == 'd'))
goto trailing_dst;
while (ISSPACE ((unsigned char) *s))
s++;
if ((s[0] == 'D' || s[0] == 'd')
&& (s[1] == 'S' || s[1] == 's')
&& (s[2] == 'T' || s[2] == 't'))
{
s += 3;
trailing_dst:
*zone = z + 60*60;
return (char *) s;
}
switch (*s)
{
case '-':
case '+':
break;
default:
*zone = z;
return (char *) s;
}
break;
}
sign = *s++;
if (! (s = parse_ranged (s, 2, 0, 23, &hh)))
return 0;
mm = ss = 0;
if (*s == ':')
s++;
if (ISDIGIT (*s))
{
if (! (s = parse_ranged (s, 2, 0, 59, &mm)))
return 0;
if (*s == ':' && s[-3] == ':' && ISDIGIT (s[1])
&& ! (s = parse_ranged (s + 1, 2, 0, 59, &ss)))
return 0;
}
if (ISDIGIT (*s))
return 0;
offset = (hh * 60 + mm) * 60L + ss;
*zone = z + (sign == '-' ? -offset : offset);
return (char *) s;
}
static char const *
parse_pattern_letter (s, c, t)
char const *s;
int c;
struct partime *t;
{
switch (c)
{
case '$':
if (ISDIGIT (*s))
return 0;
break;
case '-':
case '/':
case ':':
if (*s++ != c)
return 0;
break;
case '4':
s = parse_fixed (s, 4, &t->tm.tm_year);
break;
case '=':
s += *s == '-';
break;
case 'A':
switch (*s++)
{
case 'A':
case 'a':
if (t->tm.tm_hour == 12)
t->tm.tm_hour = 0;
break;
case 'P':
case 'p':
if (t->tm.tm_hour != 12)
t->tm.tm_hour += 12;
break;
default:
return 0;
}
switch (*s)
{
case 'M':
case 'm':
s++;
break;
}
if (ISALNUM ((unsigned char) *s))
return 0;
break;
case 'D':
s = parse_ranged (s, 2, 1, 31, &t->tm.tm_mday);
break;
case 'd':
s = parse_ranged (s, 3, 1, 366, &t->tm.tm_yday);
t->tm.tm_yday--;
break;
case 'E':
s = parse_ranged (s, (ISDIGIT (s[0]) && ISDIGIT (s[1])) + 1, 1, 31,
&t->tm.tm_mday);
break;
case 'h':
{
int frac;
s = parse_decimal (s, 2, 0, 23, 60 * 60, &t->tm.tm_hour, &frac);
t->tm.tm_min = frac / 60;
t->tm.tm_sec = frac % 60;
}
break;
case 'm':
s = parse_decimal (s, 2, 0, 59, 60, &t->tm.tm_min, &t->tm.tm_sec);
break;
case 'n':
if (! TM_DEFINED (t->tm.tm_mon = lookup (s, month_names)))
return 0;
while (ISALPHA ((unsigned char) *s))
s++;
break;
case 'N':
s = parse_ranged (s, 2, 1, 12, &t->tm.tm_mon);
t->tm.tm_mon--;
break;
case 'r':
s = parse_fixed (s, 1, &t->tm.tm_year);
t->ymodulus = 10;
break;
case_R:
case 'R':
s = parse_fixed (s, 2, &t->tm.tm_year);
t->ymodulus = 100;
break;
case 's':
{
int frac;
s = parse_decimal (s, 2, 0, 60, 1, &t->tm.tm_sec, &frac);
t->tm.tm_sec += frac;
}
break;
case 'T':
switch (*s++)
{
case 'T':
case 't':
break;
default:
return 0;
}
break;
case 't':
s = parse_ranged (s, (ISDIGIT (s[0]) && ISDIGIT (s[1])) + 1, 1, 12,
&t->tm.tm_hour);
break;
case 'w':
switch (*s++)
{
case 'W':
case 'w':
break;
default:
return 0;
}
break;
case 'W':
switch (*s++)
{
case 'W':
case 'w':
break;
default:
return 0;
}
s = parse_ranged (s, 2, 0, 53, &t->yweek);
break;
case 'X':
s = parse_ranged (s, 1, 1, 7, &t->tm.tm_wday);
t->tm.tm_wday--;
break;
case 'x':
if (! TM_DEFINED (t->tm.tm_wday = lookup (s, weekday_names)))
return 0;
while (ISALPHA ((unsigned char) *s))
s++;
break;
case 'y':
if (ISDIGIT (s[0]) && ISDIGIT (s[1]) && ! ISDIGIT (s[2]))
goto case_R;
case 'Y':
{
int len = 0;
while (ISDIGIT (s[len]))
len++;
if (len < 4)
return 0;
s = parse_fixed (s, len, &t->tm.tm_year);
}
break;
case 'Z':
s = parzone (s, &t->zone);
break;
case '_':
while (! ISALNUM ((unsigned char) *s) && *s)
s++;
break;
default:
return 0;
}
return s;
}
static int
merge_partime (t, u)
struct partime *t;
struct partime const *u;
{
# define conflict(a,b) ((a) != (b)  &&  TM_DEFINED (a)  &&  TM_DEFINED (b))
if (conflict (t->tm.tm_sec, u->tm.tm_sec)
|| conflict (t->tm.tm_min, u->tm.tm_min)
|| conflict (t->tm.tm_hour, u->tm.tm_hour)
|| conflict (t->tm.tm_mday, u->tm.tm_mday)
|| conflict (t->tm.tm_mon, u->tm.tm_mon)
|| conflict (t->tm.tm_year, u->tm.tm_year)
|| conflict (t->tm.tm_wday, u->tm.tm_yday)
|| conflict (t->ymodulus, u->ymodulus)
|| conflict (t->yweek, u->yweek)
|| (t->zone != u->zone
&& t->zone != TM_UNDEFINED_ZONE
&& u->zone != TM_UNDEFINED_ZONE))
return -1;
# undef conflict
# define merge_(a,b) if (TM_DEFINED (b)) (a) = (b);
merge_ (t->tm.tm_sec, u->tm.tm_sec)
merge_ (t->tm.tm_min, u->tm.tm_min)
merge_ (t->tm.tm_hour, u->tm.tm_hour)
merge_ (t->tm.tm_mday, u->tm.tm_mday)
merge_ (t->tm.tm_mon, u->tm.tm_mon)
merge_ (t->tm.tm_year, u->tm.tm_year)
merge_ (t->tm.tm_wday, u->tm.tm_yday)
merge_ (t->ymodulus, u->ymodulus)
merge_ (t->yweek, u->yweek)
# undef merge_
if (u->zone != TM_UNDEFINED_ZONE)
t->zone = u->zone;
return 0;
}
char *
partime (s, t)
char const *s;
struct partime *t;
{
struct partime p;
undefine (t);
while (*s)
{
int i = 0;
char const *s1;
do
{
if (! (s1 = parse_prefix (s, &p, &i)))
return (char *) s;
}
while (merge_partime (t, &p) != 0);
s = s1;
}
return (char *) s;
}