#include <u.h>
#include <libc.h>
#include <ctype.h>
typedef ulong Time;
enum {
AM, PM, HR24,
Month = 1,
Year,
Day,
Timetok,
Tz,
Dtz,
Ignore,
Ampm,
Maxtok		= 6,
Maxdateflds	= 25,
};
#define TOVAL(tp, v)	((tp)->value = (v) / 10)
#define FROMVAL(tp)	((tp)->value * 10)
typedef struct {
char	token[Maxtok];
char	type;
schar	value;
} Datetok;
int dtok_numparsed;
Datetok	*datetoktype(char *s, int *bigvalp);
static Datetok datetktbl[];
static unsigned szdatetktbl;
static int
eatnum(char **cpp)
{
int c, x;
char *cp;
cp = *cpp;
c = *cp;
if (!isascii(c) || !isdigit(c))
return -1;
x = c - '0';
c = *++cp;
if (isascii(c) && isdigit(c)) {
x = 10*x + c - '0';
cp++;
}
*cpp = cp;
return x;
}
int
parsetime(char *time, Tm *tm)
{
tm->hour = eatnum(&time);
if (tm->hour == -1 || *time++ != ':')
return -1;
tm->min = eatnum(&time);
if (tm->min == -1)
return -1;
if (*time++ != ':') {
tm->sec = 0;
return 0;
}
tm->sec = eatnum(&time);
if (tm->sec == -1)
return -1;
return *time == '\0' || isascii(*time) && isspace(*time)? 0: -1;
}
int
tryabsdate(char **fields, int nf, Tm *now, Tm *tm)
{
int i, mer = HR24, bigval = -1;
long flg = 0, ty;
char *p;
Datetok *tp;
now = localtime(time(0));
tm->tzoff = now->tzoff;
strncpy(tm->zone, now->zone, sizeof tm->zone - 1);
tm->zone[sizeof tm->zone - 1] = '\0';
tm->mday = tm->mon = tm->year = -1;
tm->hour = tm->min = tm->sec = 0;
dtok_numparsed = 0;
for (i = 0; i < nf; i++) {
if (fields[i][0] == '\0')
continue;
tp = datetoktype(fields[i], &bigval);
ty = (1L << tp->type) & ~(1L << Ignore);
if (flg & ty)
return -1;
flg |= ty;
switch (tp->type) {
case Year:
tm->year = bigval;
if (tm->year < 1970 || tm->year > 2106)
return -1;
if (tm->year >= 1900)
tm->year -= 1900;
break;
case Day:
tm->mday = bigval;
break;
case Month:
tm->mon = tp->value - 1;
break;
case Timetok:
if (parsetime(fields[i], tm) < 0)
return -1;
break;
case Dtz:
case Tz:
tm->tzoff = FROMVAL(tp);
strncpy(tm->zone, fields[i], sizeof tm->zone - 1);
tm->zone[sizeof tm->zone - 1] = '\0';
for (p = tm->zone; *p; p++)
if (isascii(*p) && islower(*p))
*p = toupper(*p);
break;
case Ignore:
break;
case Ampm:
mer = tp->value;
break;
default:
return -1;
}
}
if (tm->year == -1 || tm->mon == -1 || tm->mday == -1)
return -1;
if (mer == PM)
tm->hour += 12;
return 0;
}
int
prsabsdate(char *timestr, Tm *now, Tm *tm)
{
int nf;
char *fields[Maxdateflds];
static char delims[] = "- \t\n/,";
nf = gettokens(timestr, fields, nelem(fields), delims+1);
if (nf > nelem(fields))
return -1;
if (tryabsdate(fields, nf, now, tm) < 0) {
char *p = timestr;
while (--nf > 0) {
while (*p++ != '\0')
;
p[-1] = ' ';
}
nf = gettokens(timestr, fields, nelem(fields), delims);
if (nf > nelem(fields) || tryabsdate(fields, nf, now, tm) < 0)
return -1;
}
return 0;
}
int
validtm(Tm *tm)
{
if (tm->year < 0 || tm->mon < 0 || tm->mon > 11 ||
tm->mday < 1 || tm->hour < 0 || tm->hour >= 24 ||
tm->min < 0 || tm->min > 59 ||
tm->sec < 0 || tm->sec > 61)
return 0;
return 1;
}
Time
seconds(char *timestr)
{
Tm date;
memset(&date, 0, sizeof date);
if (prsabsdate(timestr, localtime(time(0)), &date) < 0)
return -1;
return validtm(&date)? tm2sec(&date): -1;
}
int
convert(char *timestr)
{
char *copy;
Time tstime;
copy = strdup(timestr);
if (copy == nil)
sysfatal("out of memory");
tstime = seconds(copy);
free(copy);
if (tstime == -1) {
fprint(2, "%s: `%s' not a valid date\n", argv0, timestr);
return 1;
}
print("%lud\n", tstime);
return 0;
}
static void
usage(void)
{
fprint(2, "usage: %s date-time ...\n", argv0);
exits("usage");
}
void
main(int argc, char **argv)
{
int i, sts;
sts = 0;
ARGBEGIN{
default:
usage();
}ARGEND
if (argc == 0)
usage();
for (i = 0; i < argc; i++)
sts |= convert(argv[i]);
exits(sts != 0? "bad": 0);
}
Datetok *
datebsearch(char *key, Datetok *base, unsigned nel)
{
int cmp;
Datetok *last = base + nel - 1, *pos;
while (last >= base) {
pos = base + ((last - base) >> 1);
cmp = key[0] - pos->token[0];
if (cmp == 0) {
cmp = strncmp(key, pos->token, Maxtok);
if (cmp == 0)
return pos;
}
if (cmp < 0)
last = pos - 1;
else
base = pos + 1;
}
return 0;
}
Datetok *
datetoktype(char *s, int *bigvalp)
{
char *cp = s;
char c = *cp;
static Datetok t;
Datetok *tp = &t;
if (isascii(c) && isdigit(c)) {
int len = strlen(cp);
if (len > 3 && (cp[1] == ':' || cp[2] == ':'))
tp->type = Timetok;
else {
if (bigvalp != nil)
*bigvalp = atoi(cp);
if (len == 4)
tp->type = Year;
else if (++dtok_numparsed == 1)
tp->type = Day;
else
tp->type = Year;
}
} else if (c == '-' || c == '+') {
int val = atoi(cp + 1);
int hr =  val / 100;
int min = val % 100;
val = hr*60 + min;
TOVAL(tp, c == '-'? -val: val);
tp->type = Tz;
} else {
char lowtoken[Maxtok+1];
char *ltp = lowtoken, *endltp = lowtoken+Maxtok;
while ((c = *cp++) != '\0' && ltp < endltp)
*ltp++ = (isascii(c) && isupper(c)? tolower(c): c);
*ltp = '\0';
tp = datebsearch(lowtoken, datetktbl, szdatetktbl);
if (tp == nil) {
tp = &t;
tp->type = Ignore;
}
}
return tp;
}
static Datetok datetktbl[] = {
"acsst",	Dtz,	63,
"acst",		Tz,	57,
"adt",		Dtz,	-18,
"aesst",	Dtz,	66,
"aest",		Tz,	60,
"ahst",		Tz,	60,
"am",		Ampm,	AM,
"apr",		Month,	4,
"april",	Month,	4,
"ast",		Tz,	-24,
"at",		Ignore,	0,
"aug",		Month,	8,
"august",	Month,	8,
"awsst",	Dtz,	54,
"awst",		Tz,	48,
"bst",		Tz,	6,
"bt",		Tz,	18,
"cadt",		Dtz,	63,
"cast",		Tz,	57,
"cat",		Tz,	-60,
"cct",		Tz,	48,
"cdt",		Dtz,	-30,
"cet",		Tz,	6,
"cetdst",	Dtz,	12,
"cst",		Tz,	-36,
"dec",		Month,	12,
"decemb",	Month,	12,
"dnt",		Tz,	6,
"dst",		Ignore,	0,
"east",		Tz,	-60,
"edt",		Dtz,	-24,
"eet",		Tz,	12,
"eetdst",	Dtz,	18,
"est",		Tz,	-30,
"feb",		Month,	2,
"februa",	Month,	2,
"fri",		Ignore,	5,
"friday",	Ignore,	5,
"fst",		Tz,	6,
"fwt",		Dtz,	12,
"gmt",		Tz,	0,
"gst",		Tz,	60,
"hdt",		Dtz,	-54,
"hmt",		Dtz,	18,
"hst",		Tz,	-60,
"idle",		Tz,	72,
"idlw",		Tz,	-72,
"ist",		Tz,	12,
"it",		Tz,	22,
"jan",		Month,	1,
"januar",	Month,	1,
"jst",		Tz,	54,
"jt",		Tz,	45,
"jul",		Month,	7,
"july",		Month,	7,
"jun",		Month,	6,
"june",		Month,	6,
"kst",		Tz,	54,
"ligt",		Tz,	60,
"mar",		Month,	3,
"march",	Month,	3,
"may",		Month,	5,
"mdt",		Dtz,	-36,
"mest",		Dtz,	12,
"met",		Tz,	6,
"metdst",	Dtz,	12,
"mewt",		Tz,	6,
"mez",		Tz,	6,
"mon",		Ignore,	1,
"monday",	Ignore,	1,
"mst",		Tz,	-42,
"mt",		Tz,	51,
"ndt",		Dtz,	-15,
"nft",		Tz,	-21,
"nor",		Tz,	6,
"nov",		Month,	11,
"novemb",	Month,	11,
"nst",		Tz,	-21,
"nt",		Tz,	-66,
"nzdt",		Dtz,	78,
"nzst",		Tz,	72,
"nzt",		Tz,	72,
"oct",		Month,	10,
"octobe",	Month,	10,
"on",		Ignore,	0,
"pdt",		Dtz,	-42,
"pm",		Ampm,	PM,
"pst",		Tz,	-48,
"sadt",		Dtz,	63,
"sast",		Tz,	57,
"sat",		Ignore,	6,
"saturd",	Ignore,	6,
"sep",		Month,	9,
"sept",		Month,	9,
"septem",	Month,	9,
"set",		Tz,	-6,
"sst",		Dtz,	12,
"sun",		Ignore,	0,
"sunday",	Ignore,	0,
"swt",		Tz,	6,
"thu",		Ignore,	4,
"thur",		Ignore,	4,
"thurs",	Ignore,	4,
"thursd",	Ignore,	4,
"tue",		Ignore,	2,
"tues",		Ignore,	2,
"tuesda",	Ignore,	2,
"ut",		Tz,	0,
"utc",		Tz,	0,
"wadt",		Dtz,	48,
"wast",		Tz,	42,
"wat",		Tz,	-6,
"wdt",		Dtz,	54,
"wed",		Ignore,	3,
"wednes",	Ignore,	3,
"weds",		Ignore,	3,
"wet",		Tz,	0,
"wetdst",	Dtz,	6,
"wst",		Tz,	48,
"ydt",		Dtz,	-48,
"yst",		Tz,	-54,
"zp4",		Tz,	-24,
"zp5",		Tz,	-30,
"zp6",		Tz,	-36,
};
static unsigned szdatetktbl = nelem(datetktbl);