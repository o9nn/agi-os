#include "sh.h"
#include "ksh_stat.h"
#ifdef S_ISLNK
static char	*do_phys_path ARGS((XString *xsp, char *xp, const char *path));
#endif
int
make_path(cwd, file, cdpathp, xsp, phys_pathp)
const char *cwd;
const char *file;
char	**cdpathp;
XString	*xsp;
int	*phys_pathp;
{
int	rval = 0;
int	use_cdpath = 1;
char	*plist;
int	len;
int	plen = 0;
char	*xp = Xstring(*xsp, xp);
if (!file)
file = null;
if (!ISRELPATH(file)) {
*phys_pathp = 0;
use_cdpath = 0;
} else {
if (file[0] == '.') {
char c = file[1];
if (c == '.')
c = file[2];
if (ISDIRSEP(c) || c == '\0')
use_cdpath = 0;
}
plist = *cdpathp;
if (!plist)
use_cdpath = 0;
else if (use_cdpath) {
char *pend;
for (pend = plist; *pend && *pend != PATHSEP; pend++)
;
plen = pend - plist;
*cdpathp = *pend ? ++pend : (char *) 0;
}
if ((use_cdpath == 0 || !plen || ISRELPATH(plist))
&& (cwd && *cwd))
{
len = strlen(cwd);
XcheckN(*xsp, xp, len);
memcpy(xp, cwd, len);
xp += len;
if (!ISDIRSEP(cwd[len - 1]))
Xput(*xsp, xp, DIRSEP);
}
*phys_pathp = Xlength(*xsp, xp);
if (use_cdpath && plen) {
XcheckN(*xsp, xp, plen);
memcpy(xp, plist, plen);
xp += plen;
if (!ISDIRSEP(plist[plen - 1]))
Xput(*xsp, xp, DIRSEP);
rval = 1;
}
}
len = strlen(file) + 1;
XcheckN(*xsp, xp, len);
memcpy(xp, file, len);
if (!use_cdpath)
*cdpathp = (char *) 0;
return rval;
}
void
simplify_path(path)
char	*path;
{
char	*cur;
char	*t;
int	isrooted;
char	*very_start = path;
char	*start;
if (!*path)
return;
if ((isrooted = ISROOTEDPATH(path)))
very_start++;
#if defined (OS2) || defined (__CYGWIN__)
if (path[0] && path[1] == ':')
very_start += 2;
#endif
#ifdef __CYGWIN__
if (path[0] && ISDIRSEP(path[0]) && path[1] && ISDIRSEP(path[1]))
very_start++;
#endif
for (cur = t = start = very_start; ; ) {
while (ISDIRSEP(*t))
t++;
if (*t == '\0') {
if (cur == path)
*cur++ = '.';
*cur = '\0';
break;
}
if (t[0] == '.') {
if (!t[1] || ISDIRSEP(t[1])) {
t += 1;
continue;
} else if (t[1] == '.' && (!t[2] || ISDIRSEP(t[2]))) {
if (!isrooted && cur == start) {
if (cur != very_start)
*cur++ = DIRSEP;
*cur++ = '.';
*cur++ = '.';
start = cur;
} else if (cur != start)
while (--cur > start && !ISDIRSEP(*cur))
;
t += 2;
continue;
}
}
if (cur != very_start)
*cur++ = DIRSEP;
while (*t && !ISDIRSEP(*t))
*cur++ = *t++;
}
}
void
set_current_wd(path)
char *path;
{
int len;
char *p = path;
if (!p && !(p = ksh_get_wd((char *) 0, 0)))
p = null;
len = strlen(p) + 1;
if (len > current_wd_size)
current_wd = aresize(current_wd, current_wd_size = len, APERM);
memcpy(current_wd, p, len);
if (p != path && p != null)
afree(p, ATEMP);
}
#ifdef S_ISLNK
char *
get_phys_path(path)
const char *path;
{
XString xs;
char *xp;
Xinit(xs, xp, strlen(path) + 1, ATEMP);
xp = do_phys_path(&xs, xp, path);
if (!xp)
return (char *) 0;
if (Xlength(xs, xp) == 0)
Xput(xs, xp, DIRSEP);
Xput(xs, xp, '\0');
return Xclose(xs, xp);
}
static char *
do_phys_path(xsp, xp, path)
XString *xsp;
char *xp;
const char *path;
{
const char *p, *q;
int len, llen;
int savepos;
char lbuf[PATH];
Xcheck(*xsp, xp);
for (p = path; p; p = q) {
while (ISDIRSEP(*p))
p++;
if (!*p)
break;
len = (q = ksh_strchr_dirsep(p)) ? q - p : strlen(p);
if (len == 1 && p[0] == '.')
continue;
if (len == 2 && p[0] == '.' && p[1] == '.') {
while (xp > Xstring(*xsp, xp)) {
xp--;
if (ISDIRSEP(*xp))
break;
}
continue;
}
savepos = Xsavepos(*xsp, xp);
Xput(*xsp, xp, DIRSEP);
XcheckN(*xsp, xp, len + 1);
memcpy(xp, p, len);
xp += len;
*xp = '\0';
llen = readlink(Xstring(*xsp, xp), lbuf, sizeof(lbuf) - 1);
if (llen < 0) {
if (errno != EINVAL)
return (char *) 0;
continue;
}
lbuf[llen] = '\0';
xp = ISABSPATH(lbuf) ? Xstring(*xsp, xp)
: Xrestpos(*xsp, xp, savepos);
if (!(xp = do_phys_path(xsp, xp, lbuf)))
return (char *) 0;
}
return xp;
}
#endif
#ifdef	TEST
main(argc, argv)
{
int	rv;
char	*cp, cdpath[256], pwd[256], file[256], result[256];
printf("enter CDPATH: "); gets(cdpath);
printf("enter PWD: "); gets(pwd);
while (1) {
if (printf("Enter file: "), gets(file) == 0)
return 0;
cp = cdpath;
do {
rv = make_path(pwd, file, &cp, result, sizeof(result));
printf("make_path returns (%d), \"%s\" ", rv, result);
simplify_path(result);
printf("(simpifies to \"%s\")\n", result);
} while (cp);
}
}
#endif