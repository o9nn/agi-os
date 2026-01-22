#include "config.h"
#ifdef KSH
#include "sh.h"
#include "ksh_stat.h"
#include "ksh_time.h"
#define MBMESSAGE "you have mail in $_"
typedef struct mbox {
struct mbox *mb_next;
char *mb_path;
char *mb_msg;
time_t mb_mtime;
} mbox_t;
static mbox_t *mplist;
static mbox_t mbox;
static time_t mlastchkd;
static time_t mailcheck_interval;
static void munset ARGS((mbox_t *mlist));
static mbox_t * mballoc ARGS((char *p, char *m));
static void mprintit ARGS((mbox_t *mbp));
void
mcheck()
{
register mbox_t *mbp;
time_t now;
struct tbl *vp;
struct stat stbuf;
now = time((time_t *) 0);
if (mlastchkd == 0)
mlastchkd = now;
if (now - mlastchkd >= mailcheck_interval) {
mlastchkd = now;
if (mplist)
mbp = mplist;
else if ((vp = global("MAIL")) && (vp->flag & ISSET))
mbp = &mbox;
else
mbp = NULL;
while (mbp) {
if (mbp->mb_path && stat(mbp->mb_path, &stbuf) == 0
&& S_ISREG(stbuf.st_mode))
{
if (stbuf.st_size
&& mbp->mb_mtime != stbuf.st_mtime
&& stbuf.st_atime <= stbuf.st_mtime)
mprintit(mbp);
mbp->mb_mtime = stbuf.st_mtime;
} else {
mbp->mb_mtime = 0;
}
mbp = mbp->mb_next;
}
}
}
void
mcset(interval)
long interval;
{
mailcheck_interval = interval;
}
void
mbset(p)
register char *p;
{
struct stat stbuf;
if (mbox.mb_msg)
afree((void *)mbox.mb_msg, APERM);
if (mbox.mb_path)
afree((void *)mbox.mb_path, APERM);
mbox.mb_path = str_save(p, APERM);
mbox.mb_msg = NULL;
if (p && stat(p, &stbuf) == 0 && S_ISREG(stbuf.st_mode))
mbox.mb_mtime = stbuf.st_mtime;
else
mbox.mb_mtime = 0;
}
void
mpset(mptoparse)
register char *mptoparse;
{
register mbox_t *mbp;
register char *mpath, *mmsg, *mval;
char *p;
munset( mplist );
mplist = NULL;
mval = str_save(mptoparse, APERM);
while (mval) {
mpath = mval;
if ((mval = strchr(mval, PATHSEP)) != NULL) {
*mval = '\0', mval++;
}
for (p = mpath; (mmsg = strchr(p, '%')); ) {
if (mmsg[-1] == '\\') {
memmove(mmsg - 1, mmsg, strlen(mmsg) + 1);
p = mmsg + 1;
continue;
}
break;
}
if (!mmsg && !Flag(FPOSIX))
mmsg = strchr(mpath, '?');
if (mmsg) {
*mmsg = '\0';
mmsg++;
}
mbp = mballoc(mpath, mmsg);
mbp->mb_next = mplist;
mplist = mbp;
}
}
static void
munset(mlist)
register mbox_t *mlist;
{
register mbox_t *mbp;
while (mlist != NULL) {
mbp = mlist;
mlist = mbp->mb_next;
if (!mlist)
afree((void *)mbp->mb_path, APERM);
afree((void *)mbp, APERM);
}
}
static mbox_t *
mballoc(p, m)
char *p;
char *m;
{
struct stat stbuf;
register mbox_t *mbp;
mbp = (mbox_t *)alloc(sizeof(mbox_t), APERM);
mbp->mb_next = NULL;
mbp->mb_path = p;
mbp->mb_msg = m;
if (stat(mbp->mb_path, &stbuf) == 0 && S_ISREG(stbuf.st_mode))
mbp->mb_mtime = stbuf.st_mtime;
else
mbp->mb_mtime = 0;
return(mbp);
}
static void
mprintit( mbp )
mbox_t *mbp;
{
struct tbl *vp;
setstr((vp = local("_", FALSE)), mbp->mb_path, KSH_RETURN_ERROR);
shellf("%s\n", substitute(mbp->mb_msg ? mbp->mb_msg : MBMESSAGE, 0));
unset(vp, 0);
}
#endif