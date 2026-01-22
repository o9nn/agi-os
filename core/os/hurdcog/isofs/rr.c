#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <sys/sysmacros.h>
#include "isofs.h"
int susp_live = 0;
int rock_live = 0;
int gnuext_live = 0;
int susp_skip = 0;
void
release_rrip (struct rrip_lookup *rr)
{
if ((rr->valid & VALID_NM) && rr->name)
free (rr->name);
if ((rr->valid & VALID_SL) && rr->target)
free (rr->target);
if ((rr->valid & VALID_TR) && rr->trans)
free (rr->trans);
}
static int
rrip_work (struct dirrect *dr, struct rrip_lookup *rr,
const char *match_name, size_t match_name_len,
int initializing, int ignorenm)
{
void *bp, *terminus;
void *slbuf, *nmbuf;
size_t slbufsize, nmbufsize;
int nomorenm, nomoresl;
rr->valid = 0;
rr->target = rr->name = 0;
if (!susp_live && !initializing)
return 0;
if (!rock_live && !initializing)
return 0;
nmbuf = slbuf = 0;
nmbufsize = slbufsize = 0;
nomorenm = nomoresl = 0;
if (dr == (struct dirrect *)sblock->root)
{
struct dirrect *p;
off_t filestart;
unsigned char *c;
error_t err;
err = calculate_file_start (dr, &filestart, 0);
if (err)
return 0;
p = disk_image + (filestart << store->log2_block_size);
c = p->name + p->namelen;
if ((uintptr_t)c & 1)
c++;
if (!bcmp (c, "SP\7\1\276\357", 6))
bp = c;
else if (!bcmp (c + 15, "SP\7\1\276\357", 6))
bp = c + 15;
else
return 0;
terminus = (char *) p + p->len;
}
else
{
bp = dr->name + dr->namelen;
if ((uintptr_t) bp & 1)
bp++;
bp += susp_skip;
terminus = (char *) dr + dr->len;
}
while (bp < terminus)
{
struct su_header *susp = bp;
void *body;
if (bp + sizeof (struct su_header) > terminus
|| bp + susp->len > terminus)
break;
body = (char *) susp + sizeof (struct su_header);
if (susp->sig[0] == 'C'
&& susp->sig[1] == 'E'
&& susp->version == 1)
{
int offset;
int location;
int size;
struct su_ce *ce = body;
offset = isonum_733 (ce->offset);
location = isonum_733 (ce->continuation);
size = isonum_733 (ce->size);
bp = disk_image + (location * logical_block_size) + offset;
terminus = bp + size;
continue;
}
if (initializing
&& susp->sig[0] == 'S'
&& susp->sig[1] == 'P'
&& susp->version == 1)
{
struct su_sp *sp = body;
if (sp->check[0] == SU_SP_CHECK_0
&& sp->check[1] == SU_SP_CHECK_1)
susp_live = 1;
susp_skip = sp->skip;
goto next_field;
}
if (initializing
&& susp->sig[0] == 'E'
&& susp->sig[1] == 'R'
&& susp->version == 1)
{
struct su_er *er = body;
if ((void *) er->more + er->len_id + er->len_des + er->len_src
> terminus)
goto next_field;
if (er->ext_ver == ROCK_VERS
&& !memcmp (ROCK_ID, er->more, er->len_id))
rock_live = 1;
else if (er->ext_ver == GNUEXT_VERS
&& !memcmp (GNUEXT_ID, er->more, er->len_id))
gnuext_live = 1;
}
if (susp->sig[0] == 'P'
&& susp->sig[1] == 'D'
&& susp->version == 1)
goto next_field;
if (susp->sig[0] == 'S'
&& susp->sig[1] == 'T'
&& susp->version == 1)
break;
if (initializing || !rock_live)
goto next_field;
if (susp->sig[0] == 'R'
&& susp->sig[1] == 'E'
&& susp->version == 1)
{
rr->valid |= VALID_RE;
break;
}
if (susp->sig[0] == 'N'
&& susp->sig[1] == 'M'
&& susp->version == 1
&& !ignorenm)
{
struct rr_nm *nm = body;
size_t nmlen = susp->len - 5;
char *name;
size_t namelen;
if (nomorenm)
goto next_field;
if (nm->flags & NAME_DOT)
{
name = ".";
namelen = 1;
goto finalize_nm;
}
else if (nm->flags & NAME_DOTDOT)
{
name = "..";
namelen = 2;
goto finalize_nm;
}
else if (nm->flags & NAME_HOST)
{
name = host_name;
namelen = strlen (host_name);
goto finalize_nm;
}
if (!nmbuf)
nmbuf = malloc ((nmbufsize = nmlen) + 1);
else
nmbuf = realloc (nmbuf, (nmbufsize += nmlen) + 1);
assert_backtrace (nmbuf);
memcpy (nmbuf + nmbufsize - nmlen, nm->name, nmlen);
if (nm->flags & NAME_CONTINUE)
goto next_field;
name = nmbuf;
namelen = nmbufsize;
finalize_nm:
nomorenm = 1;
if (match_name && (match_name_len != namelen
|| memcmp (match_name, name, match_name_len)))
{
if (nmbuf)
free (nmbuf);
return 0;
}
rr->valid |= VALID_NM;
if (name != nmbuf)
{
rr->name = strdup (name);
assert_backtrace (rr->name);
}
else
{
rr->name = name;
name[namelen] = '\0';
}
if (rr->valid & VALID_CL)
goto clrecurse;
goto next_field;
}
if (susp->sig[0] == 'P'
&& susp->sig[1] == 'X'
&& susp->version == 1)
{
struct rr_px *px = body;
rr->valid |= VALID_PX;
rr->mode = isonum_733 (px->mode);
rr->nlink = isonum_733 (px->nlink);
rr->uid = isonum_733 (px->uid);
rr->gid = isonum_733 (px->gid);
goto next_field;
}
if (susp->sig[0] == 'P'
&& susp->sig[1] == 'N'
&& susp->version == 1)
{
struct rr_pn *pn = body;
rr->valid |= VALID_PN;
rr->rdev = gnu_dev_makedev (isonum_733 (pn->high), isonum_733 (pn->low));
goto next_field;
}
if (susp->sig[0] == 'S'
&& susp->sig[1] == 'L'
&& susp->version == 1)
{
struct rr_sl *sl = body;
size_t crlen = susp->len - 5;
struct rr_sl_comp *comp;
void *cp;
size_t targalloced, targused;
void add_comp (char *cname, size_t cnamelen)
{
if (rr->target == 0)
{
rr->target = malloc (cnamelen * 2);
targused = 0;
targalloced = cnamelen * 2;
}
else while (targused + cnamelen > targalloced)
rr->target = realloc (rr->target, targalloced *= 2);
assert_backtrace (rr->target);
memcpy (rr->target + targused, cname, cnamelen);
targused += cnamelen;
}
if (nomoresl)
goto next_field;
if (!slbuf)
slbuf = malloc (slbufsize = crlen);
else
slbuf = realloc (slbuf, slbufsize += crlen);
assert_backtrace (slbuf);
memcpy (slbuf + slbufsize - crlen, sl->data, crlen);
if (sl->flags & 1)
goto next_field;
for (cp = slbuf; cp < slbuf + slbufsize; cp += comp->len + 2)
{
comp = (struct rr_sl_comp *)cp;
nomoresl = 1;
if (comp->flags & NAME_DOT)
add_comp ("./", 2);
else if (comp->flags & NAME_DOTDOT)
add_comp ("../", 3);
else if (comp->flags & NAME_ROOT)
{
targused = 0;
add_comp ("/", 1);
}
else if (comp->flags & NAME_VOLROOT)
{
targused = 0;
add_comp (mounted_on, strlen (mounted_on));
}
else if (comp->flags & NAME_HOST)
{
add_comp (host_name, strlen (host_name));
add_comp ("/", 1);
}
else
{
add_comp (comp->name, comp->len);
if (!(comp->flags & NAME_CONTINUE))
add_comp ("/", 1);
}
}
if (rr->target[targused - 1] == '/')
rr->target[targused - 1] = '\0';
else
add_comp ("", 1);
rr->valid |= VALID_SL;
free (slbuf);
goto next_field;
}
if (susp->sig[0] == 'T'
&& susp->sig[1] == 'F'
&& susp->version == 1)
{
char *(*convert)(char *, struct timespec *);
struct rr_tf *tf = body;
char *c;
if (tf->flags & TF_LONG_FORM)
convert = isodate_84261;
else
convert = isodate_915;
rr->valid |= VALID_TF;
rr->tfflags = tf->flags;
c = tf->data;
if (rr->tfflags & TF_CREATION)
c = (*convert) (c, &rr->ctime);
if (rr->tfflags & TF_MODIFY)
c = (*convert) (c, &rr->mtime);
if (rr->tfflags & TF_ACCESS)
c = (*convert) (c, &rr->atime);
goto next_field;
}
if (susp->sig[0] == 'C'
&& susp->sig[1] == 'L'
&& susp->version == 1)
{
struct rr_cl *cl = body;
rr->realdirent
= disk_image + (isonum_733 (cl->loc) * logical_block_size);
rr->valid |= VALID_CL;
if (rr->valid & VALID_NM)
{
char *savename;
struct dirrect *realdir;
clrecurse:
savename = (rr->valid & VALID_NM) ? rr->name : 0;
realdir = rr->realdirent;
rrip_work (realdir, rr, 0, 0, 0, 1);
rr->valid |= VALID_CL;
rr->realdirent = realdir;
if (savename)
{
rr->valid |= VALID_NM;
rr->name = savename;
}
return (rr->valid & VALID_NM) ? 1 : 0;
}
goto next_field;
}
if (susp->sig[0] == 'P'
&& susp->sig[1] == 'L'
&& susp->version == 1)
{
struct rr_pl *pl = body;
rr->realfilestart = (isonum_733 (pl->loc)
* (logical_block_size
>> store->log2_block_size));
rr->valid |= VALID_PL;
goto next_field;
}
if (!gnuext_live)
goto next_field;
if (susp->sig[0] == 'A'
&& susp->sig[1] == 'U'
&& susp->version == 1)
{
struct gn_au *au = body;
rr->author = isonum_733 (au->author);
rr->valid |= VALID_AU;
goto next_field;
}
if (susp->sig[0] == 'T'
&& susp->sig[1] == 'R'
&& susp->version == 1)
{
struct gn_tr *tr = body;
rr->translen = tr->len;
rr->trans = malloc (rr->translen);
assert_backtrace (rr->trans);
memcpy (tr->data, rr->trans, rr->translen);
rr->valid |= VALID_TR;
goto next_field;
}
if (susp->sig[0] == 'M'
&& susp->sig[1] == 'D'
&& susp->version == 1)
{
struct gn_md *md = body;
rr->allmode = isonum_733 (md->mode);
rr->valid |= VALID_MD;
goto next_field;
}
if (susp->sig[0] == 'F'
&& susp->sig[1] == 'L'
&& susp->version == 1)
{
struct gn_fl *fl = body;
rr->flags = isonum_733 (fl->flags);
rr->valid |= VALID_FL;
goto next_field;
}
next_field:
bp = bp + susp->len;
}
if (rr->valid & VALID_CL)
goto clrecurse;
return rr->valid & VALID_NM ? 1 : 0;
}
int
rrip_match_lookup (struct dirrect *dr, const char *name, size_t namelen,
struct rrip_lookup *rr)
{
return rrip_work (dr, rr, name, namelen, 0, 0);
}
void
rrip_lookup (struct dirrect *dr, struct rrip_lookup *rr, int ignorenm)
{
rrip_work (dr, rr, 0, 0, 0, ignorenm);
}
void
rrip_initialize (struct dirrect *dr)
{
struct rrip_lookup rr;
rrip_work (dr, &rr, 0, 0, 1, 1);
release_rrip (&rr);
}