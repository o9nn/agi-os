#include <u.h>
#include <libc.h>
#include <fcall.h>
#include <thread.h>
#include <9p.h>
#include "cifs.h"
#include "remsmb.h"
#include "apinums.h"
static Pkt *
thdr(Session *s, Share *sp)
{
Pkt *p;
p = cifshdr(s, sp, SMB_COM_TRANSACTION);
p->tbase = pl16(p, 0);
pl16(p, 0);
pl16(p, 64);
pl16(p, MTU - T2HDRLEN - 128);
pl16(p, 1);
pl16(p, 0);
pl32(p, 1000);
pl16(p, 0);
pl16(p, 0);
pl16(p, 0);
pl16(p, 0);
pl16(p, 0);
pl16(p, 0);
pbytes(p);
return p;
}
static void
ptparam(Pkt *p)
{
uchar *pos;
if(((p->pos - p->tbase) % 2) != 0)
p8(p, 0);
pos = p->pos;
p->pos = p->tbase + 20;
pl16(p, pos - p->buf - NBHDRLEN);
p->tparam = p->pos = pos;
}
static void
ptdata(Pkt *p)
{
uchar *pos = p->pos;
assert(p->tparam != 0);
if(((p->pos - p->tbase) % 2) != 0)
p8(p, 0);
p->pos = p->tbase + 0;
pl16(p, pos - p->tparam);
p->pos = p->tbase + 18;
pl16(p, pos - p->tparam);
p->pos = p->tbase + 24;
pl16(p, pos - p->buf - NBHDRLEN);
p->tdata = p->pos = pos;
}
static int
trpc(Pkt *p)
{
int got;
uchar *pos = p->pos;
assert(p->tbase != 0);
assert(p->tdata != 0);
p->pos = p->tbase + 2;
pl16(p, pos - p->tdata);
p->pos = p->tbase + 22;
pl16(p, pos - p->tdata);
p->pos = pos;
if((got = cifsrpc(p)) == -1)
return -1;
gl16(p);
gl16(p);
gl16(p);
gl16(p);
p->tparam = p->buf + NBHDRLEN + gl16(p);
gl16(p);
gl16(p);
p->tdata = p->buf + NBHDRLEN + gl16(p);
gl16(p);
g8(p);
g8(p);
return got;
}
static void
gtparam(Pkt *p)
{
p->pos = p->tparam;
}
static void
gtdata(Pkt *p)
{
p->pos = p->tdata;
}
int
RAPshareenum(Session *s, Share *sp, Share **ent)
{
int ngot = 0, err, navail, nret;
char tmp[1024];
Pkt *p;
Share *q;
p = thdr(s, sp);
pstr(p, "\\PIPE\\LANMAN");
ptparam(p);
pl16(p, API_WShareEnum);
pascii(p, REMSmb_NetShareEnum_P);
pascii(p, REMSmb_share_info_0);
pl16(p, 0);
pl16(p, MTU - 200);
ptdata(p);
if(trpc(p) == -1){
free(p);
return -1;
}
gtparam(p);
err = gl16(p);
gl16(p);
nret = gl16(p);
navail = gl16(p);
if(err && err != RAP_ERR_MOREINFO){
werrstr("%s", raperrstr(err));
free(p);
return -1;
}
if(ngot == 0){
*ent = emalloc9p(sizeof(Share) * navail);
memset(*ent, 0, sizeof(Share) * navail);
}
q = *ent + ngot;
for (; ngot < navail && nret--; ngot++){
gmem(p, tmp, 13);
tmp[13] = 0;
q->name = estrdup9p(tmp);
q++;
}
if(ngot < navail)
fprint(2, "%s: %d/%d - share list incomplete\n", argv0, ngot, navail);
free(p);
return ngot;
}
int
RAPshareinfo(Session *s, Share *sp, char *share, Shareinfo2 *si2p)
{
int conv, err;
char tmp[1024];
Pkt *p;
p = thdr(s, sp);
pstr(p, "\\PIPE\\LANMAN");
ptparam(p);
pl16(p, API_WShareGetInfo);
pascii(p, REMSmb_NetShareGetInfo_P);
pascii(p, REMSmb_share_info_2);
pascii(p, share);
pl16(p, 1);
pl16(p, MTU - 200);
ptdata(p);
if(trpc(p) == -1){
free(p);
return -1;
}
gtparam(p);
err = gl16(p);
conv = gl16(p);
gl16(p);
gl16(p);
if(err){
werrstr("%s", raperrstr(err));
free(p);
return -1;
}
memset(si2p, 0, sizeof(Shareinfo2));
gmem(p, tmp, 13);
tmp[13] = 0;
g8(p);
si2p->name = estrdup9p(tmp);
si2p->type = gl16(p);
gconv(p, conv, tmp, sizeof tmp);
si2p->comment = estrdup9p(tmp);
gl16(p);
si2p->perms = gl16(p);
si2p->maxusrs = gl16(p);
si2p->activeusrs = gl16(p);
gconv(p, conv, tmp, sizeof tmp);
si2p->path = estrdup9p(tmp);
gl16(p);
gmem(p, tmp, 9);
tmp[9] = 0;
si2p->passwd = estrdup9p(tmp);
free(p);
return 0;
}
int
RAPsessionenum(Session *s, Share *sp, Sessinfo **sip)
{
int ngot = 0, conv, err, navail, nret;
char tmp[1024];
Pkt *p;
Sessinfo *q;
p = thdr(s, sp);
pstr(p, "\\PIPE\\LANMAN");
ptparam(p);
pl16(p, API_WSessionEnum);
pascii(p, REMSmb_NetSessionEnum_P);
pascii(p, REMSmb_session_info_10);
pl16(p, 10);
pl16(p, MTU - 200);
ptdata(p);
if(trpc(p) == -1){
free(p);
return -1;
}
gtparam(p);
err = gl16(p);
conv = gl16(p);
nret = gl16(p);
navail = gl16(p);
if(err && err != RAP_ERR_MOREINFO){
werrstr("%s", raperrstr(err));
free(p);
return -1;
}
if(ngot == 0){
*sip = emalloc9p(sizeof(Sessinfo) * navail);
memset(*sip, 0, sizeof(Sessinfo) * navail);
}
q = *sip + ngot;
while(nret-- != 0){
gconv(p, conv, tmp, sizeof tmp);
q->wrkstn = estrdup9p(tmp);
gconv(p, conv, tmp, sizeof tmp);
q->user = estrdup9p(tmp);
q->sesstime = gl32(p);
q->idletime = gl32(p);
ngot++;
q++;
}
if(ngot < navail)
fprint(2, "warning: %d/%d - session list incomplete\n", ngot, navail);
free(p);
return ngot;
}
int
RAPgroupenum(Session *s, Share *sp, Namelist **nlp)
{
int ngot, err, navail, nret;
char tmp[1024];
Pkt *p;
Namelist *q;
ngot = 0;
p = thdr(s, sp);
pstr(p, "\\PIPE\\LANMAN");
ptparam(p);
pl16(p, API_WGroupEnum);
pascii(p, REMSmb_NetGroupEnum_P);
pascii(p, REMSmb_group_info_0);
pl16(p, 0);
pl16(p, MTU - 200);
ptdata(p);
if(trpc(p) == -1){
free(p);
return -1;
}
gtparam(p);
err = gl16(p);
gl16(p);
nret = gl16(p);
navail = gl16(p);
if(err && err != RAP_ERR_MOREINFO){
werrstr("%s", raperrstr(err));
free(p);
return -1;
}
*nlp = emalloc9p(sizeof(Namelist) * navail);
memset(*nlp, 0, sizeof(Namelist) * navail);
q = *nlp + ngot;
while(ngot < navail && nret--){
gmem(p, tmp, 21);
tmp[21] = 0;
q->name = estrdup9p(tmp);
ngot++;
q++;
if(p->pos >= p->eop)
break;
}
free(p);
return ngot;
}
int
RAPgroupusers(Session *s, Share *sp, char *group, Namelist **nlp)
{
int ngot, err, navail, nret;
char tmp[1024];
Pkt *p;
Namelist *q;
ngot = 0;
p = thdr(s, sp);
pstr(p, "\\PIPE\\LANMAN");
ptparam(p);
pl16(p, API_WGroupGetUsers);
pascii(p, REMSmb_NetGroupGetUsers_P);
pascii(p, REMSmb_user_info_0);
pascii(p, group);
pl16(p, 0);
pl16(p, MTU - 200);
ptdata(p);
if(trpc(p) == -1){
free(p);
return -1;
}
gtparam(p);
err = gl16(p);
gl16(p);
nret = gl16(p);
navail = gl16(p);
if(err && err != RAP_ERR_MOREINFO){
werrstr("%s", raperrstr(err));
free(p);
return -1;
}
*nlp = emalloc9p(sizeof(Namelist) * navail);
memset(*nlp, 0, sizeof(Namelist) * navail);
q = *nlp + ngot;
while(ngot < navail && nret--){
gmem(p, tmp, 21);
tmp[21] = 0;
q->name = estrdup9p(tmp);
ngot++;
q++;
if(p->pos >= p->eop)
break;
}
free(p);
return ngot;
}
int
RAPuserenum(Session *s, Share *sp, Namelist **nlp)
{
int ngot, err, navail, nret;
char tmp[1024];
Pkt *p;
Namelist *q;
ngot = 0;
p = thdr(s, sp);
pstr(p, "\\PIPE\\LANMAN");
ptparam(p);
pl16(p, API_WUserEnum);
pascii(p, REMSmb_NetUserEnum_P);
pascii(p, REMSmb_user_info_0);
pl16(p, 0);
pl16(p, MTU - 200);
ptdata(p);
if(trpc(p) == -1){
free(p);
return -1;
}
gtparam(p);
err = gl16(p);
gl16(p);
nret = gl16(p);
navail = gl16(p);
if(err && err != RAP_ERR_MOREINFO){
werrstr("%s", raperrstr(err));
free(p);
return -1;
}
*nlp = emalloc9p(sizeof(Namelist) * navail);
memset(*nlp, 0, sizeof(Namelist) * navail);
q = *nlp + ngot;
while(ngot < navail && nret--){
gmem(p, tmp, 21);
tmp[21] = 0;
q->name = estrdup9p(tmp);
ngot++;
q++;
if(p->pos >= p->eop)
break;
}
free(p);
return ngot;
}
int
RAPuserenum2(Session *s, Share *sp, Namelist **nlp)
{
int ngot, resume, err, navail, nret;
char tmp[1024];
Pkt *p;
Namelist *q;
ngot = 0;
resume = 0;
more:
p = thdr(s, sp);
pstr(p, "\\PIPE\\LANMAN");
ptparam(p);
pl16(p, API_WUserEnum2);
pascii(p, REMSmb_NetUserEnum2_P);
pascii(p, REMSmb_user_info_0);
pl16(p, 0);
pl16(p, MTU - 200);
pl32(p, resume);
ptdata(p);
if(trpc(p) == -1){
free(p);
return -1;
}
gtparam(p);
err = gl16(p);
gl16(p);
resume = gl32(p);
nret = gl16(p);
navail = gl16(p);
if(err && err != RAP_ERR_MOREINFO){
werrstr("%s", raperrstr(err));
free(p);
return -1;
}
if(ngot == 0){
*nlp = emalloc9p(sizeof(Namelist) * navail);
memset(*nlp, 0, sizeof(Namelist) * navail);
}
q = *nlp + ngot;
while(ngot < navail && nret--){
gmem(p, tmp, 21);
tmp[21] = 0;
q->name = estrdup9p(tmp);
ngot++;
q++;
if(p->pos >= p->eop)
break;
}
free(p);
if(ngot < navail)
goto more;
return ngot;
}
int
RAPuserinfo(Session *s, Share *sp, char *user, Userinfo *uip)
{
int conv, err;
char tmp[1024];
Pkt *p;
p = thdr(s, sp);
pstr(p, "\\PIPE\\LANMAN");
ptparam(p);
pl16(p, API_WUserGetInfo);
pascii(p, REMSmb_NetUserGetInfo_P);
pascii(p, REMSmb_user_info_10);
pascii(p, user);
pl16(p, 10);
pl16(p, MTU - 200);
ptdata(p);
if(trpc(p) == -1){
free(p);
return -1;
}
gtparam(p);
err = gl16(p);
conv = gl16(p);
gl16(p);
gl16(p);
if(err && err != RAP_ERR_MOREINFO){
werrstr("%s", raperrstr(err));
free(p);
return -1;
}
gmem(p, tmp, 21);
tmp[21] = 0;
uip->user = estrdup9p(tmp);
g8(p);
gconv(p, conv, tmp, sizeof tmp);
uip->comment = estrdup9p(tmp);
gconv(p, conv, tmp, sizeof tmp);
uip->user_comment = estrdup9p(tmp);
gconv(p, conv, tmp, sizeof tmp);
uip->fullname = estrdup9p(tmp);
free(p);
return 0;
}
int
RAPServerenum2(Session *s, Share *sp, char *workgroup, int type, int *more,
Serverinfo **si)
{
int ngot = 0, conv, err, nret, navail;
char tmp[1024];
Pkt *p;
Serverinfo *q;
p = thdr(s, sp);
pstr(p, "\\PIPE\\LANMAN");
ptparam(p);
pl16(p, API_NetServerEnum2);
pascii(p, REMSmb_NetServerEnum2_P);
pascii(p, REMSmb_server_info_1);
pl16(p, 1);
pl16(p, MTU - 200);
pl32(p, type);
pascii(p, workgroup);
ptdata(p);
if(trpc(p) == -1){
free(p);
return -1;
}
gtparam(p);
err = gl16(p);
conv = gl16(p);
nret = gl16(p);
navail = gl16(p);
if(err && err != RAP_ERR_MOREINFO){
werrstr("%s", raperrstr(err));
free(p);
return -1;
}
*si = emalloc9p(sizeof(Serverinfo) * navail);
memset(*si, 0, sizeof(Serverinfo) * navail);
q = *si;
for (; nret-- != 0 && ngot < navail; ngot++){
gmem(p, tmp, 16);
tmp[16] = 0;
q->name = estrdup9p(tmp);
q->major = g8(p);
q->minor = g8(p);
q->type = gl32(p);
gconv(p, conv, tmp, sizeof tmp);
q->comment = estrdup9p(tmp);
q++;
}
free(p);
*more = err == RAP_ERR_MOREINFO;
return ngot;
}
int
RAPServerenum3(Session *s, Share *sp, char *workgroup, int type, int last,
Serverinfo *si)
{
int conv, err, ngot, nret, navail;
char *first, tmp[1024];
Pkt *p;
Serverinfo *q;
ngot = last +1;
first = si[last].name;
more:
p = thdr(s, sp);
pstr(p, "\\PIPE\\LANMAN");
ptparam(p);
pl16(p, API_NetServerEnum3);
pascii(p, REMSmb_NetServerEnum3_P);
pascii(p, REMSmb_server_info_1);
pl16(p, 1);
pl16(p, MTU - 200);
pl32(p, type);
pascii(p, workgroup);
pascii(p, first);
ptdata(p);
if(trpc(p) == -1){
free(p);
return -1;
}
gtparam(p);
err = gl16(p);
conv = gl16(p);
nret = gl16(p);
navail = gl16(p);
if(err && err != RAP_ERR_MOREINFO){
werrstr("%s", raperrstr(err));
free(p);
return -1;
}
if(nret < 2){
free(p);
return ngot;
}
q = si+ngot;
while(nret-- != 0 && ngot < navail){
gmem(p, tmp, 16);
tmp[16] = 0;
q->name = estrdup9p(tmp);
q->major = g8(p);
q->minor = g8(p);
q->type = gl32(p);
gconv(p, conv, tmp, sizeof tmp);
tmp[sizeof tmp - 1] = 0;
q->comment = estrdup9p(tmp);
if(strcmp(first, tmp) == 0){
free(q->name);
free(q->comment);
continue;
}
ngot++;
q++;
}
free(p);
if(ngot < navail)
goto more;
return ngot;
}
int
RAPFileenum2(Session *s, Share *sp, char *user, char *path, Fileinfo **fip)
{
int conv, err, ngot, resume, nret, navail;
char tmp[1024];
Pkt *p;
Fileinfo *q;
ngot = 0;
resume = 0;
more:
p = thdr(s, sp);
pstr(p, "\\PIPE\\LANMAN");
ptparam(p);
pl16(p, API_WFileEnum2);
pascii(p, REMSmb_NetFileEnum2_P);
pascii(p, REMSmb_file_info_1);
pascii(p, path);
pascii(p, user);
pl16(p, 1);
pl16(p, MTU - 200);
pl32(p, resume);
pl32(p, 0);
ptdata(p);
if(trpc(p) == -1){
free(p);
return -1;
}
gtparam(p);
err = gl16(p);
conv = gl16(p);
resume = gl32(p);
nret = gl16(p);
navail = gl16(p);
if(err && err != RAP_ERR_MOREINFO){
werrstr("%s", raperrstr(err));
free(p);
return -1;
}
if(nret < 2){
free(p);
return ngot;
}
if(ngot == 0){
*fip = emalloc9p(sizeof(Fileinfo) * navail);
memset(*fip, 0, sizeof(Fileinfo) * navail);
}
q = *fip + ngot;
for(; nret-- && ngot < navail; ngot++){
q->ident = gl16(p);
q->perms = gl16(p);
q->locks = gl16(p);
gconv(p, conv, tmp, sizeof tmp);
tmp[sizeof tmp - 1] = 0;
q->path = estrdup9p(tmp);
gconv(p, conv, tmp, sizeof tmp);
tmp[sizeof tmp - 1] = 0;
q->user = estrdup9p(tmp);
q++;
}
free(p);
if(ngot < navail)
goto more;
return ngot;
}