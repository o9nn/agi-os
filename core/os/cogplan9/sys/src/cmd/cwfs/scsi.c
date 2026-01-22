#include "all.h"
#include "io.h"
enum {
Ninquiry	= 255,
Nsense		= 255,
CMDtest		= 0x00,
CMDreqsense	= 0x03,
CMDread6	= 0x08,
CMDwrite6	= 0x0A,
CMDinquiry	= 0x12,
CMDstart	= 0x1B,
CMDread10	= 0x28,
CMDwrite10	= 0x2A,
};
typedef struct {
Target	target[NTarget];
} Ctlr;
static Ctlr scsictlr[MaxScsi];
extern int scsiverbose;
void
scsiinit(void)
{
Ctlr *ctlr;
int ctlrno, targetno;
Target *tp;
scsiverbose = 1;
for(ctlrno = 0; ctlrno < MaxScsi; ctlrno++){
ctlr = &scsictlr[ctlrno];
memset(ctlr, 0, sizeof(Ctlr));
for(targetno = 0; targetno < NTarget; targetno++){
tp = &ctlr->target[targetno];
qlock(tp);
qunlock(tp);
sprint(tp->id, "scsictlr#%d.%d", ctlrno, targetno);
tp->ctlrno = ctlrno;
tp->targetno = targetno;
tp->inquiry = malloc(Ninquiry);
tp->sense = malloc(Nsense);
}
}
}
static uchar lastcmd[16];
static int lastcmdsz;
static int
sense2stcode(uchar *sense)
{
switch(sense[2] & 0x0F){
case 6:
if(sense[12] != 0x28 && sense[12] != 0x29)
return STcheck;
case 0:
case 1:
return STok;
case 8:
return STblank;
case 2:
if(sense[12] == 0x3A)
return STcheck;
default:
if((sense[12] == 0x04 && sense[13] == 0x01)) {
fprint(2, "sense2stcode: unit becoming ready\n");
return STcheck;
}
return STcheck;
}
}
static int
doscsi(Target* tp, int rw, uchar* cmd, int cbytes, void* data, int* dbytes)
{
int lun, db = 0;
uchar reqcmd[6], reqdata[Nsense], dummy[1];
Scsi *sc;
sc = tp->sc;
if (sc == nil)
panic("doscsi: nil tp->sc");
lun = cmd[1] >> 5;
if (dbytes != nil)
db = *dbytes;
if (data == nil)
data = dummy;
if (scsi(sc, cmd, cbytes, data, db, rw) >= 0)
return STok;
memset(reqcmd, 0, sizeof reqcmd);
reqcmd[0] = CMDreqsense;
reqcmd[1] = lun<<5;
reqcmd[4] = Nsense;
memset(reqdata, 0, sizeof reqdata);
if (scsicmd(sc, reqcmd, sizeof reqcmd, reqdata, sizeof reqdata,
Sread) < 0)
return STharderr;
return sense2stcode(reqdata);
}
static int
scsiexec(Target* tp, int rw, uchar* cmd, int cbytes, void* data, int* dbytes)
{
int s;
s = doscsi(tp, rw, cmd, cbytes, data, dbytes);
switch(s){
case STcheck:
memmove(lastcmd, cmd, cbytes);
lastcmdsz = cbytes;
default:
break;
}
return s;
}
static int
scsitest(Target* tp, char lun)
{
uchar cmd[6];
memset(cmd, 0, sizeof cmd);
cmd[0] = CMDtest;
cmd[1] = lun<<5;
return scsiexec(tp, SCSIread, cmd, sizeof cmd, 0, 0);
}
static int
scsistart(Target* tp, char lun, int start)
{
uchar cmd[6];
memset(cmd, 0, sizeof cmd);
cmd[0] = CMDstart;
cmd[1] = lun<<5;
if(start)
cmd[4] = 1;
return scsiexec(tp, SCSIread, cmd, sizeof cmd, 0, 0);
}
static int
scsiinquiry(Target* tp, char lun, int* nbytes)
{
uchar cmd[6];
memset(cmd, 0, sizeof cmd);
cmd[0] = CMDinquiry;
cmd[1] = lun<<5;
*nbytes = Ninquiry;
cmd[4] = *nbytes;
return scsiexec(tp, SCSIread, cmd, sizeof cmd, tp->inquiry, nbytes);
}
static char *key[] =
{
"no sense",
"recovered error",
"not ready",
"medium error",
"hardware error",
"illegal request",
"unit attention",
"data protect",
"blank check",
"vendor specific",
"copy aborted",
"aborted command",
"equal",
"volume overflow",
"miscompare",
"reserved"
};
static int
scsireqsense(Target* tp, char lun, int* nbytes, int quiet)
{
char *s;
int n, status, try;
uchar cmd[6], *sense;
sense = tp->sense;
for(try = 0; try < 20; try++) {
memset(cmd, 0, sizeof cmd);
cmd[0] = CMDreqsense;
cmd[1] = lun<<5;
cmd[4] = Ninquiry;
memset(sense, 0, Ninquiry);
*nbytes = Ninquiry;
status = scsiexec(tp, SCSIread, cmd, sizeof cmd, sense, nbytes);
if(status != STok)
return status;
*nbytes = sense[0x07]+8;
switch(sense[2] & 0x0F){
case 6:
if(sense[12] != 0x28 && sense[12] != 0x29)
goto buggery;
case 0:
case 1:
return STok;
case 8:
return STblank;
case 2:
if(sense[12] == 0x3A)
goto buggery;
default:
if((sense[12] == 0x04 && sense[13] == 0x01)){
delay(500);
scsitest(tp, lun);
break;
}
goto buggery;
}
}
buggery:
if(quiet == 0){
s = key[sense[2]&0x0F];
print("%s: reqsense: '%s' code #%2.2ux #%2.2ux\n",
tp->id, s, sense[12], sense[13]);
print("%s: byte 2: #%2.2ux, bytes 15-17: #%2.2ux #%2.2ux #%2.2ux\n",
tp->id, sense[2], sense[15], sense[16], sense[17]);
print("lastcmd (%d): ", lastcmdsz);
for(n = 0; n < lastcmdsz; n++)
print(" #%2.2ux", lastcmd[n]);
print("\n");
}
return STcheck;
}
static Target*
scsitarget(Device* d)
{
int ctlrno, targetno;
ctlrno = d->wren.ctrl;
if(ctlrno < 0 || ctlrno >= MaxScsi )
return 0;
targetno = d->wren.targ;
if(targetno < 0 || targetno >= NTarget)
return 0;
return &scsictlr[ctlrno].target[targetno];
}
static void
scsiprobe(Device* d)
{
Target *tp;
int nbytes, s;
uchar *sense;
int acount;
if((tp = scsitarget(d)) == 0)
panic("scsiprobe: device = %Z", d);
acount = 0;
again:
s = scsitest(tp, d->wren.lun);
if(s < STok){
print("%s: test, status %d\n", tp->id, s);
return;
}
s = scsireqsense(tp, d->wren.lun, &nbytes, acount);
sense = tp->sense;
switch(s){
case STok:
if ((sense[2] & 0x0F) == 0x06 &&
(sense[12] == 0x28 || sense[12] == 0x29))
if(acount == 0){
acount = 1;
goto again;
}
break;
case STcheck:
if((sense[2] & 0x0F) == 0x02){
if(sense[12] == 0x3A)
break;
if(sense[12] == 0x04 && sense[13] == 0x02){
print("%s: starting...\n", tp->id);
if(scsistart(tp, d->wren.lun, 1) == STok)
break;
s = scsireqsense(tp, d->wren.lun, &nbytes, 0);
}
}
default:
print("%s: unavailable, status %d\n", tp->id, s);
return;
}
s = scsiinquiry(tp, d->wren.lun, &nbytes);
if(s != STok) {
print("%s: inquiry failed, status %d\n", tp->id, s);
return;
}
print("%s: %s\n", tp->id, (char*)tp->inquiry+8);
tp->ok = 1;
}
int
scsiio(Device* d, int rw, uchar* cmd, int cbytes, void* data, int dbytes)
{
Target *tp;
int e, nbytes, s;
if((tp = scsitarget(d)) == 0)
panic("scsiio: device = %Z", d);
qlock(tp);
if(tp->ok == 0)
scsiprobe(d);
qunlock(tp);
s = STinit;
for(e = 0; e < 10; e++){
for(;;){
nbytes = dbytes;
s = scsiexec(tp, rw, cmd, cbytes, data, &nbytes);
if(s == STok)
break;
s = scsireqsense(tp, d->wren.lun, &nbytes, 0);
if(s == STblank && rw == SCSIread) {
memset(data, 0, dbytes);
return STok;
}
if(s != STok)
break;
}
if(s == STok)
break;
}
if(e)
print("%s: retry %d cmd #%x\n", tp->id, e, cmd[0]);
return s;
}
void
newscsi(Device *d, Scsi *sc)
{
Target *tp;
if((tp = scsitarget(d)) == nil)
panic("newscsi: device = %Z", d);
tp->sc = sc;
}