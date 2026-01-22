#include <lib9.h>
#include <bio.h>
#include <ctype.h>
#include "mach.h"
#define Extern extern
#include "acid.h"
static int fsize[] =
{
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,
4,
4,
1,
4,
0,
8,
8,
0,0,0,0,0,0,0,
4,
0,
4,
4,
4,
0,
4,
8,
0,
4,
8,
8,
0,0,0,0,0,0,
4,
1,
1,
2,
0,
4,
4,
0,0,0,0,0,0,0,
2,
0,
2,
2,
4,
0,
2,
0,0,
2,
};
int
fmtsize(Value *v)
{
int ret;
switch(v->vstore.fmt) {
default:
return fsize[v->vstore.fmt];
case 'i':
case 'I':
if(v->type != TINT || machdata == 0)
error("no size for i fmt pointer ++/--");
ret = (*machdata->instsize)(cormap, v->vstore.u0.sival);
if(ret < 0) {
ret = (*machdata->instsize)(symmap, v->vstore.u0.sival);
if(ret < 0)
error("%r");
}
return ret;
}
}
void
chklval(Node *lp)
{
if(lp->op != ONAME)
error("need l-value");
}
void
olist(Node *n, Node *res)
{
expr(n->left, res);
expr(n->right, res);
}
void
oeval(Node *n, Node *res)
{
expr(n->left, res);
if(res->type != TCODE)
error("bad type for eval");
expr(res->nstore.u0.scc, res);
}
void
ocast(Node *n, Node *res)
{
if(n->sym->lt == 0)
error("%s is not a complex type", n->sym->name);
expr(n->left, res);
res->nstore.comt = n->sym->lt;
res->nstore.fmt = 'a';
}
void
oindm(Node *n, Node *res)
{
Map *m;
Node l;
m = cormap;
if(m == 0)
m = symmap;
expr(n->left, &l);
if(l.type != TINT)
error("bad type for *");
if(m == 0)
error("no map for *");
indir(m, l.nstore.u0.sival, l.nstore.fmt, res);
res->nstore.comt = l.nstore.comt;
}
void
oindc(Node *n, Node *res)
{
Map *m;
Node l;
m = symmap;
if(m == 0)
m = cormap;
expr(n->left, &l);
if(l.type != TINT)
error("bad type for @");
if(m == 0)
error("no map for @");
indir(m, l.nstore.u0.sival, l.nstore.fmt, res);
res->nstore.comt = l.nstore.comt;
}
void
oframe(Node *n, Node *res)
{
char *p;
Node *lp;
uvlong ival;
Frtype *f;
p = n->sym->name;
while(*p && *p == '$')
p++;
lp = n->left;
if(localaddr(cormap, p, lp->sym->name, &ival, rget) < 0)
error("colon: %r");
res->nstore.u0.sival = ival;
res->op = OCONST;
res->nstore.fmt = 'X';
res->type = TINT;
for(f = n->sym->local; f; f = f->next) {
if(f->var == lp->sym) {
res->nstore.comt = f->type;
res->nstore.fmt = 'a';
break;
}
}
}
void
oindex(Node *n, Node *res)
{
Node l, r;
expr(n->left, &l);
expr(n->right, &r);
if(r.type != TINT)
error("bad type for []");
switch(l.type) {
default:
error("lhs[] has bad type");
case TINT:
indir(cormap, l.nstore.u0.sival+(r.nstore.u0.sival*fsize[l.nstore.fmt]), l.nstore.fmt, res);
res->nstore.comt = l.nstore.comt;
res->nstore.fmt = l.nstore.fmt;
break;
case TLIST:
nthelem(l.nstore.u0.sl, r.nstore.u0.sival, res);
break;
case TSTRING:
res->nstore.u0.sival = 0;
if(r.nstore.u0.sival >= 0 && r.nstore.u0.sival < l.nstore.u0.sstring->len) {
int xx8;
xx8 = r.nstore.u0.sival;
res->nstore.u0.sival = l.nstore.u0.sstring->string[xx8];
}
res->op = OCONST;
res->type = TINT;
res->nstore.fmt = 'c';
break;
}
}
void
oappend(Node *n, Node *res)
{
Node r, l;
expr(n->left, &l);
expr(n->right, &r);
if(l.type != TLIST)
error("must append to list");
append(res, &l, &r);
}
void
odelete(Node *n, Node *res)
{
Node l, r;
expr(n->left, &l);
expr(n->right, &r);
if(l.type != TLIST)
error("must delete from list");
if(r.type != TINT)
error("delete index must be integer");
delete(l.nstore.u0.sl, r.nstore.u0.sival, res);
}
void
ohead(Node *n, Node *res)
{
Node l;
expr(n->left, &l);
if(l.type != TLIST)
error("head needs list");
res->op = OCONST;
if(l.nstore.u0.sl) {
res->type = l.nstore.u0.sl->type;
res->nstore = l.nstore.u0.sl->lstore;
}
else {
res->type = TLIST;
res->nstore.u0.sl = 0;
}
}
void
otail(Node *n, Node *res)
{
Node l;
expr(n->left, &l);
if(l.type != TLIST)
error("tail needs list");
res->op = OCONST;
res->type = TLIST;
if(l.nstore.u0.sl)
res->nstore.u0.sl = l.nstore.u0.sl->next;
else
res->nstore.u0.sl = 0;
}
void
oconst(Node *n, Node *res)
{
res->op = OCONST;
res->type = n->type;
res->nstore = n->nstore;
res->nstore.comt = n->nstore.comt;
}
void
oname(Node *n, Node *res)
{
Value *v;
v = n->sym->v;
if(v->set == 0)
error("%s used but not set", n->sym->name);
res->op = OCONST;
res->type = v->type;
res->nstore = v->vstore;
res->nstore.comt = v->vstore.comt;
}
void
octruct(Node *n, Node *res)
{
res->op = OCONST;
res->type = TLIST;
res->nstore.u0.sl = construct(n->left);
}
void
oasgn(Node *n, Node *res)
{
Node *lp, r;
Value *v;
lp = n->left;
switch(lp->op) {
case OINDM:
windir(cormap, lp->left, n->right, res);
break;
case OINDC:
windir(symmap, lp->left, n->right, res);
break;
default:
chklval(lp);
v = lp->sym->v;
expr(n->right, &r);
v->set = 1;
v->type = r.type;
v->vstore = r.nstore;
res->op = OCONST;
res->type = v->type;
res->nstore = v->vstore;
res->nstore.comt = v->vstore.comt;
}
}
void
oadd(Node *n, Node *res)
{
Node l, r;
expr(n->left, &l);
expr(n->right, &r);
res->nstore.fmt = l.nstore.fmt;
res->op = OCONST;
res->type = TFLOAT;
switch(l.type) {
default:
error("bad lhs type +");
case TINT:
switch(r.type) {
case TINT:
res->type = TINT;
res->nstore.u0.sival = l.nstore.u0.sival+r.nstore.u0.sival;
break;
case TFLOAT:
res->nstore.u0.sfval = l.nstore.u0.sival+r.nstore.u0.sfval;
break;
default:
error("bad rhs type +");
}
break;
case TFLOAT:
switch(r.type) {
case TINT:
res->nstore.u0.sfval = l.nstore.u0.sfval+r.nstore.u0.sival;
break;
case TFLOAT:
res->nstore.u0.sfval = l.nstore.u0.sfval+r.nstore.u0.sfval;
break;
default:
error("bad rhs type +");
}
break;
case TSTRING:
if(r.type == TSTRING) {
res->type = TSTRING;
res->nstore.fmt = 's';
res->nstore.u0.sstring = stradd(l.nstore.u0.sstring, r.nstore.u0.sstring);
break;
}
error("bad rhs for +");
case TLIST:
res->type = TLIST;
switch(r.type) {
case TLIST:
res->nstore.u0.sl = addlist(l.nstore.u0.sl, r.nstore.u0.sl);
break;
default:
r.left = 0;
r.right = 0;
res->nstore.u0.sl = addlist(l.nstore.u0.sl, construct(&r));
break;
}
}
}
void
osub(Node *n, Node *res)
{
Node l, r;
expr(n->left, &l);
expr(n->right, &r);
res->nstore.fmt = l.nstore.fmt;
res->op = OCONST;
res->type = TFLOAT;
switch(l.type) {
default:
error("bad lhs type -");
case TINT:
switch(r.type) {
case TINT:
res->type = TINT;
res->nstore.u0.sival = l.nstore.u0.sival-r.nstore.u0.sival;
break;
case TFLOAT:
res->nstore.u0.sfval = l.nstore.u0.sival-r.nstore.u0.sfval;
break;
default:
error("bad rhs type -");
}
break;
case TFLOAT:
switch(r.type) {
case TINT:
res->nstore.u0.sfval = l.nstore.u0.sfval-r.nstore.u0.sival;
break;
case TFLOAT:
res->nstore.u0.sfval = l.nstore.u0.sfval-r.nstore.u0.sfval;
break;
default:
error("bad rhs type -");
}
break;
}
}
void
omul(Node *n, Node *res)
{
Node l, r;
expr(n->left, &l);
expr(n->right, &r);
res->nstore.fmt = l.nstore.fmt;
res->op = OCONST;
res->type = TFLOAT;
switch(l.type) {
default:
error("bad lhs type *");
case TINT:
switch(r.type) {
case TINT:
res->type = TINT;
res->nstore.u0.sival = l.nstore.u0.sival*r.nstore.u0.sival;
break;
case TFLOAT:
res->nstore.u0.sfval = l.nstore.u0.sival*r.nstore.u0.sfval;
break;
default:
error("bad rhs type *");
}
break;
case TFLOAT:
switch(r.type) {
case TINT:
res->nstore.u0.sfval = l.nstore.u0.sfval*r.nstore.u0.sival;
break;
case TFLOAT:
res->nstore.u0.sfval = l.nstore.u0.sfval*r.nstore.u0.sfval;
break;
default:
error("bad rhs type *");
}
break;
}
}
void
odiv(Node *n, Node *res)
{
Node l, r;
expr(n->left, &l);
expr(n->right, &r);
res->nstore.fmt = l.nstore.fmt;
res->op = OCONST;
res->type = TFLOAT;
switch(l.type) {
default:
error("bad lhs type /");
case TINT:
switch(r.type) {
case TINT:
res->type = TINT;
if(r.nstore.u0.sival == 0)
error("zero divide");
res->nstore.u0.sival = l.nstore.u0.sival/r.nstore.u0.sival;
break;
case TFLOAT:
if(r.nstore.u0.sfval == 0)
error("zero divide");
res->nstore.u0.sfval = l.nstore.u0.sival/r.nstore.u0.sfval;
break;
default:
error("bad rhs type /");
}
break;
case TFLOAT:
switch(r.type) {
case TINT:
res->nstore.u0.sfval = l.nstore.u0.sfval/r.nstore.u0.sival;
break;
case TFLOAT:
res->nstore.u0.sfval = l.nstore.u0.sfval/r.nstore.u0.sfval;
break;
default:
error("bad rhs type /");
}
break;
}
}
void
omod(Node *n, Node *res)
{
Node l, r;
expr(n->left, &l);
expr(n->right, &r);
res->nstore.fmt = l.nstore.fmt;
res->op = OCONST;
res->type = TINT;
if(l.type != TINT || r.type != TINT)
error("bad expr type %");
res->nstore.u0.sival = l.nstore.u0.sival%r.nstore.u0.sival;
}
void
olsh(Node *n, Node *res)
{
Node l, r;
expr(n->left, &l);
expr(n->right, &r);
res->nstore.fmt = l.nstore.fmt;
res->op = OCONST;
res->type = TINT;
if(l.type != TINT || r.type != TINT)
error("bad expr type <<");
res->nstore.u0.sival = l.nstore.u0.sival<<r.nstore.u0.sival;
}
void
orsh(Node *n, Node *res)
{
Node l, r;
expr(n->left, &l);
expr(n->right, &r);
res->nstore.fmt = l.nstore.fmt;
res->op = OCONST;
res->type = TINT;
if(l.type != TINT || r.type != TINT)
error("bad expr type >>");
res->nstore.u0.sival = l.nstore.u0.sival>>r.nstore.u0.sival;
}
void
olt(Node *n, Node *res)
{
Node l, r;
expr(n->left, &l);
expr(n->right, &r);
res->nstore.fmt = l.nstore.fmt;
res->op = OCONST;
res->type = TINT;
switch(l.type) {
default:
error("bad lhs type <");
case TINT:
switch(r.type) {
case TINT:
res->nstore.u0.sival = l.nstore.u0.sival < r.nstore.u0.sival;
break;
case TFLOAT:
res->nstore.u0.sival = l.nstore.u0.sival < r.nstore.u0.sfval;
break;
default:
error("bad rhs type <");
}
break;
case TFLOAT:
switch(r.type) {
case TINT:
res->nstore.u0.sival = l.nstore.u0.sfval < r.nstore.u0.sival;
break;
case TFLOAT:
res->nstore.u0.sival = l.nstore.u0.sfval < r.nstore.u0.sfval;
break;
default:
error("bad rhs type <");
}
break;
}
}
void
ogt(Node *n, Node *res)
{
Node l, r;
expr(n->left, &l);
expr(n->right, &r);
res->nstore.fmt = 'D';
res->op = OCONST;
res->type = TINT;
switch(l.type) {
default:
error("bad lhs type >");
case TINT:
switch(r.type) {
case TINT:
res->nstore.u0.sival = l.nstore.u0.sival > r.nstore.u0.sival;
break;
case TFLOAT:
res->nstore.u0.sival = l.nstore.u0.sival > r.nstore.u0.sfval;
break;
default:
error("bad rhs type >");
}
break;
case TFLOAT:
switch(r.type) {
case TINT:
res->nstore.u0.sival = l.nstore.u0.sfval > r.nstore.u0.sival;
break;
case TFLOAT:
res->nstore.u0.sival = l.nstore.u0.sfval > r.nstore.u0.sfval;
break;
default:
error("bad rhs type >");
}
break;
}
}
void
oleq(Node *n, Node *res)
{
Node l, r;
expr(n->left, &l);
expr(n->right, &r);
res->nstore.fmt = 'D';
res->op = OCONST;
res->type = TINT;
switch(l.type) {
default:
error("bad expr type <=");
case TINT:
switch(r.type) {
case TINT:
res->nstore.u0.sival = l.nstore.u0.sival <= r.nstore.u0.sival;
break;
case TFLOAT:
res->nstore.u0.sival = l.nstore.u0.sival <= r.nstore.u0.sfval;
break;
default:
error("bad expr type <=");
}
break;
case TFLOAT:
switch(r.type) {
case TINT:
res->nstore.u0.sival = l.nstore.u0.sfval <= r.nstore.u0.sival;
break;
case TFLOAT:
res->nstore.u0.sival = l.nstore.u0.sfval <= r.nstore.u0.sfval;
break;
default:
error("bad expr type <=");
}
break;
}
}
void
ogeq(Node *n, Node *res)
{
Node l, r;
expr(n->left, &l);
expr(n->right, &r);
res->nstore.fmt = 'D';
res->op = OCONST;
res->type = TINT;
switch(l.type) {
default:
error("bad lhs type >=");
case TINT:
switch(r.type) {
case TINT:
res->nstore.u0.sival = l.nstore.u0.sival >= r.nstore.u0.sival;
break;
case TFLOAT:
res->nstore.u0.sival = l.nstore.u0.sival >= r.nstore.u0.sfval;
break;
default:
error("bad rhs type >=");
}
break;
case TFLOAT:
switch(r.type) {
case TINT:
res->nstore.u0.sival = l.nstore.u0.sfval >= r.nstore.u0.sival;
break;
case TFLOAT:
res->nstore.u0.sival = l.nstore.u0.sfval >= r.nstore.u0.sfval;
break;
default:
error("bad rhs type >=");
}
break;
}
}
void
oeq(Node *n, Node *res)
{
Node l, r;
expr(n->left, &l);
expr(n->right, &r);
res->nstore.fmt = 'D';
res->op = OCONST;
res->type = TINT;
res->nstore.u0.sival = 0;
switch(l.type) {
default:
break;
case TINT:
switch(r.type) {
case TINT:
res->nstore.u0.sival = l.nstore.u0.sival == r.nstore.u0.sival;
break;
case TFLOAT:
res->nstore.u0.sival = l.nstore.u0.sival == r.nstore.u0.sfval;
break;
default:
break;
}
break;
case TFLOAT:
switch(r.type) {
case TINT:
res->nstore.u0.sival = l.nstore.u0.sfval == r.nstore.u0.sival;
break;
case TFLOAT:
res->nstore.u0.sival = l.nstore.u0.sfval == r.nstore.u0.sfval;
break;
default:
break;
}
break;
case TSTRING:
if(r.type == TSTRING) {
res->nstore.u0.sival = scmp(r.nstore.u0.sstring, l.nstore.u0.sstring);
break;
}
break;
case TLIST:
if(r.type == TLIST) {
res->nstore.u0.sival = listcmp(l.nstore.u0.sl, r.nstore.u0.sl);
break;
}
break;
}
if(n->op == ONEQ)
res->nstore.u0.sival = !res->nstore.u0.sival;
}
void
oland(Node *n, Node *res)
{
Node l, r;
expr(n->left, &l);
expr(n->right, &r);
res->nstore.fmt = l.nstore.fmt;
res->op = OCONST;
res->type = TINT;
if(l.type != TINT || r.type != TINT)
error("bad expr type &");
res->nstore.u0.sival = l.nstore.u0.sival&r.nstore.u0.sival;
}
void
oxor(Node *n, Node *res)
{
Node l, r;
expr(n->left, &l);
expr(n->right, &r);
res->nstore.fmt = l.nstore.fmt;
res->op = OCONST;
res->type = TINT;
if(l.type != TINT || r.type != TINT)
error("bad expr type ^");
res->nstore.u0.sival = l.nstore.u0.sival^r.nstore.u0.sival;
}
void
olor(Node *n, Node *res)
{
Node l, r;
expr(n->left, &l);
expr(n->right, &r);
res->nstore.fmt = l.nstore.fmt;
res->op = OCONST;
res->type = TINT;
if(l.type != TINT || r.type != TINT)
error("bad expr type |");
res->nstore.u0.sival = l.nstore.u0.sival|r.nstore.u0.sival;
}
void
ocand(Node *n, Node *res)
{
Node l, r;
res->op = OCONST;
res->type = TINT;
res->nstore.u0.sival = 0;
expr(n->left, &l);
res->nstore.fmt = l.nstore.fmt;
if(boolx(&l) == 0)
return;
expr(n->right, &r);
if(boolx(&r) == 0)
return;
res->nstore.u0.sival = 1;
}
void
onot(Node *n, Node *res)
{
Node l;
res->op = OCONST;
res->type = TINT;
res->nstore.u0.sival = 0;
expr(n->left, &l);
if(boolx(&l) == 0)
res->nstore.u0.sival = 1;
}
void
ocor(Node *n, Node *res)
{
Node l, r;
res->op = OCONST;
res->type = TINT;
res->nstore.u0.sival = 0;
expr(n->left, &l);
if(boolx(&l)) {
res->nstore.u0.sival = 1;
return;
}
expr(n->right, &r);
if(boolx(&r)) {
res->nstore.u0.sival = 1;
return;
}
}
void
oeinc(Node *n, Node *res)
{
Value *v;
chklval(n->left);
v = n->left->sym->v;
res->op = OCONST;
res->type = v->type;
switch(v->type) {
case TINT:
if(n->op == OEDEC)
v->vstore.u0.sival -= fmtsize(v);
else
v->vstore.u0.sival += fmtsize(v);
break;
case TFLOAT:
if(n->op == OEDEC)
v->vstore.u0.sfval--;
else
v->vstore.u0.sfval++;
break;
default:
error("bad type for pre --/++");
}
res->nstore = v->vstore;
}
void
opinc(Node *n, Node *res)
{
Value *v;
chklval(n->left);
v = n->left->sym->v;
res->op = OCONST;
res->type = v->type;
res->nstore = v->vstore;
switch(v->type) {
case TINT:
if(n->op == OPDEC)
v->vstore.u0.sival -= fmtsize(v);
else
v->vstore.u0.sival += fmtsize(v);
break;
case TFLOAT:
if(n->op == OPDEC)
v->vstore.u0.sfval--;
else
v->vstore.u0.sfval++;
break;
default:
error("bad type for post --/++");
}
}
void
ocall(Node *n, Node *res)
{
Lsym *s;
Rplace *rsav;
res->op = OCONST;
res->type = TLIST;
res->nstore.u0.sl = 0;
chklval(n->left);
s = n->left->sym;
if(s->builtin) {
(*s->builtin)(res, n->right);
return;
}
if(s->proc == 0)
error("no function %s", s->name);
rsav = ret;
call(s->name, n->right, s->proc->left, s->proc->right, res);
ret = rsav;
}
void
ofmt(Node *n, Node *res)
{
expr(n->left, res);
res->nstore.fmt = n->right->nstore.u0.sival;
}
void
owhat(Node *n, Node *res)
{
res->op = OCONST;
res->type = TLIST;
res->nstore.u0.sl = 0;
whatis(n->sym);
}
void (*expop[])(Node*, Node*) =
{
oname,
oconst,
omul,
odiv,
omod,
oadd,
osub,
orsh,
olsh,
olt,
ogt,
oleq,
ogeq,
oeq,
oeq,
oland,
oxor,
olor,
ocand,
ocor,
oasgn,
oindm,
oeinc,
oeinc,
opinc,
opinc,
onot,
0,
0,
olist,
ocall,
octruct,
0,
0,
ohead,
otail,
oappend,
0,
oindex,
oindc,
odot,
0,
oframe,
0,
odelete,
ocast,
ofmt,
oeval,
owhat,
};