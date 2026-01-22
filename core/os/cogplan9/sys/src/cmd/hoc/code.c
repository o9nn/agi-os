#include <u.h>
#include <libc.h>
#include <bio.h>
#include "hoc.h"
#include "y.tab.h"
#define	NSTACK	256
static Datum stack[NSTACK];
static Datum *stackp;
#define	NPROG	2000
Inst	prog[NPROG];
Inst	*progp;
Inst	*pc;
Inst	*progbase = prog;
int	returning;
int	indef;
typedef struct Frame {
Symbol	*sp;
Inst	*retpc;
Datum	*argn;
int	nargs;
} Frame;
#define	NFRAME	100
Frame	frame[NFRAME];
Frame	*fp;
void
initcode(void)
{
progp = progbase;
stackp = stack;
fp = frame;
returning = 0;
indef = 0;
}
void
push(Datum d)
{
if (stackp >= &stack[NSTACK])
execerror("stack too deep", 0);
*stackp++ = d;
}
Datum
pop(void)
{
if (stackp == stack)
execerror("stack underflow", 0);
return *--stackp;
}
void
xpop(void)
{
if (stackp == stack)
execerror("stack underflow", (char *)0);
--stackp;
}
void
constpush(void)
{
Datum d;
d.val = ((Symbol *)*pc++)->u.val;
push(d);
}
void
varpush(void)
{
Datum d;
d.sym = (Symbol *)(*pc++);
push(d);
}
void
whilecode(void)
{
Datum d;
Inst *savepc = pc;
execute(savepc+2);
d = pop();
while (d.val) {
execute(*((Inst **)(savepc)));
if (returning)
break;
execute(savepc+2);
d = pop();
}
if (!returning)
pc = *((Inst **)(savepc+1));
}
void
forcode(void)
{
Datum d;
Inst *savepc = pc;
execute(savepc+4);
pop();
execute(*((Inst **)(savepc)));
d = pop();
while (d.val) {
execute(*((Inst **)(savepc+2)));
if (returning)
break;
execute(*((Inst **)(savepc+1)));
pop();
execute(*((Inst **)(savepc)));
d = pop();
}
if (!returning)
pc = *((Inst **)(savepc+3));
}
void
ifcode(void)
{
Datum d;
Inst *savepc = pc;
execute(savepc+3);
d = pop();
if (d.val)
execute(*((Inst **)(savepc)));
else if (*((Inst **)(savepc+1)))
execute(*((Inst **)(savepc+1)));
if (!returning)
pc = *((Inst **)(savepc+2));
}
void
define(Symbol* sp, Formal *f)
{
Fndefn *fd;
int n;
fd = emalloc(sizeof(Fndefn));
fd->code = progbase;
progbase = progp;
fd->formals = f;
for(n=0; f; f=f->next)
n++;
fd->nargs = n;
sp->u.defn = fd;
}
void
call(void)
{
Formal *f;
Datum *arg;
Saveval *s;
int i;
Symbol *sp = (Symbol *)pc[0];
if (fp >= &frame[NFRAME-1])
execerror(sp->name, "call nested too deeply");
fp++;
fp->sp = sp;
fp->nargs = (int)(uintptr)pc[1];
fp->retpc = pc + 2;
fp->argn = stackp - 1;
if(fp->nargs != sp->u.defn->nargs)
execerror(sp->name, "called with wrong number of arguments");
f = sp->u.defn->formals;
arg = stackp - fp->nargs;
while(f){
s = emalloc(sizeof(Saveval));
s->val = f->sym->u;
s->type = f->sym->type;
s->next = f->save;
f->save = s;
f->sym->u.val = arg->val;
f->sym->type = VAR;
f = f->next;
arg++;
}
for (i = 0; i < fp->nargs; i++)
pop();
execute(sp->u.defn->code);
returning = 0;
}
void
restore(Symbol *sp)
{
Formal *f;
Saveval *s;
f = sp->u.defn->formals;
while(f){
s = f->save;
if(s == 0)
break;
f->sym->u = s->val;
f->sym->type = s->type;
f->save = s->next;
free(s);
f = f->next;
}
}
void
restoreall(void)
{
while(fp>=frame && fp->sp){
restore(fp->sp);
--fp;
}
fp = frame;
}
static void
ret(void)
{
restore(fp->sp);
pc = (Inst *)fp->retpc;
--fp;
returning = 1;
}
void
funcret(void)
{
Datum d;
if (fp->sp->type == PROCEDURE)
execerror(fp->sp->name, "(proc) returns value");
d = pop();
ret();
push(d);
}
void
procret(void)
{
if (fp->sp->type == FUNCTION)
execerror(fp->sp->name,
"(func) returns no value");
ret();
}
void
bltin(void)
{
Datum d;
d = pop();
d.val = (*(double (*)(double))*pc++)(d.val);
push(d);
}
void
add(void)
{
Datum d1, d2;
d2 = pop();
d1 = pop();
d1.val += d2.val;
push(d1);
}
void
sub(void)
{
Datum d1, d2;
d2 = pop();
d1 = pop();
d1.val -= d2.val;
push(d1);
}
void
mul(void)
{
Datum d1, d2;
d2 = pop();
d1 = pop();
d1.val *= d2.val;
push(d1);
}
void
div(void)
{
Datum d1, d2;
d2 = pop();
if (d2.val == 0.0)
execerror("division by zero", (char *)0);
d1 = pop();
d1.val /= d2.val;
push(d1);
}
void
mod(void)
{
Datum d1, d2;
d2 = pop();
if (d2.val == 0.0)
execerror("division by zero", (char *)0);
d1 = pop();
d1.val = fmod(d1.val, d2.val);
push(d1);
}
void
negate(void)
{
Datum d;
d = pop();
d.val = -d.val;
push(d);
}
void
verify(Symbol* s)
{
if (s->type != VAR && s->type != UNDEF)
execerror("attempt to evaluate non-variable", s->name);
if (s->type == UNDEF)
execerror("undefined variable", s->name);
}
void
eval(void)
{
Datum d;
d = pop();
verify(d.sym);
d.val = d.sym->u.val;
push(d);
}
void
preinc(void)
{
Datum d;
d.sym = (Symbol *)(*pc++);
verify(d.sym);
d.val = d.sym->u.val += 1.0;
push(d);
}
void
predec(void)
{
Datum d;
d.sym = (Symbol *)(*pc++);
verify(d.sym);
d.val = d.sym->u.val -= 1.0;
push(d);
}
void
postinc(void)
{
Datum d;
double v;
d.sym = (Symbol *)(*pc++);
verify(d.sym);
v = d.sym->u.val;
d.sym->u.val += 1.0;
d.val = v;
push(d);
}
void
postdec(void)
{
Datum d;
double v;
d.sym = (Symbol *)(*pc++);
verify(d.sym);
v = d.sym->u.val;
d.sym->u.val -= 1.0;
d.val = v;
push(d);
}
void
gt(void)
{
Datum d1, d2;
d2 = pop();
d1 = pop();
d1.val = (double)(d1.val > d2.val);
push(d1);
}
void
lt(void)
{
Datum d1, d2;
d2 = pop();
d1 = pop();
d1.val = (double)(d1.val < d2.val);
push(d1);
}
void
ge(void)
{
Datum d1, d2;
d2 = pop();
d1 = pop();
d1.val = (double)(d1.val >= d2.val);
push(d1);
}
void
le(void)
{
Datum d1, d2;
d2 = pop();
d1 = pop();
d1.val = (double)(d1.val <= d2.val);
push(d1);
}
void
eq(void)
{
Datum d1, d2;
d2 = pop();
d1 = pop();
d1.val = (double)(d1.val == d2.val);
push(d1);
}
void
ne(void)
{
Datum d1, d2;
d2 = pop();
d1 = pop();
d1.val = (double)(d1.val != d2.val);
push(d1);
}
void
and(void)
{
Datum d1, d2;
d2 = pop();
d1 = pop();
d1.val = (double)(d1.val != 0.0 && d2.val != 0.0);
push(d1);
}
void
or(void)
{
Datum d1, d2;
d2 = pop();
d1 = pop();
d1.val = (double)(d1.val != 0.0 || d2.val != 0.0);
push(d1);
}
void
not(void)
{
Datum d;
d = pop();
d.val = (double)(d.val == 0.0);
push(d);
}
void
power(void)
{
Datum d1, d2;
d2 = pop();
d1 = pop();
d1.val = Pow(d1.val, d2.val);
push(d1);
}
void
assign(void)
{
Datum d1, d2;
d1 = pop();
d2 = pop();
if (d1.sym->type != VAR && d1.sym->type != UNDEF)
execerror("assignment to non-variable",
d1.sym->name);
d1.sym->u.val = d2.val;
d1.sym->type = VAR;
push(d2);
}
void
addeq(void)
{
Datum d1, d2;
d1 = pop();
d2 = pop();
if (d1.sym->type != VAR && d1.sym->type != UNDEF)
execerror("assignment to non-variable",
d1.sym->name);
d2.val = d1.sym->u.val += d2.val;
d1.sym->type = VAR;
push(d2);
}
void
subeq(void)
{
Datum d1, d2;
d1 = pop();
d2 = pop();
if (d1.sym->type != VAR && d1.sym->type != UNDEF)
execerror("assignment to non-variable",
d1.sym->name);
d2.val = d1.sym->u.val -= d2.val;
d1.sym->type = VAR;
push(d2);
}
void
muleq(void)
{
Datum d1, d2;
d1 = pop();
d2 = pop();
if (d1.sym->type != VAR && d1.sym->type != UNDEF)
execerror("assignment to non-variable",
d1.sym->name);
d2.val = d1.sym->u.val *= d2.val;
d1.sym->type = VAR;
push(d2);
}
void
diveq(void)
{
Datum d1, d2;
d1 = pop();
d2 = pop();
if (d1.sym->type != VAR && d1.sym->type != UNDEF)
execerror("assignment to non-variable",
d1.sym->name);
d2.val = d1.sym->u.val /= d2.val;
d1.sym->type = VAR;
push(d2);
}
void
modeq(void)
{
Datum d1, d2;
long x;
d1 = pop();
d2 = pop();
if (d1.sym->type != VAR && d1.sym->type != UNDEF)
execerror("assignment to non-variable",
d1.sym->name);
x = d1.sym->u.val;
x %= (long) d2.val;
d2.val = d1.sym->u.val = x;
d1.sym->type = VAR;
push(d2);
}
void
printtop(void)
{
Datum d;
static Symbol *s;
if (s == 0)
s = install("_", VAR, 0.0);
d = pop();
print("%.12g\n", d.val);
s->u.val = d.val;
}
void
prexpr(void)
{
Datum d;
d = pop();
print("%.12g ", d.val);
}
void
prstr(void)
{
print("%s", (char *) *pc++);
}
void
varread(void)
{
Datum d;
extern Biobuf *bin;
Symbol *var = (Symbol *) *pc++;
int c;
Again:
do
c = Bgetc(bin);
while(c==' ' || c=='\t' || c=='\n');
if(c == Beof){
Iseof:
if(moreinput())
goto Again;
d.val = var->u.val = 0.0;
goto Return;
}
if(strchr("+-.0123456789", c) == 0)
execerror("non-number read into", var->name);
Bungetc(bin);
if(Bgetd(bin, &var->u.val) == Beof)
goto Iseof;
else
d.val = 1.0;
Return:
var->type = VAR;
push(d);
}
Inst*
code(Inst f)
{
Inst *oprogp = progp;
if (progp >= &prog[NPROG])
execerror("program too big", (char *)0);
*progp++ = f;
return oprogp;
}
void
execute(Inst* p)
{
for (pc = p; *pc != STOP && !returning; )
(*((++pc)[-1]))();
}