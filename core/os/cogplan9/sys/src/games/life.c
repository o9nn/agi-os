#include <u.h>
#include <libc.h>
#include <bio.h>
#include <draw.h>
#include <event.h>
enum {
NLIFE	= 256,
PX	= 4,
BX	= PX - 1,
NADJUST	= NLIFE * NLIFE,
};
char	life[NLIFE][NLIFE];
int	row[NLIFE];
int	col[NLIFE];
char	action[18];
char	*adjust[NADJUST];
int		delay;
Point	cen;
Image	*box;
int	i0, i1, j0, j1;
int	needresize;
void	birth(int, int);
void	centerlife(void);
void	death(int, int);
int	generate(void);
int	interest(int [NLIFE], int);
void	main(int, char *[]);
int	min(int, int);
void	readlife(char *);
void	redraw(void);
void	setrules(char *);
void	window(void);
static void	reshape(void);
static void
setbox(int i, int j)
{
Point loc;
loc = Pt(cen.x + j*PX, cen.y + i*PX);
draw(screen, Rpt(loc, addpt(loc, Pt(BX, BX))), box, nil, ZP);
}
static void
clrbox(int i, int j)
{
Point loc;
loc = Pt(cen.x + j*PX, cen.y + i*PX);
draw(screen, Rpt(loc, addpt(loc, Pt(BX, BX))), display->white, nil, ZP);
}
void
setrules(char *r)
{
char *a;
for (a = action; a != &action[nelem(action)]; *a++ = *r++)
;
}
static void
g9err(Display *, char *err)
{
static int entered = 0;
fprint(2, "%s: %s (%r)\n", argv0, err);
exits(err);
}
void
usage(void)
{
fprint(2, "Usage: %s [-3o] [-r rules] file\n", argv0);
exits("usage");
}
void
idle(void)
{
int c;
while (ecanmouse())
emouse();
while (ecankbd()){
c = ekbd();
if (c  == 'q' || c == 0177)
exits(nil);
if (c >= '1' && c <= '9')
delay = (c - '0') * 100;
else if (c == '0')
delay = 1000;
}
if (needresize)
reshape();
}
void
main(int argc, char *argv[])
{
delay = 1000;
setrules(".d.d..b..d.d.d.d.d");
ARGBEGIN {
case '3':
setrules(".d.d.db.b..d.d.d.d");
break;
case 'o':
setrules(".d.d.db.b.b..d.d.d");
break;
case 'r':
setrules(EARGF(usage()));
break;
default:
usage();
} ARGEND
if (argc != 1)
usage();
initdraw(g9err, 0, argv0);
einit(Emouse|Ekeyboard);
cen = divpt(subpt(addpt(screen->r.min, screen->r.max),
Pt(NLIFE * PX, NLIFE * PX)), 2);
box  = allocimage(display, Rect(0, 0, BX, BX), RGB24, 1, DBlack);
assert(box != nil);
redraw();
readlife(argv[0]);
do {
flushimage(display, 1);
idle();
sleep(delay);
idle();
} while (generate());
exits(nil);
}
int
interest(int rc[NLIFE], int i)
{
return(rc[i-1] != 0 || rc[i] != 0 || rc[i+1] != 0);
}
#define	neighbour(di, dj, op) lp[(di)*NLIFE+(dj)] op= 2
#define	neighbours(op)\
neighbour(-1, -1, op);\
neighbour(-1,  0, op);\
neighbour(-1,  1, op);\
neighbour( 0, -1, op);\
neighbour( 0,  1, op);\
neighbour( 1, -1, op);\
neighbour( 1,  0, op);\
neighbour( 1,  1, op)
int
generate(void)
{
char *lp;
char **p, **addp, **delp;
int i, j, j0 = NLIFE, j1 = -1;
int drow[NLIFE], dcol[NLIFE];
for (j = 1; j != NLIFE - 1; j++) {
drow[j] = dcol[j] = 0;
if (interest(col, j)) {
if (j < j0)
j0 = j;
if (j1 < j)
j1 = j;
}
}
addp = adjust;
delp = &adjust[NADJUST];
for (i = 1; i != NLIFE - 1; i++)
if (interest(row, i)) {
for (j = j0, lp = &life[i][j0]; j <= j1; j++, lp++)
switch (action[*lp]) {
case 'b':
++*lp;
++drow[i];
++dcol[j];
setbox(i, j);
*addp++ = lp;
break;
case 'd':
--*lp;
--drow[i];
--dcol[j];
clrbox(i, j);
*--delp = lp;
break;
}
}
if (addp == adjust && delp == &adjust[NADJUST])
return 0;
if (delp < addp)
sysfatal("Out of space (delp < addp)");
p = adjust;
while (p != addp) {
lp = *p++;
neighbours(+);
}
p = delp;
while (p != &adjust[NADJUST]) {
lp = *p++;
neighbours(-);
}
for (i = 1; i != NLIFE - 1; i++) {
row[i] += drow[i];
col[i] += dcol[i];
}
if (row[1] || row[NLIFE-2] || col[1] || col[NLIFE-2])
centerlife();
return 1;
}
void
birth(int i, int j)
{
char *lp;
if (i < 1 || NLIFE - 1 <= i || j < 1 || NLIFE - 1 <= j ||
life[i][j] & 1)
return;
lp = &life[i][j];
++*lp;
++row[i];
++col[j];
neighbours(+);
setbox(i, j);
}
void
death(int i, int j)
{
char *lp;
if (i < 1 || NLIFE - 1 <= i || j < 1 || NLIFE - 1 <= j ||
!(life[i][j] & 1))
return;
lp = &life[i][j];
--*lp;
--row[i];
--col[j];
neighbours(-);
clrbox(i, j);
}
void
readlife(char *filename)
{
int c, i, j;
char name[256];
Biobuf *bp;
if ((bp = Bopen(filename, OREAD)) == nil) {
snprint(name, sizeof name, "/sys/games/lib/life/%s", filename);
if ((bp = Bopen(name, OREAD)) == nil)
sysfatal("can't read %s: %r", name);
}
draw(screen, screen->r, display->white, nil, ZP);
for (i = 0; i != NLIFE; i++) {
row[i] = col[i] = 0;
for (j = 0; j != NLIFE; j++)
life[i][j] = 0;
}
c = 0;
for (i = 1; i != NLIFE - 1 && c >= 0; i++) {
j = 1;
while ((c = Bgetc(bp)) >= 0 && c != '\n')
if (j != NLIFE - 1)
switch (c) {
case '.':
j++;
break;
case 'x':
birth(i, j);
j++;
break;
}
}
Bterm(bp);
centerlife();
}
int
min(int a, int b)
{
return(a < b ? a : b);
}
void
centerlife(void)
{
int i, j, di, dj, iinc, jinc, t;
window();
di = NLIFE / 2 - (i0 + i1) / 2;
if (i0 + di < 1)
di = 1 - i0;
else if (i1 + di >= NLIFE - 1)
di = NLIFE - 2 - i1;
dj = NLIFE / 2 - (j0 + j1) / 2;
if (j0 + dj < 1)
dj = 1 - j0;
else if (j1 + dj >= NLIFE - 1)
dj = NLIFE - 2 - j1;
if (di != 0 || dj != 0) {
if (di > 0) {
iinc = -1;
t = i0;
i0 = i1;
i1 = t;
} else
iinc = 1;
if (dj > 0) {
jinc = -1;
t = j0;
j0 = j1;
j1 = t;
} else
jinc = 1;
for (i = i0; i * iinc <= i1 * iinc; i += iinc)
for (j = j0; j * jinc <= j1 * jinc; j += jinc)
if (life[i][j] & 1) {
birth(i + di, j + dj);
death(i, j);
}
}
}
void
redraw(void)
{
int i, j;
window();
draw(screen, screen->r, display->white, nil, ZP);
for (i = i0; i <= i1; i++)
for (j = j0; j <= j1; j++)
if (life[i][j] & 1)
setbox(i, j);
}
void
window(void)
{
for (i0 = 1; i0 != NLIFE - 2 && row[i0] == 0; i0++)
;
for (i1 = NLIFE - 2; i1 != i0 && row[i1] == 0; --i1)
;
for (j0 = 1; j0 != NLIFE - 2 && col[j0] == 0; j0++)
;
for (j1 = NLIFE - 2; j1 != j0 && col[j1] == 0; --j1)
;
}
static void
reshape(void)
{
sleep(1000);
needresize = 0;
cen = divpt(subpt(addpt(screen->r.min, screen->r.max),
Pt(NLIFE * PX, NLIFE * PX)), 2);
redraw();
flushimage(display, 1);
}
void
eresized(int callgetwin)
{
needresize = callgetwin + 1;
if (needresize > 1 && getwindow(display, Refnone) < 0)
sysfatal("can't reattach to window: %r");
if (Dx(screen->r) == 0 || Dy(screen->r) == 0)
exits("window gone");
reshape();
}