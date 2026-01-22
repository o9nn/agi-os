#include "gx.h"
#include "gserrors.h"
#include "gsmatrix.h"
#include "gspaint.h"
#include "gsstate.h"
#include "gspath.h"
typedef struct {
gs_state *pgs;
const byte *data;
int width, height, raster;
int dx, dy;
int count;
} status;
#define outline_scale 4
#define step 1
private int get_pixel(const status *, int, int);
private int trace_from(status *, int, int, int);
private int add_dxdy(status *, int, int, int);
#define add_deltas(s, dx, dy, n)\
if ( (code = add_dxdy(s, dx, dy, n)) < 0 ) return code
int
gs_imagepath(gs_state * pgs, int width, int height, const byte * data)
{
status stat;
status *out = &stat;
int code, x, y;
stat.pgs = pgs;
stat.data = data;
stat.width = width;
stat.height = height;
stat.raster = (width + 7) / 8;
for (y = height - 1; y >= 0; y--)
for (x = width - 1; x >= 0; x--) {
if (get_pixel(out, x, y) && !get_pixel(out, x, y - 1) &&
(!get_pixel(out, x + 1, y) || get_pixel(out, x + 1, y - 1)) &&
!trace_from(out, x, y, 1)
) {
stat.count = 0;
stat.dx = stat.dy = 0;
if ((code = trace_from(out, x, y, 0)) < 0)
return code;
add_deltas(out, 0, 0, 1);
if ((code = gs_closepath(pgs)) < 0)
return code;
}
}
return 0;
}
private int
get_pixel(register const status * out, int x, int y)
{
if (x < 0 || x >= out->width || y < 0 || y >= out->height)
return 0;
return (out->data[y * out->raster + (x >> 3)] >> (~x & 7)) & 1;
}
private int
trace_from(register status * out, int x0, int y0, int detect)
{
int x = x0, y = y0;
int dx = -1, dy = 0;
int part = 0;
int code;
if (!detect) {
part = (get_pixel(out, x + 1, y - 1) ?
outline_scale - step : step);
code = gs_moveto(out->pgs,
x + 1 - part / (float)outline_scale,
(float)y);
if (code < 0)
return code;
}
while (1) {
int tx = dx - dy, ty = dy + dx;
if (get_pixel(out, x + tx, y + ty)) {
if (!detect) {
if (out->dx == ty && out->dy == -tx) {
#define half_scale (outline_scale / 2 - step)
out->count -= half_scale;
add_deltas(out, tx, ty, outline_scale / 2);
#undef half_scale
} else {
add_deltas(out, dx, dy, step - part);
add_deltas(out, tx, ty, outline_scale - step);
}
part = outline_scale - step;
}
x += tx, y += ty;
dx = -dy, dy += tx;
} else if (!get_pixel(out, x + dx, y + dy)) {
if (!detect) {
add_deltas(out, dx, dy, outline_scale - step - part);
add_deltas(out, ty, -tx, step);
part = step;
}
dx = dy, dy -= ty;
} else {
if (!detect) {
add_deltas(out, dx, dy, outline_scale);
}
x += dx, y += dy;
}
if (dx == -step && dy == 0 && !(tx == -step && ty == -step)) {
if (x == x0 && y == y0)
return 0;
if (detect && (y > y0 || (y == y0 && x > x0)))
return 1;
}
}
}
private int
add_dxdy(register status * out, int dx, int dy, int count)
{
if (count != 0) {
if (dx == out->dx && dy == out->dy)
out->count += count;
else {
if (out->count != 0) {
int code = gs_rlineto(out->pgs,
out->dx * out->count / (float)outline_scale,
out->dy * out->count / (float)outline_scale);
if (code < 0)
return code;
}
out->dx = dx, out->dy = dy;
out->count = count;
}
}
return 0;
}