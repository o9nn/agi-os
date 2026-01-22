GX_FILL_TRAPEZOID (gx_device * dev, const EDGE_TYPE * left,
const EDGE_TYPE * right, fixed ybot, fixed ytop, int flags,
const gx_device_color * pdevc, FILL_ATTRS fa)
{
const fixed ymin = fixed_pixround(ybot) + fixed_half;
const fixed ymax = fixed_pixround(ytop);
if (ymin >= ymax)
return 0;
{
int iy = fixed2int_var(ymin);
const int iy1 = fixed2int_var(ymax);
trap_line l, r;
register int rxl, rxr;
int ry;
const fixed
x0l = left->start.x, x1l = left->end.x, x0r = right->start.x,
x1r = right->end.x, dxl = x1l - x0l, dxr = x1r - x0r;
const fixed
ysl = ymin - left->start.y, ysr = ymin - right->start.y;
fixed fxl;
int code;
# if CONTIGUOUS_FILL
const bool peak0 = ((flags & 1) != 0);
const bool peak1 = ((flags & 2) != 0);
int peak_y0 = ybot + fixed_half;
int peak_y1 = ytop - fixed_half;
# endif
# if LINEAR_COLOR
int num_components = dev->color_info.num_components;
frac31 lgc[GX_DEVICE_COLOR_MAX_COMPONENTS];
int32_t lgf[GX_DEVICE_COLOR_MAX_COMPONENTS];
int32_t lgnum[GX_DEVICE_COLOR_MAX_COMPONENTS];
frac31 rgc[GX_DEVICE_COLOR_MAX_COMPONENTS];
int32_t rgf[GX_DEVICE_COLOR_MAX_COMPONENTS];
int32_t rgnum[GX_DEVICE_COLOR_MAX_COMPONENTS];
frac31 xgc[GX_DEVICE_COLOR_MAX_COMPONENTS];
int32_t xgf[GX_DEVICE_COLOR_MAX_COMPONENTS];
int32_t xgnum[GX_DEVICE_COLOR_MAX_COMPONENTS];
trap_gradient lg, rg, xg;
# else
gx_color_index cindex = pdevc->colors.pure;
dev_proc_fill_rectangle((*fill_rect)) =
dev_proc(dev, fill_rectangle);
# endif
if_debug2('z', "[z]y=[%d,%d]\n", iy, iy1);
l.h = left->end.y - left->start.y;
r.h = right->end.y - right->start.y;
l.x = x0l + (fixed_half - fixed_epsilon);
r.x = x0r + (fixed_half - fixed_epsilon);
ry = iy;
#define FILL_TRAP_RECT_INDIRECT(x,y,w,h)\
(SWAP_AXES ? gx_fill_rectangle_device_rop(y, x, h, w, pdevc, dev, fa) :\
gx_fill_rectangle_device_rop(x, y, w, h, pdevc, dev, fa))
#define FILL_TRAP_RECT_DIRECT(x,y,w,h)\
(SWAP_AXES ? (*fill_rect)(dev, y, x, h, w, cindex) :\
(*fill_rect)(dev, x, y, w, h, cindex))
#if LINEAR_COLOR
# define FILL_TRAP_RECT(x,y,w,h)\
(!(w) ? 0 : dev_proc(dev, fill_linear_color_scanline)(dev, fa, x, y, w, xg.c, xg.f, xg.num, xg.den))
#else
# define FILL_TRAP_RECT(x,y,w,h)\
(FILL_DIRECT ? FILL_TRAP_RECT_DIRECT(x,y,w,h) : FILL_TRAP_RECT_INDIRECT(x,y,w,h))
#endif
#define VD_RECT_SWAPPED(rxl, ry, rxr, iy)\
vd_rect(int2fixed(SWAP_AXES ? ry : rxl), int2fixed(SWAP_AXES ? rxl : ry),\
int2fixed(SWAP_AXES ? iy : rxr), int2fixed(SWAP_AXES ? rxr : iy),\
1, VD_RECT_COLOR);
#define YMULT_QUO(ys, tl)\
(ys < fixed_1 && tl.df < YMULT_LIMIT ? ys * tl.df / tl.h :\
fixed_mult_quo(ys, tl.df, tl.h))
#if CONTIGUOUS_FILL
#define SET_MINIMAL_WIDTH(ixl, ixr, l, r) \
if (ixl == ixr) \
if ((!peak0 || iy >= peak_y0) && (!peak1 || iy <= peak_y1)) {\
fixed x = int2fixed(ixl) + fixed_half;\
if (x - l.x < r.x - x)\
++ixr;\
else\
--ixl;\
}
#define CONNECT_RECTANGLES(ixl, ixr, rxl, rxr, iy, ry, adj1, adj2, fill)\
if (adj1 < adj2) {\
if (iy - ry > 1) {\
VD_RECT_SWAPPED(rxl, ry, rxr, iy - 1);\
code = fill(rxl, ry, rxr - rxl, iy - ry - 1);\
if (code < 0)\
goto xit;\
ry = iy - 1;\
}\
adj1 = adj2 = (adj2 + adj2) / 2;\
}
#else
#define SET_MINIMAL_WIDTH(ixl, ixr, l, r) DO_NOTHING
#define CONNECT_RECTANGLES(ixl, ixr, rxl, rxr, iy, ry, adj1, adj2, fill) DO_NOTHING
#endif
if (fixed_floor(l.x) == fixed_pixround(x1l)) {
l.di = 0, l.df = 0;
fxl = 0;
} else {
compute_dx(&l, dxl, ysl);
fxl = YMULT_QUO(ysl, l);
l.x += fxl;
}
if (fixed_floor(r.x) == fixed_pixround(x1r)) {
# if !LINEAR_COLOR
if (l.di == 0 && l.df == 0) {
rxl = fixed2int_var(l.x);
rxr = fixed2int_var(r.x);
SET_MINIMAL_WIDTH(rxl, rxr, l, r);
VD_RECT_SWAPPED(rxl, ry, rxr, iy1);
code = FILL_TRAP_RECT(rxl, ry, rxr - rxl, iy1 - ry);
goto xit;
}
# endif
r.di = 0, r.df = 0;
}
else if (dxr == dxl && fxl != 0) {
if (l.di == 0)
r.di = 0, r.df = l.df;
else
compute_dx(&r, dxr, ysr);
if (ysr == ysl && r.h == l.h)
r.x += fxl;
else
r.x += YMULT_QUO(ysr, r);
} else {
compute_dx(&r, dxr, ysr);
r.x += YMULT_QUO(ysr, r);
}
compute_ldx(&l, ysl);
compute_ldx(&r, ysr);
l.x += fixed_epsilon;
r.x += fixed_epsilon;
# if LINEAR_COLOR
# ifdef DEBUG
if (check_gradient_overflow(left, right, num_components)) {
return_error(gs_error_unregistered);
}
# endif
lg.c = lgc;
lg.f = lgf;
lg.num = lgnum;
rg.c = rgc;
rg.f = rgf;
rg.num = rgnum;
xg.c = xgc;
xg.f = xgf;
xg.num = xgnum;
init_gradient(&lg, fa, left, right, &l, ymin, num_components);
init_gradient(&rg, fa, right, left, &r, ymin, num_components);
# endif
#define rational_floor(tl)\
fixed2int_var(fixed_is_int(tl.x) && tl.xf == -tl.h ? tl.x - fixed_1 : tl.x)
#define STEP_LINE(ix, tl)\
tl.x += tl.ldi;\
if ( (tl.xf += tl.ldf) >= 0 ) tl.xf -= tl.h, tl.x++;\
ix = rational_floor(tl)
rxl = rational_floor(l);
rxr = rational_floor(r);
SET_MINIMAL_WIDTH(rxl, rxr, l, r);
while (LINEAR_COLOR ? 1 : ++iy != iy1) {
# if LINEAR_COLOR
if (rxl != rxr) {
code = set_x_gradient(&xg, &lg, &rg, &l, &r, rxl, rxr, num_components);
if (code < 0)
goto xit;
code = FILL_TRAP_RECT(rxl, iy, rxr - rxl, 1);
if (code < 0)
goto xit;
}
if (++iy == iy1)
break;
STEP_LINE(rxl, l);
STEP_LINE(rxr, r);
step_gradient(&lg, num_components);
step_gradient(&rg, num_components);
# else
register int ixl, ixr;
STEP_LINE(ixl, l);
STEP_LINE(ixr, r);
SET_MINIMAL_WIDTH(ixl, ixr, l, r);
if (ixl != rxl || ixr != rxr) {
CONNECT_RECTANGLES(ixl, ixr, rxl, rxr, iy, ry, rxr, ixl, FILL_TRAP_RECT);
CONNECT_RECTANGLES(ixl, ixr, rxl, rxr, iy, ry, ixr, rxl, FILL_TRAP_RECT);
VD_RECT_SWAPPED(rxl, ry, rxr, iy);
code = FILL_TRAP_RECT(rxl, ry, rxr - rxl, iy - ry);
if (code < 0)
goto xit;
rxl = ixl, rxr = ixr, ry = iy;
}
# endif
}
# if !LINEAR_COLOR
VD_RECT_SWAPPED(rxl, ry, rxr, iy);
code = FILL_TRAP_RECT(rxl, ry, rxr - rxl, iy - ry);
# else
code = 0;
# endif
#undef STEP_LINE
#undef SET_MINIMAL_WIDTH
#undef CONNECT_RECTANGLES
#undef FILL_TRAP_RECT
#undef FILL_TRAP_RECT_DIRECT
#undef FILL_TRAP_RECT_INRECT
#undef YMULT_QUO
#undef VD_RECT_SWAPPED
xit: if (code < 0 && FILL_DIRECT)
return_error(code);
return_if_interrupt(dev->memory);
return code;
}
}
#undef GX_FILL_TRAPEZOID
#undef CONTIGUOUS_FILL
#undef SWAP_AXES
#undef FLAGS_TYPE