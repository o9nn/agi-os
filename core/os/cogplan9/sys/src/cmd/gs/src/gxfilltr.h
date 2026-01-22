private int
TEMPLATE_spot_into_trapezoids (line_list *ll, fixed band_mask)
{
const fill_options fo = *ll->fo;
int rule = fo.rule;
const fixed y_limit = fo.ymax;
active_line *yll = ll->y_list;
fixed y;
int code;
const bool all_bands = fo.is_spotan;
if (yll == 0)
return 0;
y = yll->start.y;
ll->x_list = 0;
ll->x_head.x_current = min_fixed;
ll->margin_set0.y = fixed_pixround(y) - fixed_half;
ll->margin_set1.y = fixed_pixround(y) - fixed_1 - fixed_half;
while (1) {
fixed y1;
active_line *alp, *plp = NULL;
bool covering_pixel_centers;
INCR(iter);
while (yll != 0 && yll->start.y == y) {
active_line *ynext = yll->next;
ll->y_list = ynext;
if (ll->y_line == yll)
ll->y_line = ynext;
if (ynext != NULL)
ynext->prev = NULL;
if (yll->direction == DIR_HORIZONTAL) {
if (!PSEUDO_RASTERIZATION) {
int yi = fixed2int_pixround(y - (!FILL_ADJUST ? 0 : fo.adjust_below));
int xi, wi;
if (yll->start.x <= yll->end.x) {
xi = fixed2int_pixround(yll->start.x - (!FILL_ADJUST ? 0 : fo.adjust_left));
wi = fixed2int_pixround(yll->end.x + (!FILL_ADJUST ? 0 : fo.adjust_right)) - xi;
} else {
xi = fixed2int_pixround(yll->end.x - (!FILL_ADJUST ? 0 : fo.adjust_left));
wi = fixed2int_pixround(yll->start.x + (!FILL_ADJUST ? 0 : fo.adjust_right)) - xi;
}
VD_RECT(xi, yi, wi, 1, VD_TRAP_COLOR);
code = LOOP_FILL_RECTANGLE_DIRECT(&fo, xi, yi, wi, 1);
if (code < 0)
return code;
} else if (PSEUDO_RASTERIZATION)
insert_h_new(yll, ll);
} else
insert_x_new(yll, ll);
yll = ynext;
}
if (ll->x_list == 0) {
if (yll == 0)
break;
y = yll->start.y;
ll->h_list1 = ll->h_list0;
ll->h_list0 = 0;
continue;
}
if (vd_enabled) {
vd_circle(0, y, 3, RGB(255, 0, 0));
y += 0;
}
y1 = (yll != 0 ? yll->start.y : ll->y_break);
{
fixed y_band = y | ~band_mask;
if (y1 > y_band)
y1 = y_band + 1;
}
for (alp = ll->x_list; alp != 0; alp = alp->next) {
if (alp->end.y < y1)
y1 = alp->end.y;
}
# ifdef DEBUG
if (gs_debug_c('F')) {
dlprintf2("[F]before loop: y=%f y1=%f:\n",
fixed2float(y), fixed2float(y1));
print_line_list(ll->x_list);
}
# endif
if (y == y1) {
code = process_h_segments(ll, y);
if (code < 0)
return code;
move_al_by_y(ll, y1);
if (code > 0) {
yll = ll->y_list;
continue;
}
}
if (y >= y_limit)
break;
covering_pixel_centers = COVERING_PIXEL_CENTERS(y, y1,
(!FILL_ADJUST ? 0 : fo.adjust_below),
(!FILL_ADJUST ? 0 : fo.adjust_above));
if (y != y1) {
intersect_al(ll, y, &y1, (covering_pixel_centers ? 1 : -1), all_bands);
covering_pixel_centers = COVERING_PIXEL_CENTERS(y, y1,
(!FILL_ADJUST ? 0 : fo.adjust_below),
(!FILL_ADJUST ? 0 : fo.adjust_above));
}
if (PSEUDO_RASTERIZATION) {
code = start_margin_set(fo.dev, ll, y1);
if (code < 0)
return code;
}
if (covering_pixel_centers || all_bands) {
int inside = 0;
active_line *flp = NULL;
INCR(band);
for (alp = ll->x_list; alp != 0; alp = alp->next) {
int code;
print_al("step", alp);
INCR(band_step);
if (!INSIDE_PATH_P(inside, rule)) {
inside += alp->direction;
if (INSIDE_PATH_P(inside, rule))
flp = alp;
continue;
}
inside += alp->direction;
if (INSIDE_PATH_P(inside, rule))
continue;
if (alp->next != NULL &&
alp->x_current == alp->next->x_current &&
alp->x_next == alp->next->x_next) {
alp = alp->next;
inside += alp->direction;
continue;
}
INCR(band_fill);
if (FILL_ADJUST && !(flp->end.x == flp->start.x && alp->end.x == alp->start.x) &&
(fo.adjust_below | fo.adjust_above) != 0) {
if (FILL_DIRECT)
code = slant_into_trapezoids__fd(ll, flp, alp, y, y1);
else
code = slant_into_trapezoids__nd(ll, flp, alp, y, y1);
} else {
fixed ybot = max(y, fo.pbox->p.y);
fixed ytop = min(y1, fo.pbox->q.y);
if (IS_SPOTAN) {
code = gx_san_trap_store((gx_device_spot_analyzer *)fo.dev,
y, y1, flp->x_current, alp->x_current, flp->x_next, alp->x_next,
flp->pseg, alp->pseg, flp->direction, alp->direction);
} else {
if (flp->end.x == flp->start.x && alp->end.x == alp->start.x) {
if (FILL_ADJUST) {
ybot = max(y - fo.adjust_below, fo.pbox->p.y);
ytop = min(y1 + fo.adjust_above, fo.pbox->q.y);
}
if (ytop > ybot) {
int yi = fixed2int_pixround(ybot);
int hi = fixed2int_pixround(ytop) - yi;
int xli = fixed2int_var_pixround(flp->end.x - (!FILL_ADJUST ? 0 : fo.adjust_left));
int xi = fixed2int_var_pixround(alp->end.x + (!FILL_ADJUST ? 0 : fo.adjust_right));
if (PSEUDO_RASTERIZATION && xli == xi) {
fixed xx = int2fixed(xli);
if (xx - flp->end.x < alp->end.x - xx)
++xi;
else
--xli;
}
vd_rect(flp->end.x, y, alp->end.x, y1, 1, VD_TRAP_COLOR);
code = LOOP_FILL_RECTANGLE_DIRECT(&fo, xli, yi, xi - xli, hi);
} else
code = 0;
} else if (ybot < ytop) {
gs_fixed_edge le, re;
le.start = flp->start;
le.end = flp->end;
re.start = alp->start;
re.end = alp->end;
vd_quad(flp->x_current, ybot, alp->x_current, ybot, alp->x_next, ytop, flp->x_next, ytop, 1, VD_TRAP_COLOR);
if (PSEUDO_RASTERIZATION) {
int flags = ftf_pseudo_rasterization;
if (flp->start.x == alp->start.x && flp->start.y == y && alp->start.y == y)
flags |= ftf_peak0;
if (flp->end.x == alp->end.x && flp->end.y == y1 && alp->end.y == y1)
flags |= ftf_peak0;
if (FILL_DIRECT)
code = gx_fill_trapezoid_cf_fd(fo.dev, &le, &re, ybot, ytop, flags, fo.pdevc, fo.lop);
else
code = gx_fill_trapezoid_cf_nd(fo.dev, &le, &re, ybot, ytop, flags, fo.pdevc, fo.lop);
} else
code = fo.fill_trap(fo.dev,
&le, &re, ybot, ytop, false, fo.pdevc, fo.lop);
} else
code = 0;
}
if (PSEUDO_RASTERIZATION) {
if (code < 0)
return code;
code = complete_margin(ll, flp, alp, y, y1);
if (code < 0)
return code;
code = margin_interior(ll, flp, alp, y, y1);
if (code < 0)
return code;
code = add_margin(ll, flp, alp, y, y1);
if (code < 0)
return code;
code = process_h_lists(ll, plp, flp, alp, y, y1);
plp = alp;
}
}
if (code < 0)
return code;
}
} else {
if (PSEUDO_RASTERIZATION) {
active_line *flp = NULL;
int inside = 0;
for (alp = ll->x_list; alp != 0; alp = alp->next) {
if (!INSIDE_PATH_P(inside, rule)) {
inside += alp->direction;
if (INSIDE_PATH_P(inside, rule))
flp = alp;
continue;
}
inside += alp->direction;
if (INSIDE_PATH_P(inside, rule))
continue;
code = continue_margin(ll, flp, alp, y, y1);
if (code < 0)
return code;
code = process_h_lists(ll, plp, flp, alp, y, y1);
plp = alp;
if (code < 0)
return code;
}
}
}
if (PSEUDO_RASTERIZATION && plp != 0) {
code = process_h_lists(ll, plp, 0, 0, y, y1);
if (code < 0)
return code;
}
move_al_by_y(ll, y1);
ll->h_list1 = ll->h_list0;
ll->h_list0 = 0;
y = y1;
}
if (PSEUDO_RASTERIZATION) {
code = process_h_lists(ll, 0, 0, 0, y, y + 1 );
if (code < 0)
return code;
code = close_margins(fo.dev, ll, &ll->margin_set1);
if (code < 0)
return code;
return close_margins(fo.dev, ll, &ll->margin_set0);
}
return 0;
}