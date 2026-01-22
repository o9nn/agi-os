private inline int
TEMPLATE_slant_into_trapezoids (const line_list *ll,
const active_line *flp, const active_line *alp, fixed y, fixed y1)
{
const fill_options * const fo = ll->fo;
int xli = fixed2int_var_pixround(flp->x_next - fo->adjust_left);
gs_fixed_edge le, re;
int code = 0;
fixed y_span_delta = _fixed_pixround_v + fo->adjust_above;
fixed y_span_limit = fo->adjust_below + fo->adjust_above;
le.start.x = flp->start.x - fo->adjust_left;
le.end.x = flp->end.x - fo->adjust_left;
re.start.x = alp->start.x + fo->adjust_right;
re.end.x = alp->end.x + fo->adjust_right;
#define ADJUSTED_Y_SPANS_PIXEL(y)\
(fixed_fraction((y) + y_span_delta) < y_span_limit)
if (le.end.x <= le.start.x) {
if (re.end.x >= re.start.x) {
le.start.y = flp->start.y - fo->adjust_below;
le.end.y = flp->end.y - fo->adjust_below;
re.start.y = alp->start.y - fo->adjust_below;
re.end.y = alp->end.y - fo->adjust_below;
code = loop_fill_trap_np(ll, &le, &re, y - fo->adjust_below, y1 - fo->adjust_below);
if (ADJUSTED_Y_SPANS_PIXEL(y1)) {
if (code < 0)
return code;
INCR(afill);
code = LOOP_FILL_RECTANGLE_DIRECT(fo,
xli, fixed2int_pixround(y1 - fo->adjust_below),
fixed2int_var_pixround(alp->x_next + fo->adjust_right) - xli, 1);
vd_rect(flp->x_next - fo->adjust_left, y1 - fo->adjust_below,
alp->x_next + fo->adjust_right, y1, 1, VD_TRAP_COLOR);
}
} else {
code = fill_slant_adjust(ll, flp, alp, y, y1);
}
} else {
if (re.end.x <= re.start.x) {
if (ADJUSTED_Y_SPANS_PIXEL(y)) {
INCR(afill);
xli = fixed2int_var_pixround(flp->x_current - fo->adjust_left);
code = LOOP_FILL_RECTANGLE_DIRECT(fo,
xli, fixed2int_pixround(y - fo->adjust_below),
fixed2int_var_pixround(alp->x_current + fo->adjust_right) - xli, 1);
vd_rect(flp->x_current - fo->adjust_left, y - fo->adjust_below,
alp->x_current + fo->adjust_right, y, 1, VD_TRAP_COLOR);
if (code < 0)
return code;
}
le.start.y = flp->start.y + fo->adjust_above;
le.end.y = flp->end.y + fo->adjust_above;
re.start.y = alp->start.y + fo->adjust_above;
re.end.y = alp->end.y + fo->adjust_above;
code = loop_fill_trap_np(ll, &le, &re, y + fo->adjust_above, y1 + fo->adjust_above);
} else {
code = fill_slant_adjust(ll, flp, alp, y, y1);
}
}
return code;
}