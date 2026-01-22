private int
TEMPLATE_spot_into_scanlines (line_list *ll, fixed band_mask)
{
const fill_options fo = *ll->fo;
active_line *yll = ll->y_list;
fixed y_limit = fo.ymax;
fixed y_frac_min =
(fo.adjust_above == fixed_0 ? fixed_half :
fixed_half + fixed_epsilon - fo.adjust_above);
fixed y_frac_max =
fixed_half + fo.adjust_below;
int y0 = fixed2int(min_fixed);
fixed y_bot = min_fixed;
fixed y_top = min_fixed;
fixed y = min_fixed;
coord_range_list_t rlist;
coord_range_t rlocal[MAX_LOCAL_ACTIVE];
int code = 0;
if (yll == 0)
return 0;
range_list_init(&rlist, rlocal, countof(rlocal), ll->memory);
ll->x_list = 0;
ll->x_head.x_current = min_fixed;
while (code >= 0) {
active_line *alp, *nlp;
fixed x;
bool new_band;
INCR(iter);
move_al_by_y(ll, y);
if (ll->x_list == 0)
y = (yll == 0 ? ll->y_break : yll->start.y);
else {
y = y_bot + fixed_1;
if (yll != 0)
y = min(y, yll->start.y);
for (alp = ll->x_list; alp != 0; alp = alp->next) {
fixed yy = max(alp->fi.y3, alp->fi.y0);
yy = max(yy, alp->end.y);
y = min(y, yy);
}
}
while (yll != 0 && yll->start.y == y) {
active_line *ynext = yll->next;
if (yll->direction == DIR_HORIZONTAL) {
} else
insert_x_new(yll, ll);
yll = ynext;
}
x = min_fixed;
for (alp = ll->x_list; alp != 0; alp = nlp) {
fixed nx;
nlp = alp->next;
e:if (alp->end.y <= y || alp->start.y == alp->end.y) {
if (end_x_line(alp, ll, true))
continue;
if (alp->more_flattened)
if (alp->end.y <= y || alp->start.y == alp->end.y)
step_al(alp, true);
goto e;
}
nx = alp->x_current = (alp->start.y >= y ? alp->start.x : AL_X_AT_Y(alp, y));
if (nx < x) {
active_line *ilp = alp;
while (nx < (ilp = ilp->prev)->x_current)
DO_NOTHING;
alp->prev->next = alp->next;
if (alp->next)
alp->next->prev = alp->prev;
if (ilp->next)
ilp->next->prev = alp;
alp->next = ilp->next;
ilp->next = alp;
alp->prev = ilp;
continue;
}
x = nx;
}
if (y > y_top || y >= y_limit) {
const coord_range_t *pcr;
for (pcr = rlist.first.next; pcr != &rlist.last;
pcr = pcr->next
) {
int x0 = pcr->rmin, x1 = pcr->rmax;
if_debug4('Q', "[Qr]draw 0x%lx: [%d,%d),%d\n", (ulong)pcr,
x0, x1, y0);
VD_RECT(x0, y0, x1 - x0, 1, VD_TRAP_COLOR);
code = LOOP_FILL_RECTANGLE_DIRECT(&fo, x0, y0, x1 - x0, 1);
if_debug3('F', "[F]drawing [%d:%d),%d\n", x0, x1, y0);
if (code < 0)
goto done;
}
range_list_reset(&rlist);
if (y >= y_limit)
break;
y0 = fixed2int(y);
if (fixed_fraction(y) < y_frac_min)
--y0;
y_bot = int2fixed(y0) + y_frac_min;
y_top = int2fixed(y0) + y_frac_max;
new_band = true;
} else
new_band = false;
if (y <= y_top) {
fixed y_min;
if (new_band) {
int inside = 0;
INCR(band);
for (alp = ll->x_list; alp != 0; alp = alp->next) {
int x0 = fixed2int_pixround(alp->x_current - fo.adjust_left);
for (;;) {
print_al("step", alp);
INCR(band_step);
inside += alp->direction;
if (!INSIDE_PATH_P(inside, fo.rule))
break;
if ((alp = alp->next) == 0)
goto out;
}
code = range_list_add(&rlist, x0,
fixed2int_rounded(alp->x_current +
fo.adjust_right));
if (code < 0)
goto done;
}
out:
y_min = min_fixed;
} else
y_min = y;
code = merge_ranges(&rlist, ll, y_min, y_top);
}
}
done:
range_list_free(&rlist);
return code;
}