#include "diff.h"
#include "cmpbuf.h"
extern int no_discards;
static int *xvec, *yvec;
static int *fdiag;
static int *bdiag;
static int too_expensive;
#define SNAKE_LIMIT 20
struct partition
{
int xmid, ymid;
int lo_minimal;
int hi_minimal;
};
static int diag PARAMS((int, int, int, int, int, struct partition *));
static struct change *add_change PARAMS((int, int, int, int, struct change *));
static struct change *build_reverse_script PARAMS((struct file_data const[]));
static struct change *build_script PARAMS((struct file_data const[]));
static void briefly_report PARAMS((int, struct file_data const[]));
static void compareseq PARAMS((int, int, int, int, int));
static void discard_confusing_lines PARAMS((struct file_data[]));
static void shift_boundaries PARAMS((struct file_data[]));
static int
diag (xoff, xlim, yoff, ylim, minimal, part)
int xoff, xlim, yoff, ylim, minimal;
struct partition *part;
{
int *const fd = fdiag;
int *const bd = bdiag;
int const *const xv = xvec;
int const *const yv = yvec;
int const dmin = xoff - ylim;
int const dmax = xlim - yoff;
int const fmid = xoff - yoff;
int const bmid = xlim - ylim;
int fmin = fmid, fmax = fmid;
int bmin = bmid, bmax = bmid;
int c;
int odd = (fmid - bmid) & 1;
fd[fmid] = xoff;
bd[bmid] = xlim;
for (c = 1;; ++c)
{
int d;
int big_snake = 0;
fmin > dmin ? fd[--fmin - 1] = -1 : ++fmin;
fmax < dmax ? fd[++fmax + 1] = -1 : --fmax;
for (d = fmax; d >= fmin; d -= 2)
{
int x, y, oldx, tlo = fd[d - 1], thi = fd[d + 1];
if (tlo >= thi)
x = tlo + 1;
else
x = thi;
oldx = x;
y = x - d;
while (x < xlim && y < ylim && xv[x] == yv[y])
++x, ++y;
if (x - oldx > SNAKE_LIMIT)
big_snake = 1;
fd[d] = x;
if (odd && bmin <= d && d <= bmax && bd[d] <= x)
{
part->xmid = x;
part->ymid = y;
part->lo_minimal = part->hi_minimal = 1;
return 2 * c - 1;
}
}
bmin > dmin ? bd[--bmin - 1] = INT_MAX : ++bmin;
bmax < dmax ? bd[++bmax + 1] = INT_MAX : --bmax;
for (d = bmax; d >= bmin; d -= 2)
{
int x, y, oldx, tlo = bd[d - 1], thi = bd[d + 1];
if (tlo < thi)
x = tlo;
else
x = thi - 1;
oldx = x;
y = x - d;
while (x > xoff && y > yoff && xv[x - 1] == yv[y - 1])
--x, --y;
if (oldx - x > SNAKE_LIMIT)
big_snake = 1;
bd[d] = x;
if (!odd && fmin <= d && d <= fmax && x <= fd[d])
{
part->xmid = x;
part->ymid = y;
part->lo_minimal = part->hi_minimal = 1;
return 2 * c;
}
}
if (minimal)
continue;
if (c > 200 && big_snake && heuristic)
{
int best;
best = 0;
for (d = fmax; d >= fmin; d -= 2)
{
int dd = d - fmid;
int x = fd[d];
int y = x - d;
int v = (x - xoff) * 2 - dd;
if (v > 12 * (c + (dd < 0 ? -dd : dd)))
{
if (v > best
&& xoff + SNAKE_LIMIT <= x && x < xlim
&& yoff + SNAKE_LIMIT <= y && y < ylim)
{
int k;
for (k = 1; xv[x - k] == yv[y - k]; k++)
if (k == SNAKE_LIMIT)
{
best = v;
part->xmid = x;
part->ymid = y;
break;
}
}
}
}
if (best > 0)
{
part->lo_minimal = 1;
part->hi_minimal = 0;
return 2 * c - 1;
}
best = 0;
for (d = bmax; d >= bmin; d -= 2)
{
int dd = d - bmid;
int x = bd[d];
int y = x - d;
int v = (xlim - x) * 2 + dd;
if (v > 12 * (c + (dd < 0 ? -dd : dd)))
{
if (v > best
&& xoff < x && x <= xlim - SNAKE_LIMIT
&& yoff < y && y <= ylim - SNAKE_LIMIT)
{
int k;
for (k = 0; xv[x + k] == yv[y + k]; k++)
if (k == SNAKE_LIMIT - 1)
{
best = v;
part->xmid = x;
part->ymid = y;
break;
}
}
}
}
if (best > 0)
{
part->lo_minimal = 0;
part->hi_minimal = 1;
return 2 * c - 1;
}
}
if (c >= too_expensive)
{
int fxybest, fxbest;
int bxybest, bxbest;
fxbest = bxbest = 0;
fxybest = -1;
for (d = fmax; d >= fmin; d -= 2)
{
int x = min (fd[d], xlim);
int y = x - d;
if (ylim < y)
x = ylim + d, y = ylim;
if (fxybest < x + y)
{
fxybest = x + y;
fxbest = x;
}
}
bxybest = INT_MAX;
for (d = bmax; d >= bmin; d -= 2)
{
int x = max (xoff, bd[d]);
int y = x - d;
if (y < yoff)
x = yoff + d, y = yoff;
if (x + y < bxybest)
{
bxybest = x + y;
bxbest = x;
}
}
if ((xlim + ylim) - bxybest < fxybest - (xoff + yoff))
{
part->xmid = fxbest;
part->ymid = fxybest - fxbest;
part->lo_minimal = 1;
part->hi_minimal = 0;
}
else
{
part->xmid = bxbest;
part->ymid = bxybest - bxbest;
part->lo_minimal = 0;
part->hi_minimal = 1;
}
return 2 * c - 1;
}
}
}
static void
compareseq (xoff, xlim, yoff, ylim, minimal)
int xoff, xlim, yoff, ylim, minimal;
{
int * const xv = xvec;
int * const yv = yvec;
while (xoff < xlim && yoff < ylim && xv[xoff] == yv[yoff])
++xoff, ++yoff;
while (xlim > xoff && ylim > yoff && xv[xlim - 1] == yv[ylim - 1])
--xlim, --ylim;
if (xoff == xlim)
while (yoff < ylim)
files[1].changed_flag[files[1].realindexes[yoff++]] = 1;
else if (yoff == ylim)
while (xoff < xlim)
files[0].changed_flag[files[0].realindexes[xoff++]] = 1;
else
{
int c;
struct partition part;
c = diag (xoff, xlim, yoff, ylim, minimal, &part);
if (c == 1)
{
abort ();
#if 0
if (part.xmid - part.ymid < xoff - yoff)
files[1].changed_flag[files[1].realindexes[part.ymid - 1]] = 1;
else
files[0].changed_flag[files[0].realindexes[part.xmid]] = 1;
#endif
}
else
{
compareseq (xoff, part.xmid, yoff, part.ymid, part.lo_minimal);
compareseq (part.xmid, xlim, part.ymid, ylim, part.hi_minimal);
}
}
}
static void
discard_confusing_lines (filevec)
struct file_data filevec[];
{
unsigned int f, i;
char *discarded[2];
int *equiv_count[2];
int *p;
p = (int *) xmalloc ((filevec[0].buffered_lines + filevec[1].buffered_lines)
* (2 * sizeof (int)));
for (f = 0; f < 2; f++)
{
filevec[f].undiscarded = p; p += filevec[f].buffered_lines;
filevec[f].realindexes = p; p += filevec[f].buffered_lines;
}
p = (int *) xmalloc (filevec[0].equiv_max * (2 * sizeof (int)));
equiv_count[0] = p;
equiv_count[1] = p + filevec[0].equiv_max;
bzero (p, filevec[0].equiv_max * (2 * sizeof (int)));
for (i = 0; i < filevec[0].buffered_lines; ++i)
++equiv_count[0][filevec[0].equivs[i]];
for (i = 0; i < filevec[1].buffered_lines; ++i)
++equiv_count[1][filevec[1].equivs[i]];
discarded[0] = xmalloc (sizeof (char)
* (filevec[0].buffered_lines
+ filevec[1].buffered_lines));
discarded[1] = discarded[0] + filevec[0].buffered_lines;
bzero (discarded[0], sizeof (char) * (filevec[0].buffered_lines
+ filevec[1].buffered_lines));
for (f = 0; f < 2; f++)
{
unsigned int end = filevec[f].buffered_lines;
char *discards = discarded[f];
int *counts = equiv_count[1 - f];
int *equivs = filevec[f].equivs;
unsigned int many = 5;
unsigned int tem = end / 64;
while ((tem = tem >> 2) > 0)
many *= 2;
for (i = 0; i < end; i++)
{
int nmatch;
if (equivs[i] == 0)
continue;
nmatch = counts[equivs[i]];
if (nmatch == 0)
discards[i] = 1;
else if (nmatch > many)
discards[i] = 2;
}
}
for (f = 0; f < 2; f++)
{
unsigned int end = filevec[f].buffered_lines;
register char *discards = discarded[f];
for (i = 0; i < end; i++)
{
if (discards[i] == 2)
discards[i] = 0;
else if (discards[i] != 0)
{
register int j;
unsigned int length;
unsigned int provisional = 0;
for (j = i; j < end; j++)
{
if (discards[j] == 0)
break;
if (discards[j] == 2)
++provisional;
}
while (j > i && discards[j - 1] == 2)
discards[--j] = 0, --provisional;
length = j - i;
if (provisional * 4 > length)
{
while (j > i)
if (discards[--j] == 2)
discards[j] = 0;
}
else
{
register unsigned int consec;
unsigned int minimum = 1;
unsigned int tem = length / 4;
while ((tem = tem >> 2) > 0)
minimum *= 2;
minimum++;
for (j = 0, consec = 0; j < length; j++)
if (discards[i + j] != 2)
consec = 0;
else if (minimum == ++consec)
j -= consec;
else if (minimum < consec)
discards[i + j] = 0;
for (j = 0, consec = 0; j < length; j++)
{
if (j >= 8 && discards[i + j] == 1)
break;
if (discards[i + j] == 2)
consec = 0, discards[i + j] = 0;
else if (discards[i + j] == 0)
consec = 0;
else
consec++;
if (consec == 3)
break;
}
i += length - 1;
for (j = 0, consec = 0; j < length; j++)
{
if (j >= 8 && discards[i - j] == 1)
break;
if (discards[i - j] == 2)
consec = 0, discards[i - j] = 0;
else if (discards[i - j] == 0)
consec = 0;
else
consec++;
if (consec == 3)
break;
}
}
}
}
}
for (f = 0; f < 2; f++)
{
char *discards = discarded[f];
unsigned int end = filevec[f].buffered_lines;
unsigned int j = 0;
for (i = 0; i < end; ++i)
if (no_discards || discards[i] == 0)
{
filevec[f].undiscarded[j] = filevec[f].equivs[i];
filevec[f].realindexes[j++] = i;
}
else
filevec[f].changed_flag[i] = 1;
filevec[f].nondiscarded_lines = j;
}
free (discarded[0]);
free (equiv_count[0]);
}
int inhibit;
static void
shift_boundaries (filevec)
struct file_data filevec[];
{
int f;
if (inhibit)
return;
for (f = 0; f < 2; f++)
{
char *changed = filevec[f].changed_flag;
char const *other_changed = filevec[1-f].changed_flag;
int const *equivs = filevec[f].equivs;
int i = 0;
int j = 0;
int i_end = filevec[f].buffered_lines;
while (1)
{
int runlength, start, corresponding;
while (i < i_end && changed[i] == 0)
{
while (other_changed[j++])
continue;
i++;
}
if (i == i_end)
break;
start = i;
while (changed[++i])
continue;
while (other_changed[j])
j++;
do
{
runlength = i - start;
while (start && equivs[start - 1] == equivs[i - 1])
{
changed[--start] = 1;
changed[--i] = 0;
while (changed[start - 1])
start--;
while (other_changed[--j])
continue;
}
corresponding = other_changed[j - 1] ? i : i_end;
while (i != i_end && equivs[start] == equivs[i])
{
changed[start++] = 0;
changed[i++] = 1;
while (changed[i])
i++;
while (other_changed[++j])
corresponding = i;
}
}
while (runlength != i - start);
while (corresponding < i)
{
changed[--start] = 1;
changed[--i] = 0;
while (other_changed[--j])
continue;
}
}
}
}
static struct change *
add_change (line0, line1, deleted, inserted, old)
int line0, line1, deleted, inserted;
struct change *old;
{
struct change *new = (struct change *) xmalloc (sizeof (struct change));
new->line0 = line0;
new->line1 = line1;
new->inserted = inserted;
new->deleted = deleted;
new->link = old;
return new;
}
static struct change *
build_reverse_script (filevec)
struct file_data const filevec[];
{
struct change *script = 0;
char *changed0 = filevec[0].changed_flag;
char *changed1 = filevec[1].changed_flag;
int len0 = filevec[0].buffered_lines;
int len1 = filevec[1].buffered_lines;
int i0 = 0, i1 = 0;
while (i0 < len0 || i1 < len1)
{
if (changed0[i0] || changed1[i1])
{
int line0 = i0, line1 = i1;
while (changed0[i0]) ++i0;
while (changed1[i1]) ++i1;
script = add_change (line0, line1, i0 - line0, i1 - line1, script);
}
i0++, i1++;
}
return script;
}
static struct change *
build_script (filevec)
struct file_data const filevec[];
{
struct change *script = 0;
char *changed0 = filevec[0].changed_flag;
char *changed1 = filevec[1].changed_flag;
int i0 = filevec[0].buffered_lines, i1 = filevec[1].buffered_lines;
while (i0 >= 0 || i1 >= 0)
{
if (changed0[i0 - 1] || changed1[i1 - 1])
{
int line0 = i0, line1 = i1;
while (changed0[i0 - 1]) --i0;
while (changed1[i1 - 1]) --i1;
script = add_change (i0, i1, line0 - i0, line1 - i1, script);
}
i0--, i1--;
}
return script;
}
static void
briefly_report (changes, filevec)
int changes;
struct file_data const filevec[];
{
if (changes)
message (no_details_flag ? "Files %s and %s differ\n"
: "Binary files %s and %s differ\n",
filevec[0].name, filevec[1].name);
}
int
diff_2_files (filevec, depth)
struct file_data filevec[];
int depth;
{
int diags;
int i;
struct change *e, *p;
struct change *script;
int changes;
if (read_files (filevec, no_details_flag & ~ignore_some_changes))
{
if (filevec[0].stat.st_size != filevec[1].stat.st_size
&& (filevec[0].desc < 0 || S_ISREG (filevec[0].stat.st_mode))
&& (filevec[1].desc < 0 || S_ISREG (filevec[1].stat.st_mode)))
changes = 1;
else if (filevec[0].desc == filevec[1].desc)
changes = 0;
else
{
size_t buffer_size = buffer_lcm (STAT_BLOCKSIZE (filevec[0].stat),
STAT_BLOCKSIZE (filevec[1].stat));
for (i = 0; i < 2; i++)
filevec[i].buffer = xrealloc (filevec[i].buffer, buffer_size);
for (;; filevec[0].buffered_chars = filevec[1].buffered_chars = 0)
{
for (i = 0; i < 2; i++)
if (0 <= filevec[i].desc)
while (filevec[i].buffered_chars != buffer_size)
{
int r = read (filevec[i].desc,
filevec[i].buffer
+ filevec[i].buffered_chars,
buffer_size - filevec[i].buffered_chars);
if (r == 0)
break;
if (r < 0)
pfatal_with_name (filevec[i].name);
filevec[i].buffered_chars += r;
}
if (filevec[0].buffered_chars != filevec[1].buffered_chars
|| (filevec[0].buffered_chars != 0
&& memcmp (filevec[0].buffer,
filevec[1].buffer,
filevec[0].buffered_chars) != 0))
{
changes = 1;
break;
}
if (filevec[0].buffered_chars != buffer_size)
{
changes = 0;
break;
}
}
}
briefly_report (changes, filevec);
}
else
{
size_t s = filevec[0].buffered_lines + filevec[1].buffered_lines + 4;
filevec[0].changed_flag = xmalloc (s);
bzero (filevec[0].changed_flag, s);
filevec[0].changed_flag++;
filevec[1].changed_flag = filevec[0].changed_flag
+ filevec[0].buffered_lines + 2;
discard_confusing_lines (filevec);
xvec = filevec[0].undiscarded;
yvec = filevec[1].undiscarded;
diags = filevec[0].nondiscarded_lines + filevec[1].nondiscarded_lines + 3;
fdiag = (int *) xmalloc (diags * (2 * sizeof (int)));
bdiag = fdiag + diags;
fdiag += filevec[1].nondiscarded_lines + 1;
bdiag += filevec[1].nondiscarded_lines + 1;
too_expensive = 1;
for (i = filevec[0].nondiscarded_lines + filevec[1].nondiscarded_lines;
i != 0; i >>= 2)
too_expensive <<= 1;
too_expensive = max (256, too_expensive);
files[0] = filevec[0];
files[1] = filevec[1];
compareseq (0, filevec[0].nondiscarded_lines,
0, filevec[1].nondiscarded_lines, no_discards);
free (fdiag - (filevec[1].nondiscarded_lines + 1));
shift_boundaries (filevec);
if (output_style == OUTPUT_ED)
script = build_reverse_script (filevec);
else
script = build_script (filevec);
if (ignore_blank_lines_flag || ignore_regexp_list)
{
struct change *next = script;
changes = 0;
while (next && changes == 0)
{
struct change *this, *end;
int first0, last0, first1, last1, deletes, inserts;
this = next;
end = find_change (next);
next = end->link;
end->link = 0;
analyze_hunk (this, &first0, &last0, &first1, &last1,
&deletes, &inserts);
end->link = next;
if (deletes || inserts)
changes = 1;
}
}
else
changes = (script != 0);
if (no_details_flag)
briefly_report (changes, filevec);
else
{
if (changes || ! no_diff_means_no_output)
{
setup_output (files[0].name, files[1].name, depth);
switch (output_style)
{
case OUTPUT_CONTEXT:
print_context_script (script, 0);
break;
case OUTPUT_UNIFIED:
print_context_script (script, 1);
break;
case OUTPUT_ED:
print_ed_script (script);
break;
case OUTPUT_FORWARD_ED:
pr_forward_ed_script (script);
break;
case OUTPUT_RCS:
print_rcs_script (script);
break;
case OUTPUT_NORMAL:
print_normal_script (script);
break;
case OUTPUT_IFDEF:
print_ifdef_script (script);
break;
case OUTPUT_SDIFF:
print_sdiff_script (script);
}
finish_output ();
}
}
free (filevec[0].undiscarded);
free (filevec[0].changed_flag - 1);
for (i = 1; i >= 0; --i)
free (filevec[i].equivs);
for (i = 0; i < 2; ++i)
free (filevec[i].linbuf + filevec[i].linbuf_base);
for (e = script; e; e = p)
{
p = e->link;
free (e);
}
if (! ROBUST_OUTPUT_STYLE (output_style))
for (i = 0; i < 2; ++i)
if (filevec[i].missing_newline)
{
error ("No newline at end of file %s", filevec[i].name, "");
changes = 2;
}
}
if (filevec[0].buffer != filevec[1].buffer)
free (filevec[0].buffer);
free (filevec[1].buffer);
return changes;
}