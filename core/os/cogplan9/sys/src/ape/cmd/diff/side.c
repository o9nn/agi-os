#include "diff.h"
static unsigned print_half_line PARAMS((char const * const *, unsigned, unsigned));
static unsigned tab_from_to PARAMS((unsigned, unsigned));
static void print_1sdiff_line PARAMS((char const * const *, int, char const * const *));
static void print_sdiff_common_lines PARAMS((int, int));
static void print_sdiff_hunk PARAMS((struct change *));
static int next0, next1;
void
print_sdiff_script (script)
struct change *script;
{
begin_output ();
next0 = next1 = - files[0].prefix_lines;
print_script (script, find_change, print_sdiff_hunk);
print_sdiff_common_lines (files[0].valid_lines, files[1].valid_lines);
}
static unsigned
tab_from_to (from, to)
unsigned from, to;
{
FILE *out = outfile;
unsigned tab;
if (! tab_expand_flag)
for (tab = from + TAB_WIDTH - from % TAB_WIDTH; tab <= to; tab += TAB_WIDTH)
{
putc ('\t', out);
from = tab;
}
while (from++ < to)
putc (' ', out);
return to;
}
static unsigned
print_half_line (line, indent, out_bound)
char const * const *line;
unsigned indent, out_bound;
{
FILE *out = outfile;
register unsigned in_position = 0, out_position = 0;
register char const
*text_pointer = line[0],
*text_limit = line[1];
while (text_pointer < text_limit)
{
register unsigned char c = *text_pointer++;
switch (c)
{
case '\t':
{
unsigned spaces = TAB_WIDTH - in_position % TAB_WIDTH;
if (in_position == out_position)
{
unsigned tabstop = out_position + spaces;
if (tab_expand_flag)
{
if (out_bound < tabstop)
tabstop = out_bound;
for (; out_position < tabstop; out_position++)
putc (' ', out);
}
else
if (tabstop < out_bound)
{
out_position = tabstop;
putc (c, out);
}
}
in_position += spaces;
}
break;
case '\r':
{
putc (c, out);
tab_from_to (0, indent);
in_position = out_position = 0;
}
break;
case '\b':
if (in_position != 0 && --in_position < out_bound)
if (out_position <= in_position)
for (; out_position < in_position; out_position++)
putc (' ', out);
else
{
out_position = in_position;
putc (c, out);
}
break;
case '\f':
case '\v':
control_char:
if (in_position < out_bound)
putc (c, out);
break;
default:
if (! ISPRINT (c))
goto control_char;
case ' ':
if (in_position++ < out_bound)
{
out_position = in_position;
putc (c, out);
}
break;
case '\n':
return out_position;
}
}
return out_position;
}
static void
print_1sdiff_line (left, sep, right)
char const * const *left;
int sep;
char const * const *right;
{
FILE *out = outfile;
unsigned hw = sdiff_half_width, c2o = sdiff_column2_offset;
unsigned col = 0;
int put_newline = 0;
if (left)
{
if (left[1][-1] == '\n')
put_newline = 1;
col = print_half_line (left, 0, hw);
}
if (sep != ' ')
{
col = tab_from_to (col, (hw + c2o - 1) / 2) + 1;
if (sep == '|' && put_newline != (right[1][-1] == '\n'))
sep = put_newline ? '/' : '\\';
putc (sep, out);
}
if (right)
{
if (right[1][-1] == '\n')
put_newline = 1;
if (**right != '\n')
{
col = tab_from_to (col, c2o);
print_half_line (right, col, hw);
}
}
if (put_newline)
putc ('\n', out);
}
static void
print_sdiff_common_lines (limit0, limit1)
int limit0, limit1;
{
int i0 = next0, i1 = next1;
if (! sdiff_skip_common_lines && (i0 != limit0 || i1 != limit1))
{
if (sdiff_help_sdiff)
fprintf (outfile, "i%d,%d\n", limit0 - i0, limit1 - i1);
if (! sdiff_left_only)
{
while (i0 != limit0 && i1 != limit1)
print_1sdiff_line (&files[0].linbuf[i0++], ' ', &files[1].linbuf[i1++]);
while (i1 != limit1)
print_1sdiff_line (0, ')', &files[1].linbuf[i1++]);
}
while (i0 != limit0)
print_1sdiff_line (&files[0].linbuf[i0++], '(', 0);
}
next0 = limit0;
next1 = limit1;
}
static void
print_sdiff_hunk (hunk)
struct change *hunk;
{
int first0, last0, first1, last1, deletes, inserts;
register int i, j;
analyze_hunk (hunk, &first0, &last0, &first1, &last1, &deletes, &inserts);
if (!deletes && !inserts)
return;
print_sdiff_common_lines (first0, first1);
if (sdiff_help_sdiff)
fprintf (outfile, "c%d,%d\n", last0 - first0 + 1, last1 - first1 + 1);
if (inserts && deletes)
{
for (i = first0, j = first1; i <= last0 && j <= last1; ++i, ++j)
print_1sdiff_line (&files[0].linbuf[i], '|', &files[1].linbuf[j]);
deletes = i <= last0;
inserts = j <= last1;
next0 = first0 = i;
next1 = first1 = j;
}
if (inserts)
{
for (j = first1; j <= last1; ++j)
print_1sdiff_line (0, '>', &files[1].linbuf[j]);
next1 = j;
}
if (deletes)
{
for (i = first0; i <= last0; ++i)
print_1sdiff_line (&files[0].linbuf[i], '<', 0);
next0 = i;
}
}