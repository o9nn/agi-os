#include "diff.h"
static struct change *find_hunk PARAMS((struct change *));
static void find_function PARAMS((struct file_data const *, int, char const **, size_t *));
static void mark_ignorable PARAMS((struct change *));
static void pr_context_hunk PARAMS((struct change *));
static void pr_unidiff_hunk PARAMS((struct change *));
static void print_context_label PARAMS ((char const *, struct file_data *, char const *));
static void print_context_number_range PARAMS((struct file_data const *, int, int));
static void print_unidiff_number_range PARAMS((struct file_data const *, int, int));
static int find_function_last_search;
static int find_function_last_match;
static void
print_context_label (mark, inf, label)
char const *mark;
struct file_data *inf;
char const *label;
{
if (label)
fprintf (outfile, "%s %s\n", mark, label);
else
{
char const *ct = ctime (&inf->stat.st_mtime);
if (!ct)
ct = "?\n";
fprintf (outfile, "%s %s\t%s", mark, inf->name, ct);
}
}
void
print_context_header (inf, unidiff_flag)
struct file_data inf[];
int unidiff_flag;
{
if (unidiff_flag)
{
print_context_label ("---", &inf[0], file_label[0]);
print_context_label ("+++", &inf[1], file_label[1]);
}
else
{
print_context_label ("***", &inf[0], file_label[0]);
print_context_label ("---", &inf[1], file_label[1]);
}
}
void
print_context_script (script, unidiff_flag)
struct change *script;
int unidiff_flag;
{
if (ignore_blank_lines_flag || ignore_regexp_list)
mark_ignorable (script);
else
{
struct change *e;
for (e = script; e; e = e->link)
e->ignore = 0;
}
find_function_last_search = - files[0].prefix_lines;
find_function_last_match = find_function_last_search - 1;
if (unidiff_flag)
print_script (script, find_hunk, pr_unidiff_hunk);
else
print_script (script, find_hunk, pr_context_hunk);
}
static void
print_context_number_range (file, a, b)
struct file_data const *file;
int a, b;
{
int trans_a, trans_b;
translate_range (file, a, b, &trans_a, &trans_b);
if (trans_b > trans_a)
fprintf (outfile, "%d,%d", trans_a, trans_b);
else
fprintf (outfile, "%d", trans_b);
}
static void
pr_context_hunk (hunk)
struct change *hunk;
{
int first0, last0, first1, last1, show_from, show_to, i;
struct change *next;
char const *prefix;
char const *function;
size_t function_length;
FILE *out;
analyze_hunk (hunk, &first0, &last0, &first1, &last1, &show_from, &show_to);
if (!show_from && !show_to)
return;
i = - files[0].prefix_lines;
first0 = max (first0 - context, i);
first1 = max (first1 - context, i);
last0 = min (last0 + context, files[0].valid_lines - 1);
last1 = min (last1 + context, files[1].valid_lines - 1);
function = 0;
if (function_regexp_list)
find_function (&files[0], first0, &function, &function_length);
begin_output ();
out = outfile;
fprintf (out, "***************");
if (function)
{
fprintf (out, " ");
fwrite (function, 1, min (function_length - 1, 40), out);
}
fprintf (out, "\n*** ");
print_context_number_range (&files[0], first0, last0);
fprintf (out, " ****\n");
if (show_from)
{
next = hunk;
for (i = first0; i <= last0; i++)
{
while (next && next->line0 + next->deleted <= i)
next = next->link;
prefix = " ";
if (next && next->line0 <= i)
prefix = (next->inserted > 0 ? "!" : "-");
print_1_line (prefix, &files[0].linbuf[i]);
}
}
fprintf (out, "--- ");
print_context_number_range (&files[1], first1, last1);
fprintf (out, " ----\n");
if (show_to)
{
next = hunk;
for (i = first1; i <= last1; i++)
{
while (next && next->line1 + next->inserted <= i)
next = next->link;
prefix = " ";
if (next && next->line1 <= i)
prefix = (next->deleted > 0 ? "!" : "+");
print_1_line (prefix, &files[1].linbuf[i]);
}
}
}
static void
print_unidiff_number_range (file, a, b)
struct file_data const *file;
int a, b;
{
int trans_a, trans_b;
translate_range (file, a, b, &trans_a, &trans_b);
if (trans_b <= trans_a)
fprintf (outfile, trans_b == trans_a ? "%d" : "%d,0", trans_b);
else
fprintf (outfile, "%d,%d", trans_a, trans_b - trans_a + 1);
}
static void
pr_unidiff_hunk (hunk)
struct change *hunk;
{
int first0, last0, first1, last1, show_from, show_to, i, j, k;
struct change *next;
char const *function;
size_t function_length;
FILE *out;
analyze_hunk (hunk, &first0, &last0, &first1, &last1, &show_from, &show_to);
if (!show_from && !show_to)
return;
i = - files[0].prefix_lines;
first0 = max (first0 - context, i);
first1 = max (first1 - context, i);
last0 = min (last0 + context, files[0].valid_lines - 1);
last1 = min (last1 + context, files[1].valid_lines - 1);
function = 0;
if (function_regexp_list)
find_function (&files[0], first0, &function, &function_length);
begin_output ();
out = outfile;
fprintf (out, "@@ -");
print_unidiff_number_range (&files[0], first0, last0);
fprintf (out, " +");
print_unidiff_number_range (&files[1], first1, last1);
fprintf (out, " @@");
if (function)
{
putc (' ', out);
fwrite (function, 1, min (function_length - 1, 40), out);
}
putc ('\n', out);
next = hunk;
i = first0;
j = first1;
while (i <= last0 || j <= last1)
{
if (!next || i < next->line0)
{
putc (tab_align_flag ? '\t' : ' ', out);
print_1_line (0, &files[0].linbuf[i++]);
j++;
}
else
{
k = next->deleted;
while (k--)
{
putc ('-', out);
if (tab_align_flag)
putc ('\t', out);
print_1_line (0, &files[0].linbuf[i++]);
}
k = next->inserted;
while (k--)
{
putc ('+', out);
if (tab_align_flag)
putc ('\t', out);
print_1_line (0, &files[1].linbuf[j++]);
}
next = next->link;
}
}
}
static struct change *
find_hunk (start)
struct change *start;
{
struct change *prev;
int top0, top1;
int thresh;
do
{
top0 = start->line0 + start->deleted;
top1 = start->line1 + start->inserted;
prev = start;
start = start->link;
thresh = ((prev->ignore || (start && start->ignore))
? context
: 2 * context + 1);
if (start && start->line0 - top0 != start->line1 - top1)
abort ();
} while (start
&& start->line0 < top0 + thresh);
return prev;
}
static void
mark_ignorable (script)
struct change *script;
{
while (script)
{
struct change *next = script->link;
int first0, last0, first1, last1, deletes, inserts;
script->link = 0;
analyze_hunk (script, &first0, &last0, &first1, &last1, &deletes, &inserts);
script->link = next;
script->ignore = (!deletes && !inserts);
script = next;
}
}
static void
find_function (file, linenum, linep, lenp)
struct file_data const *file;
int linenum;
char const **linep;
size_t *lenp;
{
int i = linenum;
int last = find_function_last_search;
find_function_last_search = i;
while (--i >= last)
{
struct regexp_list *r;
char const *line = file->linbuf[i];
size_t len = file->linbuf[i + 1] - line;
for (r = function_regexp_list; r; r = r->next)
if (0 <= re_search (&r->buf, line, len, 0, len, 0))
{
*linep = line;
*lenp = len;
find_function_last_match = i;
return;
}
}
if (find_function_last_match >= - file->prefix_lines)
{
i = find_function_last_match;
*linep = file->linbuf[i];
*lenp = file->linbuf[i + 1] - *linep;
return;
}
return;
}