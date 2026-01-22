#include "diff.h"
static void print_ed_hunk PARAMS((struct change *));
static void print_rcs_hunk PARAMS((struct change *));
static void pr_forward_ed_hunk PARAMS((struct change *));
void
print_ed_script (script)
struct change *script;
{
print_script (script, find_reverse_change, print_ed_hunk);
}
static void
print_ed_hunk (hunk)
struct change *hunk;
{
int f0, l0, f1, l1;
int deletes, inserts;
#if 0
hunk = flip_script (hunk);
#endif
#ifdef DEBUG
debug_script (hunk);
#endif
analyze_hunk (hunk, &f0, &l0, &f1, &l1, &deletes, &inserts);
if (!deletes && !inserts)
return;
begin_output ();
print_number_range (',', &files[0], f0, l0);
fprintf (outfile, "%c\n", change_letter (inserts, deletes));
if (inserts)
{
int i;
int inserting = 1;
for (i = f1; i <= l1; i++)
{
if (! inserting)
fprintf (outfile, "%da\n",
i - f1 + translate_line_number (&files[0], f0) - 1);
inserting = 1;
if (files[1].linbuf[i][0] == '.'
&& files[1].linbuf[i][1] == '\n')
{
fprintf (outfile, "..\n");
fprintf (outfile, ".\n");
fprintf (outfile, "%ds/^\\.\\././\n",
i - f1 + translate_line_number (&files[0], f0));
inserting = 0;
}
else
print_1_line ("", &files[1].linbuf[i]);
}
if (inserting)
fprintf (outfile, ".\n");
}
}
void
pr_forward_ed_script (script)
struct change *script;
{
print_script (script, find_change, pr_forward_ed_hunk);
}
static void
pr_forward_ed_hunk (hunk)
struct change *hunk;
{
int i;
int f0, l0, f1, l1;
int deletes, inserts;
analyze_hunk (hunk, &f0, &l0, &f1, &l1, &deletes, &inserts);
if (!deletes && !inserts)
return;
begin_output ();
fprintf (outfile, "%c", change_letter (inserts, deletes));
print_number_range (' ', files, f0, l0);
fprintf (outfile, "\n");
if (!inserts)
return;
for (i = f1; i <= l1; i++)
print_1_line ("", &files[1].linbuf[i]);
fprintf (outfile, ".\n");
}
void
print_rcs_script (script)
struct change *script;
{
print_script (script, find_change, print_rcs_hunk);
}
static void
print_rcs_hunk (hunk)
struct change *hunk;
{
int i;
int f0, l0, f1, l1;
int deletes, inserts;
int tf0, tl0, tf1, tl1;
analyze_hunk (hunk, &f0, &l0, &f1, &l1, &deletes, &inserts);
if (!deletes && !inserts)
return;
begin_output ();
translate_range (&files[0], f0, l0, &tf0, &tl0);
if (deletes)
{
fprintf (outfile, "d");
fprintf (outfile, "%d %d\n",
tf0,
(tl0 >= tf0 ? tl0 - tf0 + 1 : 1));
}
if (inserts)
{
fprintf (outfile, "a");
translate_range (&files[1], f1, l1, &tf1, &tl1);
fprintf (outfile, "%d %d\n",
tl0,
(tl1 >= tf1 ? tl1 - tf1 + 1 : 1));
for (i = f1; i <= l1; i++)
print_1_line ("", &files[1].linbuf[i]);
}
}