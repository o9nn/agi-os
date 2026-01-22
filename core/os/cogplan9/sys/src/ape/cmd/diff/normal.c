#include "diff.h"
static void print_normal_hunk PARAMS((struct change *));
void
print_normal_script (script)
struct change *script;
{
print_script (script, find_change, print_normal_hunk);
}
static void
print_normal_hunk (hunk)
struct change *hunk;
{
int first0, last0, first1, last1, deletes, inserts;
register int i;
analyze_hunk (hunk, &first0, &last0, &first1, &last1, &deletes, &inserts);
if (!deletes && !inserts)
return;
begin_output ();
print_number_range (',', &files[0], first0, last0);
fprintf (outfile, "%c", change_letter (inserts, deletes));
print_number_range (',', &files[1], first1, last1);
fprintf (outfile, "\n");
if (deletes)
for (i = first0; i <= last0; i++)
print_1_line ("<", &files[0].linbuf[i]);
if (inserts && deletes)
fprintf (outfile, "---\n");
if (inserts)
for (i = first1; i <= last1; i++)
print_1_line (">", &files[1].linbuf[i]);
}