#include "diff.h"
#ifndef PR_PROGRAM
#define PR_PROGRAM "/bin/pr"
#endif
struct msg
{
struct msg *next;
char const *format;
char const *arg1;
char const *arg2;
char const *arg3;
char const *arg4;
};
static struct msg *msg_chain;
static struct msg **msg_chain_end = &msg_chain;
void
perror_with_name (text)
char const *text;
{
int e = errno;
fprintf (stderr, "%s: ", program_name);
errno = e;
perror (text);
}
void
pfatal_with_name (text)
char const *text;
{
int e = errno;
print_message_queue ();
fprintf (stderr, "%s: ", program_name);
errno = e;
perror (text);
exit (2);
}
void
error (format, arg, arg1)
char const *format, *arg, *arg1;
{
fprintf (stderr, "%s: ", program_name);
fprintf (stderr, format, arg, arg1);
fprintf (stderr, "\n");
}
void
fatal (m)
char const *m;
{
print_message_queue ();
error ("%s", m, 0);
exit (2);
}
void
message (format, arg1, arg2)
char const *format, *arg1, *arg2;
{
message5 (format, arg1, arg2, 0, 0);
}
void
message5 (format, arg1, arg2, arg3, arg4)
char const *format, *arg1, *arg2, *arg3, *arg4;
{
if (paginate_flag)
{
struct msg *new = (struct msg *) xmalloc (sizeof (struct msg));
new->format = format;
new->arg1 = concat (arg1, "", "");
new->arg2 = concat (arg2, "", "");
new->arg3 = arg3 ? concat (arg3, "", "") : 0;
new->arg4 = arg4 ? concat (arg4, "", "") : 0;
new->next = 0;
*msg_chain_end = new;
msg_chain_end = &new->next;
}
else
{
if (sdiff_help_sdiff)
putchar (' ');
printf (format, arg1, arg2, arg3, arg4);
}
}
void
print_message_queue ()
{
struct msg *m;
for (m = msg_chain; m; m = m->next)
printf (m->format, m->arg1, m->arg2, m->arg3, m->arg4);
}
static char const *current_name0;
static char const *current_name1;
static int current_depth;
void
setup_output (name0, name1, depth)
char const *name0, *name1;
int depth;
{
current_name0 = name0;
current_name1 = name1;
current_depth = depth;
outfile = 0;
}
#if HAVE_FORK
static pid_t pr_pid;
#endif
void
begin_output ()
{
char *name;
if (outfile != 0)
return;
name = xmalloc (strlen (current_name0) + strlen (current_name1)
+ strlen (switch_string) + 7);
sprintf (name, "diff%s %s %s", switch_string, current_name0, current_name1);
if (paginate_flag)
{
#if HAVE_FORK
int pipes[2];
if (pipe (pipes) != 0)
pfatal_with_name ("pipe");
fflush (stdout);
pr_pid = fork ();
if (pr_pid < 0)
pfatal_with_name ("vfork");
if (pr_pid == 0)
{
close (pipes[1]);
if (pipes[0] != STDIN_FILENO)
{
if (dup2 (pipes[0], STDIN_FILENO) < 0)
pfatal_with_name ("dup2");
close (pipes[0]);
}
#ifdef __FreeBSD__
execl (PR_PROGRAM, PR_PROGRAM, "-F", "-h", name, 0);
#else
execl (PR_PROGRAM, PR_PROGRAM, "-f", "-h", name, 0);
#endif
pfatal_with_name (PR_PROGRAM);
}
else
{
close (pipes[0]);
outfile = fdopen (pipes[1], "w");
if (!outfile)
pfatal_with_name ("fdopen");
}
#else
char *command = xmalloc (4 * strlen (name) + strlen (PR_PROGRAM) + 10);
char *p;
char const *a = name;
sprintf (command, "%s -f -h ", PR_PROGRAM);
p = command + strlen (command);
SYSTEM_QUOTE_ARG (p, a);
*p = 0;
outfile = popen (command, "w");
if (!outfile)
pfatal_with_name (command);
free (command);
#endif
}
else
{
outfile = stdout;
if (current_depth > 0)
printf ("%s\n", name);
}
free (name);
switch (output_style)
{
case OUTPUT_CONTEXT:
print_context_header (files, 0);
break;
case OUTPUT_UNIFIED:
print_context_header (files, 1);
break;
default:
break;
}
}
void
finish_output ()
{
if (outfile != 0 && outfile != stdout)
{
int wstatus;
if (ferror (outfile))
fatal ("write error");
#if ! HAVE_FORK
wstatus = pclose (outfile);
#else
if (fclose (outfile) != 0)
pfatal_with_name ("write error");
if (waitpid (pr_pid, &wstatus, 0) < 0)
pfatal_with_name ("waitpid");
#endif
if (wstatus != 0)
fatal ("subsidiary pr failed");
}
outfile = 0;
}
int
line_cmp (s1, s2)
char const *s1, *s2;
{
register unsigned char const *t1 = (unsigned char const *) s1;
register unsigned char const *t2 = (unsigned char const *) s2;
while (1)
{
register unsigned char c1 = *t1++;
register unsigned char c2 = *t2++;
if (c1 != c2)
{
if (ignore_all_space_flag)
{
while (ISSPACE (c1) && c1 != '\n') c1 = *t1++;
while (ISSPACE (c2) && c2 != '\n') c2 = *t2++;
}
else if (ignore_space_change_flag)
{
if (ISSPACE (c1))
{
while (c1 != '\n')
{
c1 = *t1++;
if (! ISSPACE (c1))
{
--t1;
c1 = ' ';
break;
}
}
}
if (ISSPACE (c2))
{
while (c2 != '\n')
{
c2 = *t2++;
if (! ISSPACE (c2))
{
--t2;
c2 = ' ';
break;
}
}
}
if (c1 != c2)
{
if (c2 == ' ' && c1 != '\n'
&& (unsigned char const *) s1 + 1 < t1
&& ISSPACE(t1[-2]))
{
--t1;
continue;
}
if (c1 == ' ' && c2 != '\n'
&& (unsigned char const *) s2 + 1 < t2
&& ISSPACE(t2[-2]))
{
--t2;
continue;
}
}
}
if (ignore_case_flag)
{
if (ISUPPER (c1))
c1 = tolower (c1);
if (ISUPPER (c2))
c2 = tolower (c2);
}
if (c1 != c2)
break;
}
if (c1 == '\n')
return 0;
}
return (1);
}
struct change *
find_change (start)
struct change *start;
{
return start;
}
struct change *
find_reverse_change (start)
struct change *start;
{
return start;
}
void
print_script (script, hunkfun, printfun)
struct change *script;
struct change * (*hunkfun) PARAMS((struct change *));
void (*printfun) PARAMS((struct change *));
{
struct change *next = script;
while (next)
{
struct change *this, *end;
this = next;
end = (*hunkfun) (next);
next = end->link;
end->link = 0;
#ifdef DEBUG
debug_script (this);
#endif
(*printfun) (this);
end->link = next;
}
}
void
print_1_line (line_flag, line)
char const *line_flag;
char const * const *line;
{
char const *text = line[0], *limit = line[1];
FILE *out = outfile;
char const *flag_format = 0;
if (line_flag && *line_flag)
{
flag_format = tab_align_flag ? "%s\t" : "%s ";
fprintf (out, flag_format, line_flag);
}
output_1_line (text, limit, flag_format, line_flag);
if ((!line_flag || line_flag[0]) && limit[-1] != '\n')
fputc ('\n', out);
}
void
output_1_line (text, limit, flag_format, line_flag)
char const *text, *limit, *flag_format, *line_flag;
{
if (!tab_expand_flag)
fwrite (text, sizeof (char), limit - text, outfile);
else
{
register FILE *out = outfile;
register unsigned char c;
register char const *t = text;
register unsigned column = 0;
while (t < limit)
switch ((c = *t++))
{
case '\t':
{
unsigned spaces = TAB_WIDTH - column % TAB_WIDTH;
column += spaces;
do
putc (' ', out);
while (--spaces);
}
break;
case '\r':
putc (c, out);
if (flag_format && t < limit && *t != '\n')
fprintf (out, flag_format, line_flag);
column = 0;
break;
case '\b':
if (column == 0)
continue;
column--;
putc (c, out);
break;
default:
if (ISPRINT (c))
column++;
putc (c, out);
break;
}
}
}
int
change_letter (inserts, deletes)
int inserts, deletes;
{
if (!inserts)
return 'd';
else if (!deletes)
return 'a';
else
return 'c';
}
int
translate_line_number (file, lnum)
struct file_data const *file;
int lnum;
{
return lnum + file->prefix_lines + 1;
}
void
translate_range (file, a, b, aptr, bptr)
struct file_data const *file;
int a, b;
int *aptr, *bptr;
{
*aptr = translate_line_number (file, a - 1) + 1;
*bptr = translate_line_number (file, b + 1) - 1;
}
void
print_number_range (sepchar, file, a, b)
int sepchar;
struct file_data *file;
int a, b;
{
int trans_a, trans_b;
translate_range (file, a, b, &trans_a, &trans_b);
if (trans_b > trans_a)
fprintf (outfile, "%d%c%d", trans_a, sepchar, trans_b);
else
fprintf (outfile, "%d", trans_b);
}
void
analyze_hunk (hunk, first0, last0, first1, last1, deletes, inserts)
struct change *hunk;
int *first0, *last0, *first1, *last1;
int *deletes, *inserts;
{
int l0, l1, show_from, show_to;
int i;
int trivial = ignore_blank_lines_flag || ignore_regexp_list;
struct change *next;
show_from = show_to = 0;
*first0 = hunk->line0;
*first1 = hunk->line1;
next = hunk;
do
{
l0 = next->line0 + next->deleted - 1;
l1 = next->line1 + next->inserted - 1;
show_from += next->deleted;
show_to += next->inserted;
for (i = next->line0; i <= l0 && trivial; i++)
if (!ignore_blank_lines_flag || files[0].linbuf[i][0] != '\n')
{
struct regexp_list *r;
char const *line = files[0].linbuf[i];
int len = files[0].linbuf[i + 1] - line;
for (r = ignore_regexp_list; r; r = r->next)
if (0 <= re_search (&r->buf, line, len, 0, len, 0))
break;
if (!r)
trivial = 0;
}
for (i = next->line1; i <= l1 && trivial; i++)
if (!ignore_blank_lines_flag || files[1].linbuf[i][0] != '\n')
{
struct regexp_list *r;
char const *line = files[1].linbuf[i];
int len = files[1].linbuf[i + 1] - line;
for (r = ignore_regexp_list; r; r = r->next)
if (0 <= re_search (&r->buf, line, len, 0, len, 0))
break;
if (!r)
trivial = 0;
}
}
while ((next = next->link) != 0);
*last0 = l0;
*last1 = l1;
if (trivial)
show_from = show_to = 0;
*deletes = show_from;
*inserts = show_to;
}
VOID *
xmalloc (size)
size_t size;
{
register VOID *value;
if (size == 0)
size = 1;
value = (VOID *) malloc (size);
if (!value)
fatal ("memory exhausted");
return value;
}
VOID *
xrealloc (old, size)
VOID *old;
size_t size;
{
register VOID *value;
if (size == 0)
size = 1;
value = (VOID *) realloc (old, size);
if (!value)
fatal ("memory exhausted");
return value;
}
char *
concat (s1, s2, s3)
char const *s1, *s2, *s3;
{
size_t len = strlen (s1) + strlen (s2) + strlen (s3);
char *new = xmalloc (len + 1);
sprintf (new, "%s%s%s", s1, s2, s3);
return new;
}
char *
dir_file_pathname (dir, file)
char const *dir, *file;
{
char const *p = filename_lastdirchar (dir);
return concat (dir, "/" + (p && !p[1]), file);
}
void
debug_script (sp)
struct change *sp;
{
fflush (stdout);
for (; sp; sp = sp->link)
fprintf (stderr, "%3d %3d delete %d insert %d\n",
sp->line0, sp->line1, sp->deleted, sp->inserted);
fflush (stderr);
}