#include "system.h"
#include <stdio.h>
#include <signal.h>
#include "getopt.h"
extern char const version_string[];
#define	FILE0	0
#define	FILE1	1
#define	FILE2	2
#define	FILEC	FILE2
#define FO 0
#define FC 1
#define	START	0
#define	END	1
enum diff_type {
ERROR,
ADD,
CHANGE,
DELETE,
DIFF_ALL,
DIFF_1ST,
DIFF_2ND,
DIFF_3RD
};
struct diff_block {
int ranges[2][2];
char **lines[2];
size_t *lengths[2];
struct diff_block *next;
};
struct diff3_block {
enum diff_type correspond;
int ranges[3][2];
char **lines[3];
size_t *lengths[3];
struct diff3_block *next;
};
#define	D_LOWLINE(diff, filenum)	\
((diff)->ranges[filenum][START])
#define	D_HIGHLINE(diff, filenum)	\
((diff)->ranges[filenum][END])
#define	D_NUMLINES(diff, filenum)	\
(D_HIGHLINE (diff, filenum) - D_LOWLINE (diff, filenum) + 1)
#define	D_RELNUM(diff, filenum, linenum)	\
((diff)->lines[filenum][linenum])
#define	D_RELLEN(diff, filenum, linenum)	\
((diff)->lengths[filenum][linenum])
#define	D_LINEARRAY(diff, filenum)	\
((diff)->lines[filenum])
#define	D_LENARRAY(diff, filenum)	\
((diff)->lengths[filenum])
#define	D_NEXT(diff)	((diff)->next)
#define	D3_TYPE(diff)	((diff)->correspond)
#define	D_HIGH_MAPLINE(diff, fromfile, tofile, lineno)	\
((lineno)						\
- D_HIGHLINE ((diff), (fromfile))			\
+ D_HIGHLINE ((diff), (tofile)))
#define	D_LOW_MAPLINE(diff, fromfile, tofile, lineno)	\
((lineno)						\
- D_LOWLINE ((diff), (fromfile))			\
+ D_LOWLINE ((diff), (tofile)))
#define	ALLOCATE(number, type)	\
(type *) xmalloc ((number) * sizeof (type))
static int always_text;
static int edscript;
static int flagging;
static int horizon_lines = 10;
static int tab_align_flag;
static int simple_only;
static int overlap_only;
static int show_2nd;
static int finalwrite;
static int merge;
static char *program_name;
static VOID *xmalloc PARAMS((size_t));
static VOID *xrealloc PARAMS((VOID *, size_t));
static char *read_diff PARAMS((char const *, char const *, char **));
static char *scan_diff_line PARAMS((char *, char **, size_t *, char *, int));
static enum diff_type process_diff_control PARAMS((char **, struct diff_block *));
static int compare_line_list PARAMS((char * const[], size_t const[], char * const[], size_t const[], int));
static int copy_stringlist PARAMS((char * const[], size_t const[], char *[], size_t[], int));
static int dotlines PARAMS((FILE *, struct diff3_block *, int));
static int output_diff3_edscript PARAMS((FILE *, struct diff3_block *, int const[3], int const[3], char const *, char const *, char const *));
static int output_diff3_merge PARAMS((FILE *, FILE *, struct diff3_block *, int const[3], int const[3], char const *, char const *, char const *));
static size_t myread PARAMS((int, char *, size_t));
static struct diff3_block *create_diff3_block PARAMS((int, int, int, int, int, int));
static struct diff3_block *make_3way_diff PARAMS((struct diff_block *, struct diff_block *));
static struct diff3_block *reverse_diff3_blocklist PARAMS((struct diff3_block *));
static struct diff3_block *using_to_diff3_block PARAMS((struct diff_block *[2], struct diff_block *[2], int, int, struct diff3_block const *));
static struct diff_block *process_diff PARAMS((char const *, char const *, struct diff_block **));
static void check_stdout PARAMS((void));
static void fatal PARAMS((char const *));
static void output_diff3 PARAMS((FILE *, struct diff3_block *, int const[3], int const[3]));
static void perror_with_exit PARAMS((char const *));
static void try_help PARAMS((char const *));
static void undotlines PARAMS((FILE *, int, int, int));
static void usage PARAMS((void));
static char const diff_program[] = DIFF_PROGRAM;
static struct option const longopts[] =
{
{"text", 0, 0, 'a'},
{"show-all", 0, 0, 'A'},
{"ed", 0, 0, 'e'},
{"show-overlap", 0, 0, 'E'},
{"label", 1, 0, 'L'},
{"merge", 0, 0, 'm'},
{"initial-tab", 0, 0, 'T'},
{"overlap-only", 0, 0, 'x'},
{"easy-only", 0, 0, '3'},
{"version", 0, 0, 'v'},
{"help", 0, 0, 129},
{0, 0, 0, 0}
};
int
main (argc, argv)
int argc;
char **argv;
{
int c, i;
int mapping[3];
int rev_mapping[3];
int incompat = 0;
int conflicts_found;
struct diff_block *thread0, *thread1, *last_block;
struct diff3_block *diff3;
int tag_count = 0;
char *tag_strings[3];
char *commonname;
char **file;
struct stat statb;
initialize_main (&argc, &argv);
program_name = argv[0];
while ((c = getopt_long (argc, argv, "aeimvx3AEL:TX", longopts, 0)) != EOF)
{
switch (c)
{
case 'a':
always_text = 1;
break;
case 'A':
show_2nd = 1;
flagging = 1;
incompat++;
break;
case 'x':
overlap_only = 1;
incompat++;
break;
case '3':
simple_only = 1;
incompat++;
break;
case 'i':
finalwrite = 1;
break;
case 'm':
merge = 1;
break;
case 'X':
overlap_only = 1;
case 'E':
flagging = 1;
case 'e':
incompat++;
break;
case 'T':
tab_align_flag = 1;
break;
case 'v':
printf ("diff3 - GNU diffutils version %s\n", version_string);
exit (0);
case 129:
usage ();
check_stdout ();
exit (0);
case 'L':
if (tag_count < 3)
{
tag_strings[tag_count++] = optarg;
break;
}
try_help ("Too many labels were given.  The limit is 3.");
default:
try_help (0);
}
}
edscript = incompat & ~merge;
show_2nd |= ~incompat & merge;
flagging |= ~incompat & merge;
if (incompat > 1
|| finalwrite & merge
|| (tag_count && ! flagging))
try_help ("incompatible options");
if (argc - optind != 3)
try_help (argc - optind < 3 ? "missing operand" : "extra operand");
file = &argv[optind];
for (i = tag_count; i < 3; i++)
tag_strings[i] = file[i];
if (strcmp (file[2], "-") == 0)
{
if (strcmp (file[0], "-") == 0 || strcmp (file[1], "-") == 0)
fatal ("`-' specified for more than one input file");
mapping[0] = 0;
mapping[1] = 2;
mapping[2] = 1;
}
else
{
mapping[0] = 0;
mapping[1] = 1;
mapping[2] = 2;
}
for (i = 0; i < 3; i++)
rev_mapping[mapping[i]] = i;
for (i = 0; i < 3; i++)
if (strcmp (file[i], "-") != 0)
{
if (stat (file[i], &statb) < 0)
perror_with_exit (file[i]);
else if (S_ISDIR(statb.st_mode))
{
fprintf (stderr, "%s: %s: Is a directory\n",
program_name, file[i]);
exit (2);
}
}
#if !defined(SIGCHLD) && defined(SIGCLD)
#define SIGCHLD SIGCLD
#endif
#ifdef SIGCHLD
signal (SIGCHLD, SIG_DFL);
#endif
commonname = file[rev_mapping[FILEC]];
thread1 = process_diff (file[rev_mapping[FILE1]], commonname, &last_block);
if (thread1)
for (i = 0; i < 2; i++)
{
horizon_lines = max (horizon_lines, D_NUMLINES (thread1, i));
horizon_lines = max (horizon_lines, D_NUMLINES (last_block, i));
}
thread0 = process_diff (file[rev_mapping[FILE0]], commonname, &last_block);
diff3 = make_3way_diff (thread0, thread1);
if (edscript)
conflicts_found
= output_diff3_edscript (stdout, diff3, mapping, rev_mapping,
tag_strings[0], tag_strings[1], tag_strings[2]);
else if (merge)
{
if (! freopen (file[rev_mapping[FILE0]], "r", stdin))
perror_with_exit (file[rev_mapping[FILE0]]);
conflicts_found
= output_diff3_merge (stdin, stdout, diff3, mapping, rev_mapping,
tag_strings[0], tag_strings[1], tag_strings[2]);
if (ferror (stdin))
fatal ("read error");
}
else
{
output_diff3 (stdout, diff3, mapping, rev_mapping);
conflicts_found = 0;
}
check_stdout ();
exit (conflicts_found);
return conflicts_found;
}
static void
try_help (reason)
char const *reason;
{
if (reason)
fprintf (stderr, "%s: %s\n", program_name, reason);
fprintf (stderr, "%s: Try `%s --help' for more information.\n",
program_name, program_name);
exit (2);
}
static void
check_stdout ()
{
if (ferror (stdout) || fclose (stdout) != 0)
fatal ("write error");
}
static void
usage ()
{
printf ("Usage: %s [OPTION]... MYFILE OLDFILE YOURFILE\n\n", program_name);
printf ("%s", "\
-e  --ed  Output unmerged changes from OLDFILE to YOURFILE into MYFILE.\n\
-E  --show-overlap  Output unmerged changes, bracketing conflicts.\n\
-A  --show-all  Output all changes, bracketing conflicts.\n\
-x  --overlap-only  Output overlapping changes.\n\
-X  Output overlapping changes, bracketing them.\n\
-3  --easy-only  Output unmerged nonoverlapping changes.\n\n");
printf ("%s", "\
-m  --merge  Output merged file instead of ed script (default -A).\n\
-L LABEL  --label=LABEL  Use LABEL instead of file name.\n\
-i  Append `w' and `q' commands to ed scripts.\n\
-a  --text  Treat all files as text.\n\
-T  --initial-tab  Make tabs line up by prepending a tab.\n\n");
printf ("%s", "\
-v  --version  Output version info.\n\
--help  Output this help.\n\n");
printf ("If a FILE is `-', read standard input.\n");
}
static struct diff3_block *
make_3way_diff (thread0, thread1)
struct diff_block *thread0, *thread1;
{
struct diff_block
*using[2],
*last_using[2],
*current[2];
int
high_water_mark;
int
high_water_thread,
base_water_thread,
other_thread;
struct diff_block
*high_water_diff,
*other_diff;
struct diff3_block
*result,
*tmpblock,
**result_end;
struct diff3_block const *last_diff3;
static struct diff3_block const zero_diff3;
result = 0;
result_end = &result;
current[0] = thread0; current[1] = thread1;
last_diff3 = &zero_diff3;
while (current[0] || current[1])
{
using[0] = using[1] = last_using[0] = last_using[1] = 0;
if (!current[0])
base_water_thread = 1;
else if (!current[1])
base_water_thread = 0;
else
base_water_thread =
(D_LOWLINE (current[0], FC) > D_LOWLINE (current[1], FC));
high_water_thread = base_water_thread;
high_water_diff = current[high_water_thread];
#if 0
base_water_mark = D_LOWLINE (high_water_diff, FC);
#endif
high_water_mark = D_HIGHLINE (high_water_diff, FC);
using[high_water_thread]
= last_using[high_water_thread]
= high_water_diff;
current[high_water_thread] = high_water_diff->next;
last_using[high_water_thread]->next = 0;
other_thread = high_water_thread ^ 0x1;
other_diff = current[other_thread];
while (other_diff
&& D_LOWLINE (other_diff, FC) <= high_water_mark + 1)
{
if (using[other_thread])
last_using[other_thread]->next = other_diff;
else
using[other_thread] = other_diff;
last_using[other_thread] = other_diff;
current[other_thread] = current[other_thread]->next;
other_diff->next = 0;
if (high_water_mark < D_HIGHLINE (other_diff, FC))
{
high_water_thread ^= 1;
high_water_diff = other_diff;
high_water_mark = D_HIGHLINE (other_diff, FC);
}
other_thread = high_water_thread ^ 0x1;
other_diff = current[other_thread];
}
tmpblock = using_to_diff3_block (using, last_using,
base_water_thread, high_water_thread,
last_diff3);
if (!tmpblock)
fatal ("internal error: screwup in format of diff blocks");
*result_end = tmpblock;
result_end = &tmpblock->next;
last_diff3 = tmpblock;
}
return result;
}
static struct diff3_block *
using_to_diff3_block (using, last_using, low_thread, high_thread, last_diff3)
struct diff_block
*using[2],
*last_using[2];
int low_thread, high_thread;
struct diff3_block const *last_diff3;
{
int low[2], high[2];
struct diff3_block *result;
struct diff_block *ptr;
int d, i;
int lowc = D_LOWLINE (using[low_thread], FC);
int highc = D_HIGHLINE (last_using[high_thread], FC);
for (d = 0; d < 2; d++)
if (using[d])
{
low[d] = D_LOW_MAPLINE (using[d], FC, FO, lowc);
high[d] = D_HIGH_MAPLINE (last_using[d], FC, FO, highc);
}
else
{
low[d] = D_HIGH_MAPLINE (last_diff3, FILEC, FILE0 + d, lowc);
high[d] = D_HIGH_MAPLINE (last_diff3, FILEC, FILE0 + d, highc);
}
result = create_diff3_block (low[0], high[0], low[1], high[1], lowc, highc);
for (d = 0; d < 2; d++)
for (ptr = using[d]; ptr; ptr = D_NEXT (ptr))
{
int result_offset = D_LOWLINE (ptr, FC) - lowc;
if (!copy_stringlist (D_LINEARRAY (ptr, FC),
D_LENARRAY (ptr, FC),
D_LINEARRAY (result, FILEC) + result_offset,
D_LENARRAY (result, FILEC) + result_offset,
D_NUMLINES (ptr, FC)))
return 0;
}
for (d = 0; d < 2; d++)
{
struct diff_block *u = using[d];
int lo = low[d], hi = high[d];
for (i = 0;
i + lo < (u ? D_LOWLINE (u, FO) : hi + 1);
i++)
{
D_RELNUM (result, FILE0 + d, i) = D_RELNUM (result, FILEC, i);
D_RELLEN (result, FILE0 + d, i) = D_RELLEN (result, FILEC, i);
}
for (ptr = u; ptr; ptr = D_NEXT (ptr))
{
int result_offset = D_LOWLINE (ptr, FO) - lo;
int linec;
if (!copy_stringlist (D_LINEARRAY (ptr, FO),
D_LENARRAY (ptr, FO),
D_LINEARRAY (result, FILE0 + d) + result_offset,
D_LENARRAY (result, FILE0 + d) + result_offset,
D_NUMLINES (ptr, FO)))
return 0;
linec = D_HIGHLINE (ptr, FC) + 1 - lowc;
for (i = D_HIGHLINE (ptr, FO) + 1 - lo;
i < (D_NEXT (ptr) ? D_LOWLINE (D_NEXT (ptr), FO) : hi + 1) - lo;
i++)
{
D_RELNUM (result, FILE0 + d, i) = D_RELNUM (result, FILEC, linec);
D_RELLEN (result, FILE0 + d, i) = D_RELLEN (result, FILEC, linec);
linec++;
}
}
}
if (!using[0])
D3_TYPE (result) = DIFF_2ND;
else if (!using[1])
D3_TYPE (result) = DIFF_1ST;
else
{
int nl0 = D_NUMLINES (result, FILE0);
int nl1 = D_NUMLINES (result, FILE1);
if (nl0 != nl1
|| !compare_line_list (D_LINEARRAY (result, FILE0),
D_LENARRAY (result, FILE0),
D_LINEARRAY (result, FILE1),
D_LENARRAY (result, FILE1),
nl0))
D3_TYPE (result) = DIFF_ALL;
else
D3_TYPE (result) = DIFF_3RD;
}
return result;
}
static int
copy_stringlist (fromptrs, fromlengths, toptrs, tolengths, copynum)
char * const fromptrs[];
char *toptrs[];
size_t const fromlengths[];
size_t tolengths[];
int copynum;
{
register char * const *f = fromptrs;
register char **t = toptrs;
register size_t const *fl = fromlengths;
register size_t *tl = tolengths;
while (copynum--)
{
if (*t)
{ if (*fl != *tl || memcmp (*f, *t, *fl)) return 0; }
else
{ *t = *f ; *tl = *fl; }
t++; f++; tl++; fl++;
}
return 1;
}
static struct diff3_block *
create_diff3_block (low0, high0, low1, high1, low2, high2)
register int low0, high0, low1, high1, low2, high2;
{
struct diff3_block *result = ALLOCATE (1, struct diff3_block);
int numlines;
D3_TYPE (result) = ERROR;
D_NEXT (result) = 0;
D_LOWLINE (result, FILE0) = low0;
D_HIGHLINE (result, FILE0) = high0;
D_LOWLINE (result, FILE1) = low1;
D_HIGHLINE (result, FILE1) = high1;
D_LOWLINE (result, FILE2) = low2;
D_HIGHLINE (result, FILE2) = high2;
numlines = D_NUMLINES (result, FILE0);
if (numlines)
{
D_LINEARRAY (result, FILE0) = ALLOCATE (numlines, char *);
D_LENARRAY (result, FILE0) = ALLOCATE (numlines, size_t);
bzero (D_LINEARRAY (result, FILE0), (numlines * sizeof (char *)));
bzero (D_LENARRAY (result, FILE0), (numlines * sizeof (size_t)));
}
else
{
D_LINEARRAY (result, FILE0) = 0;
D_LENARRAY (result, FILE0) = 0;
}
numlines = D_NUMLINES (result, FILE1);
if (numlines)
{
D_LINEARRAY (result, FILE1) = ALLOCATE (numlines, char *);
D_LENARRAY (result, FILE1) = ALLOCATE (numlines, size_t);
bzero (D_LINEARRAY (result, FILE1), (numlines * sizeof (char *)));
bzero (D_LENARRAY (result, FILE1), (numlines * sizeof (size_t)));
}
else
{
D_LINEARRAY (result, FILE1) = 0;
D_LENARRAY (result, FILE1) = 0;
}
numlines = D_NUMLINES (result, FILE2);
if (numlines)
{
D_LINEARRAY (result, FILE2) = ALLOCATE (numlines, char *);
D_LENARRAY (result, FILE2) = ALLOCATE (numlines, size_t);
bzero (D_LINEARRAY (result, FILE2), (numlines * sizeof (char *)));
bzero (D_LENARRAY (result, FILE2), (numlines * sizeof (size_t)));
}
else
{
D_LINEARRAY (result, FILE2) = 0;
D_LENARRAY (result, FILE2) = 0;
}
return result;
}
static int
compare_line_list (list1, lengths1, list2, lengths2, nl)
char * const list1[], * const list2[];
size_t const lengths1[], lengths2[];
int nl;
{
char
* const *l1 = list1,
* const *l2 = list2;
size_t const
*lgths1 = lengths1,
*lgths2 = lengths2;
while (nl--)
if (!*l1 || !*l2 || *lgths1 != *lgths2++
|| memcmp (*l1++, *l2++, *lgths1++))
return 0;
return 1;
}
extern char **environ;
static struct diff_block *
process_diff (filea, fileb, last_block)
char const *filea, *fileb;
struct diff_block **last_block;
{
char *diff_contents;
char *diff_limit;
char *scan_diff;
enum diff_type dt;
int i;
struct diff_block *block_list, **block_list_end, *bptr;
diff_limit = read_diff (filea, fileb, &diff_contents);
scan_diff = diff_contents;
block_list_end = &block_list;
bptr = 0;
while (scan_diff < diff_limit)
{
bptr = ALLOCATE (1, struct diff_block);
bptr->lines[0] = bptr->lines[1] = 0;
bptr->lengths[0] = bptr->lengths[1] = 0;
dt = process_diff_control (&scan_diff, bptr);
if (dt == ERROR || *scan_diff != '\n')
{
fprintf (stderr, "%s: diff error: ", program_name);
do
{
putc (*scan_diff, stderr);
}
while (*scan_diff++ != '\n');
exit (2);
}
scan_diff++;
switch (dt)
{
case ADD:
bptr->ranges[0][0]++;
break;
case DELETE:
bptr->ranges[1][0]++;
break;
case CHANGE:
break;
default:
fatal ("internal error: invalid diff type in process_diff");
break;
}
if (dt != ADD)
{
int numlines = D_NUMLINES (bptr, 0);
bptr->lines[0] = ALLOCATE (numlines, char *);
bptr->lengths[0] = ALLOCATE (numlines, size_t);
for (i = 0; i < numlines; i++)
scan_diff = scan_diff_line (scan_diff,
&(bptr->lines[0][i]),
&(bptr->lengths[0][i]),
diff_limit,
'<');
}
if (dt == CHANGE)
{
if (strncmp (scan_diff, "---\n", 4))
fatal ("invalid diff format; invalid change separator");
scan_diff += 4;
}
if (dt != DELETE)
{
int numlines = D_NUMLINES (bptr, 1);
bptr->lines[1] = ALLOCATE (numlines, char *);
bptr->lengths[1] = ALLOCATE (numlines, size_t);
for (i = 0; i < numlines; i++)
scan_diff = scan_diff_line (scan_diff,
&(bptr->lines[1][i]),
&(bptr->lengths[1][i]),
diff_limit,
'>');
}
*block_list_end = bptr;
block_list_end = &bptr->next;
}
*block_list_end = 0;
*last_block = bptr;
return block_list;
}
static enum diff_type
process_diff_control (string, db)
char **string;
struct diff_block *db;
{
char *s = *string;
int holdnum;
enum diff_type type;
#define	SKIPWHITE(s)	{ while (*s == ' ' || *s == '\t') s++; }
#define	READNUM(s, num)	\
{ unsigned char c = *s; if (!ISDIGIT (c)) return ERROR; holdnum = 0; \
do { holdnum = (c - '0' + holdnum * 10); }	\
while (ISDIGIT (c = *++s)); (num) = holdnum; }
SKIPWHITE (s);
READNUM (s, db->ranges[0][START]);
SKIPWHITE (s);
if (*s == ',')
{
s++;
READNUM (s, db->ranges[0][END]);
}
else
db->ranges[0][END] = db->ranges[0][START];
SKIPWHITE (s);
switch (*s)
{
case 'a':
type = ADD;
break;
case 'c':
type = CHANGE;
break;
case 'd':
type = DELETE;
break;
default:
return ERROR;
}
s++;
SKIPWHITE (s);
READNUM (s, db->ranges[1][START]);
SKIPWHITE (s);
if (*s == ',')
{
s++;
READNUM (s, db->ranges[1][END]);
SKIPWHITE (s);
}
else
db->ranges[1][END] = db->ranges[1][START];
*string = s;
return type;
}
static char *
read_diff (filea, fileb, output_placement)
char const *filea, *fileb;
char **output_placement;
{
char *diff_result;
size_t bytes, current_chunk_size, total;
int fd, wstatus;
struct stat pipestat;
#define INT_STRLEN_BOUND(type) ((sizeof(type)*CHAR_BIT - 1) * 302 / 1000 + 2)
#if HAVE_FORK
char const *argv[7];
char horizon_arg[17 + INT_STRLEN_BOUND (int)];
char const **ap;
int fds[2];
pid_t pid;
ap = argv;
*ap++ = diff_program;
if (always_text)
*ap++ = "-a";
sprintf (horizon_arg, "--horizon-lines=%d", horizon_lines);
*ap++ = horizon_arg;
*ap++ = "--";
*ap++ = filea;
*ap++ = fileb;
*ap = 0;
if (pipe (fds) != 0)
perror_with_exit ("pipe");
pid = fork ();
if (pid == 0)
{
close (fds[0]);
if (fds[1] != STDOUT_FILENO)
{
dup2 (fds[1], STDOUT_FILENO);
close (fds[1]);
}
execve (diff_program, (char **) argv, environ);
write (STDERR_FILENO, diff_program, strlen (diff_program));
write (STDERR_FILENO, ": not found\n", 12);
_exit (2);
}
if (pid == -1)
perror_with_exit ("fork failed");
close (fds[1]);
fd = fds[0];
#else
FILE *fpipe;
char *command = xmalloc (sizeof (diff_program) + 30 + INT_STRLEN_BOUND (int)
+ 4 * (strlen (filea) + strlen (fileb)));
char *p;
sprintf (command, "%s -a --horizon-lines=%d -- ",
diff_program, horizon_lines);
p = command + strlen (command);
SYSTEM_QUOTE_ARG (p, filea);
*p++ = ' ';
SYSTEM_QUOTE_ARG (p, fileb);
*p = '\0';
fpipe = popen (command, "r");
if (!fpipe)
perror_with_exit (command);
free (command);
fd = fileno (fpipe);
#endif
current_chunk_size = 8 * 1024;
if (fstat (fd, &pipestat) == 0)
current_chunk_size = max (current_chunk_size, STAT_BLOCKSIZE (pipestat));
diff_result = xmalloc (current_chunk_size);
total = 0;
do {
bytes = myread (fd,
diff_result + total,
current_chunk_size - total);
total += bytes;
if (total == current_chunk_size)
{
if (current_chunk_size < 2 * current_chunk_size)
current_chunk_size = 2 * current_chunk_size;
else if (current_chunk_size < (size_t) -1)
current_chunk_size = (size_t) -1;
else
fatal ("files are too large to fit into memory");
diff_result = xrealloc (diff_result, (current_chunk_size *= 2));
}
} while (bytes);
if (total != 0 && diff_result[total-1] != '\n')
fatal ("invalid diff format; incomplete last line");
*output_placement = diff_result;
#if ! HAVE_FORK
wstatus = pclose (fpipe);
#else
if (close (fd) != 0)
perror_with_exit ("pipe close");
if (waitpid (pid, &wstatus, 0) < 0)
perror_with_exit ("waitpid failed");
#endif
if (! (WIFEXITED (wstatus) && WEXITSTATUS (wstatus) < 2))
fatal ("subsidiary diff failed");
return diff_result + total;
}
static char *
scan_diff_line (scan_ptr, set_start, set_length, limit, leadingchar)
char *scan_ptr, **set_start;
size_t *set_length;
char *limit;
int leadingchar;
{
char *line_ptr;
if (!(scan_ptr[0] == leadingchar
&& scan_ptr[1] == ' '))
fatal ("invalid diff format; incorrect leading line chars");
*set_start = line_ptr = scan_ptr + 2;
while (*line_ptr++ != '\n')
;
*set_length = line_ptr - *set_start;
if (line_ptr < limit && *line_ptr == '\\')
{
if (edscript)
fprintf (stderr, "%s:", program_name);
else
--*set_length;
line_ptr++;
do
{
if (edscript)
putc (*line_ptr, stderr);
}
while (*line_ptr++ != '\n');
}
return line_ptr;
}
static void
output_diff3 (outputfile, diff, mapping, rev_mapping)
FILE *outputfile;
struct diff3_block *diff;
int const mapping[3], rev_mapping[3];
{
int i;
int oddoneout;
char *cp;
struct diff3_block *ptr;
int line;
size_t length;
int dontprint;
static int skew_increment[3] = { 2, 3, 1 };
char const *line_prefix = tab_align_flag ? "\t" : "  ";
for (ptr = diff; ptr; ptr = D_NEXT (ptr))
{
char x[2];
switch (ptr->correspond)
{
case DIFF_ALL:
x[0] = '\0';
dontprint = 3;
oddoneout = 3;
break;
case DIFF_1ST:
case DIFF_2ND:
case DIFF_3RD:
oddoneout = rev_mapping[(int) ptr->correspond - (int) DIFF_1ST];
x[0] = oddoneout + '1';
x[1] = '\0';
dontprint = oddoneout==0;
break;
default:
fatal ("internal error: invalid diff type passed to output");
}
fprintf (outputfile, "====%s\n", x);
for (i = 0; i < 3;
i = (oddoneout == 1 ? skew_increment[i] : i + 1))
{
int realfile = mapping[i];
int
lowt = D_LOWLINE (ptr, realfile),
hight = D_HIGHLINE (ptr, realfile);
fprintf (outputfile, "%d:", i + 1);
switch (lowt - hight)
{
case 1:
fprintf (outputfile, "%da\n", lowt - 1);
break;
case 0:
fprintf (outputfile, "%dc\n", lowt);
break;
default:
fprintf (outputfile, "%d,%dc\n", lowt, hight);
break;
}
if (i == dontprint) continue;
if (lowt <= hight)
{
line = 0;
do
{
fprintf (outputfile, line_prefix);
cp = D_RELNUM (ptr, realfile, line);
length = D_RELLEN (ptr, realfile, line);
fwrite (cp, sizeof (char), length, outputfile);
}
while (++line < hight - lowt + 1);
if (cp[length - 1] != '\n')
fprintf (outputfile, "\n\\ No newline at end of file\n");
}
}
}
}
static int
dotlines (outputfile, b, filenum)
FILE *outputfile;
struct diff3_block *b;
int filenum;
{
int i;
int leading_dot = 0;
for (i = 0;
i < D_NUMLINES (b, filenum);
i++)
{
char *line = D_RELNUM (b, filenum, i);
if (line[0] == '.')
{
leading_dot = 1;
fprintf (outputfile, ".");
}
fwrite (line, sizeof (char),
D_RELLEN (b, filenum, i), outputfile);
}
return leading_dot;
}
static void
undotlines (outputfile, leading_dot, start, num)
FILE *outputfile;
int leading_dot, start, num;
{
fprintf (outputfile, ".\n");
if (leading_dot)
if (num == 1)
fprintf (outputfile, "%ds/^\\.
else
fprintf (outputfile, "%d,%ds/^\\.
}
static int
output_diff3_edscript (outputfile, diff, mapping, rev_mapping,
file0, file1, file2)
FILE *outputfile;
struct diff3_block *diff;
int const mapping[3], rev_mapping[3];
char const *file0, *file1, *file2;
{
int leading_dot;
int conflicts_found = 0, conflict;
struct diff3_block *b;
for (b = reverse_diff3_blocklist (diff); b; b = b->next)
{
enum diff_type type
= ((b->correspond == DIFF_ALL) ?
DIFF_ALL :
((enum diff_type)
(((int) DIFF_1ST)
+ rev_mapping[(int) b->correspond - (int) DIFF_1ST])));
switch (type)
{
default: continue;
case DIFF_2ND: if (!show_2nd) continue; conflict = 1; break;
case DIFF_3RD: if (overlap_only) continue; conflict = 0; break;
case DIFF_ALL: if (simple_only) continue; conflict = flagging; break;
}
if (conflict)
{
conflicts_found = 1;
fprintf (outputfile, "%da\n", D_HIGHLINE (b, mapping[FILE0]));
leading_dot = 0;
if (type == DIFF_ALL)
{
if (show_2nd)
{
fprintf (outputfile, "||||||| %s\n", file1);
leading_dot = dotlines (outputfile, b, mapping[FILE1]);
}
fprintf (outputfile, "=======\n");
leading_dot |= dotlines (outputfile, b, mapping[FILE2]);
}
fprintf (outputfile, ">>>>>>> %s\n", file2);
undotlines (outputfile, leading_dot,
D_HIGHLINE (b, mapping[FILE0]) + 2,
(D_NUMLINES (b, mapping[FILE1])
+ D_NUMLINES (b, mapping[FILE2]) + 1));
fprintf (outputfile, "%da\n<<<<<<< %s\n",
D_LOWLINE (b, mapping[FILE0]) - 1,
type == DIFF_ALL ? file0 : file1);
leading_dot = 0;
if (type == DIFF_2ND)
{
leading_dot = dotlines (outputfile, b, mapping[FILE1]);
fprintf (outputfile, "=======\n");
}
undotlines (outputfile, leading_dot,
D_LOWLINE (b, mapping[FILE0]) + 1,
D_NUMLINES (b, mapping[FILE1]));
}
else if (D_NUMLINES (b, mapping[FILE2]) == 0)
{
if (D_NUMLINES (b, mapping[FILE0]) == 1)
fprintf (outputfile, "%dd\n",
D_LOWLINE (b, mapping[FILE0]));
else
fprintf (outputfile, "%d,%dd\n",
D_LOWLINE (b, mapping[FILE0]),
D_HIGHLINE (b, mapping[FILE0]));
}
else
{
switch (D_NUMLINES (b, mapping[FILE0]))
{
case 0:
fprintf (outputfile, "%da\n",
D_HIGHLINE (b, mapping[FILE0]));
break;
case 1:
fprintf (outputfile, "%dc\n",
D_HIGHLINE (b, mapping[FILE0]));
break;
default:
fprintf (outputfile, "%d,%dc\n",
D_LOWLINE (b, mapping[FILE0]),
D_HIGHLINE (b, mapping[FILE0]));
break;
}
undotlines (outputfile, dotlines (outputfile, b, mapping[FILE2]),
D_LOWLINE (b, mapping[FILE0]),
D_NUMLINES (b, mapping[FILE2]));
}
}
if (finalwrite) fprintf (outputfile, "w\nq\n");
return conflicts_found;
}
static int
output_diff3_merge (infile, outputfile, diff, mapping, rev_mapping,
file0, file1, file2)
FILE *infile, *outputfile;
struct diff3_block *diff;
int const mapping[3], rev_mapping[3];
char const *file0, *file1, *file2;
{
int c, i;
int conflicts_found = 0, conflict;
struct diff3_block *b;
int linesread = 0;
for (b = diff; b; b = b->next)
{
enum diff_type type
= ((b->correspond == DIFF_ALL) ?
DIFF_ALL :
((enum diff_type)
(((int) DIFF_1ST)
+ rev_mapping[(int) b->correspond - (int) DIFF_1ST])));
char const *format_2nd = "<<<<<<< %s\n";
switch (type)
{
default: continue;
case DIFF_2ND: if (!show_2nd) continue; conflict = 1; break;
case DIFF_3RD: if (overlap_only) continue; conflict = 0; break;
case DIFF_ALL: if (simple_only) continue; conflict = flagging;
format_2nd = "||||||| %s\n";
break;
}
i = D_LOWLINE (b, FILE0) - linesread - 1;
linesread += i;
while (0 <= --i)
do
{
c = getc (infile);
if (c == EOF)
if (ferror (infile))
perror_with_exit ("input file");
else if (feof (infile))
fatal ("input file shrank");
putc (c, outputfile);
}
while (c != '\n');
if (conflict)
{
conflicts_found = 1;
if (type == DIFF_ALL)
{
fprintf (outputfile, "<<<<<<< %s\n", file0);
for (i = 0;
i < D_NUMLINES (b, mapping[FILE0]);
i++)
fwrite (D_RELNUM (b, mapping[FILE0], i), sizeof (char),
D_RELLEN (b, mapping[FILE0], i), outputfile);
}
if (show_2nd)
{
fprintf (outputfile, format_2nd, file1);
for (i = 0;
i < D_NUMLINES (b, mapping[FILE1]);
i++)
fwrite (D_RELNUM (b, mapping[FILE1], i), sizeof (char),
D_RELLEN (b, mapping[FILE1], i), outputfile);
}
fprintf (outputfile, "=======\n");
}
for (i = 0;
i < D_NUMLINES (b, mapping[FILE2]);
i++)
fwrite (D_RELNUM (b, mapping[FILE2], i), sizeof (char),
D_RELLEN (b, mapping[FILE2], i), outputfile);
if (conflict)
fprintf (outputfile, ">>>>>>> %s\n", file2);
i = D_NUMLINES (b, FILE0);
linesread += i;
while (0 <= --i)
while ((c = getc (infile)) != '\n')
if (c == EOF)
if (ferror (infile))
perror_with_exit ("input file");
else if (feof (infile))
{
if (i || b->next)
fatal ("input file shrank");
return conflicts_found;
}
}
while ((c = getc (infile)) != EOF || !(ferror (infile) | feof (infile)))
putc (c, outputfile);
return conflicts_found;
}
static struct diff3_block *
reverse_diff3_blocklist (diff)
struct diff3_block *diff;
{
register struct diff3_block *tmp, *next, *prev;
for (tmp = diff, prev = 0;  tmp;  tmp = next)
{
next = tmp->next;
tmp->next = prev;
prev = tmp;
}
return prev;
}
static size_t
myread (fd, ptr, size)
int fd;
char *ptr;
size_t size;
{
size_t result = read (fd, ptr, size);
if (result == -1)
perror_with_exit ("read failed");
return result;
}
static VOID *
xmalloc (size)
size_t size;
{
VOID *result = (VOID *) malloc (size ? size : 1);
if (!result)
fatal ("memory exhausted");
return result;
}
static VOID *
xrealloc (ptr, size)
VOID *ptr;
size_t size;
{
VOID *result = (VOID *) realloc (ptr, size ? size : 1);
if (!result)
fatal ("memory exhausted");
return result;
}
static void
fatal (string)
char const *string;
{
fprintf (stderr, "%s: %s\n", program_name, string);
exit (2);
}
static void
perror_with_exit (string)
char const *string;
{
int e = errno;
fprintf (stderr, "%s: ", program_name);
errno = e;
perror (string);
exit (2);
}