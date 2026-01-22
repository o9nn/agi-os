#define GDIFF_MAIN
#include "diff.h"
#include <signal.h>
#include "getopt.h"
#ifdef __FreeBSD__
#include <locale.h>
#include <fnmatch.h>
#else
#include "fnmatch.h"
#endif
#include "prepend_args.h"
#ifndef DEFAULT_WIDTH
#define DEFAULT_WIDTH 130
#endif
#ifndef GUTTER_WIDTH_MINIMUM
#define GUTTER_WIDTH_MINIMUM 3
#endif
static char const *filetype PARAMS((struct stat const *));
static char *option_list PARAMS((char **, int));
static int add_exclude_file PARAMS((char const *));
static int ck_atoi PARAMS((char const *, int *));
static int compare_files PARAMS((char const *, char const *, char const *, char const *, int));
static int specify_format PARAMS((char **, char *));
static void add_exclude PARAMS((char const *));
static void add_regexp PARAMS((struct regexp_list **, char const *));
static void specify_style PARAMS((enum output_style));
static void try_help PARAMS((char const *));
static void check_stdout PARAMS((void));
static void usage PARAMS((void));
static int recursive;
int no_discards;
#if HAVE_SETMODE
static int binary_I_O;
#endif
static char *
option_list (optionvec, count)
char **optionvec;
int count;
{
int i;
size_t length = 0;
char *result;
for (i = 0; i < count; i++)
length += strlen (optionvec[i]) + 1;
result = xmalloc (length + 1);
result[0] = 0;
for (i = 0; i < count; i++)
{
strcat (result, " ");
strcat (result, optionvec[i]);
}
return result;
}
static int
ck_atoi (str, out)
char const *str;
int *out;
{
char const *p;
for (p = str; *p; p++)
if (*p < '0' || *p > '9')
return -1;
*out = atoi (optarg);
return 0;
}
static char const **exclude;
static int exclude_alloc, exclude_count;
int
excluded_filename (f)
char const *f;
{
int i;
for (i = 0;  i < exclude_count;  i++)
if (fnmatch (exclude[i], f, 0) == 0)
return 1;
return 0;
}
static void
add_exclude (pattern)
char const *pattern;
{
if (exclude_alloc <= exclude_count)
exclude = (char const **)
(exclude_alloc == 0
? xmalloc ((exclude_alloc = 64) * sizeof (*exclude))
: xrealloc (exclude, (exclude_alloc *= 2) * sizeof (*exclude)));
exclude[exclude_count++] = pattern;
}
static int
add_exclude_file (name)
char const *name;
{
struct file_data f;
char *p, *q, *lim;
f.name = optarg;
f.desc = (strcmp (name, "-") == 0
? STDIN_FILENO
: open (name, O_RDONLY, 0));
if (f.desc < 0 || fstat (f.desc, &f.stat) != 0)
return -1;
sip (&f, 1);
slurp (&f);
for (p = f.buffer, lim = p + f.buffered_chars;  p < lim;  p = q)
{
q = (char *) memchr (p, '\n', lim - p);
if (!q)
q = lim;
*q++ = 0;
add_exclude (p);
}
return close (f.desc);
}
static struct option const longopts[] =
{
{"ignore-blank-lines", 0, 0, 'B'},
{"context", 2, 0, 'C'},
{"ifdef", 1, 0, 'D'},
{"show-function-line", 1, 0, 'F'},
{"speed-large-files", 0, 0, 'H'},
{"ignore-matching-lines", 1, 0, 'I'},
{"label", 1, 0, 'L'},
{"file-label", 1, 0, 'L'},
{"new-file", 0, 0, 'N'},
{"entire-new-file", 0, 0, 'N'},
{"unidirectional-new-file", 0, 0, 'P'},
{"starting-file", 1, 0, 'S'},
{"initial-tab", 0, 0, 'T'},
{"width", 1, 0, 'W'},
{"text", 0, 0, 'a'},
{"ascii", 0, 0, 'a'},
{"ignore-space-change", 0, 0, 'b'},
{"minimal", 0, 0, 'd'},
{"ed", 0, 0, 'e'},
{"forward-ed", 0, 0, 'f'},
{"ignore-case", 0, 0, 'i'},
{"paginate", 0, 0, 'l'},
{"print", 0, 0, 'l'},
{"rcs", 0, 0, 'n'},
{"show-c-function", 0, 0, 'p'},
{"brief", 0, 0, 'q'},
{"recursive", 0, 0, 'r'},
{"report-identical-files", 0, 0, 's'},
{"expand-tabs", 0, 0, 't'},
{"version", 0, 0, 'v'},
{"ignore-all-space", 0, 0, 'w'},
{"exclude", 1, 0, 'x'},
{"exclude-from", 1, 0, 'X'},
{"side-by-side", 0, 0, 'y'},
{"unified", 2, 0, 'U'},
{"left-column", 0, 0, 129},
{"suppress-common-lines", 0, 0, 130},
{"sdiff-merge-assist", 0, 0, 131},
{"old-line-format", 1, 0, 132},
{"new-line-format", 1, 0, 133},
{"unchanged-line-format", 1, 0, 134},
{"line-format", 1, 0, 135},
{"old-group-format", 1, 0, 136},
{"new-group-format", 1, 0, 137},
{"unchanged-group-format", 1, 0, 138},
{"changed-group-format", 1, 0, 139},
{"horizon-lines", 1, 0, 140},
{"help", 0, 0, 141},
{"binary", 0, 0, 142},
{0, 0, 0, 0}
};
int
main (argc, argv)
int argc;
char *argv[];
{
int val;
int c;
int prev = -1;
int width = DEFAULT_WIDTH;
int show_c_function = 0;
#ifdef __FreeBSD__
setlocale(LC_ALL, "");
#endif
initialize_main (&argc, &argv);
program_name = argv[0];
output_style = OUTPUT_NORMAL;
context = -1;
prepend_default_options (getenv ("DIFF_OPTIONS"), &argc, &argv);
while ((c = getopt_long (argc, argv,
"0123456789abBcC:dD:efF:hHiI:lL:nNopPqrsS:tTuU:vwW:x:X:y",
longopts, 0)) != EOF)
{
switch (c)
{
case '1':
case '2':
case '3':
case '4':
case '5':
case '6':
case '7':
case '8':
case '9':
case '0':
if (context == -1)
context = 0;
else if (prev < '0' || prev > '9')
fatal ("context length specified twice");
context = context * 10 + c - '0';
break;
case 'a':
always_text_flag = 1;
break;
case 'b':
ignore_space_change_flag = 1;
ignore_some_changes = 1;
ignore_some_line_changes = 1;
break;
case 'B':
ignore_blank_lines_flag = 1;
ignore_some_changes = 1;
break;
case 'C':
case 'U':
if (optarg)
{
if (context >= 0)
fatal ("context length specified twice");
if (ck_atoi (optarg, &context))
fatal ("invalid context length argument");
}
case 'c':
specify_style (c == 'U' ? OUTPUT_UNIFIED : OUTPUT_CONTEXT);
break;
case 'd':
no_discards = 1;
break;
case 'D':
specify_style (OUTPUT_IFDEF);
{
int i, err = 0;
static char const C_ifdef_group_formats[] =
"#ifndef %s\n%%<#endif \n%c#ifdef %s\n%%>#endif \n%c%%=%c#ifndef %s\n%%<#else \n%%>#endif \n";
char *b = xmalloc (sizeof (C_ifdef_group_formats)
+ 7 * strlen(optarg) - 14
- 8 );
sprintf (b, C_ifdef_group_formats,
optarg, optarg, 0,
optarg, optarg, 0, 0,
optarg, optarg, optarg);
for (i = 0; i < 4; i++)
{
err |= specify_format (&group_format[i], b);
b += strlen (b) + 1;
}
if (err)
error ("conflicting #ifdef formats", 0, 0);
}
break;
case 'e':
specify_style (OUTPUT_ED);
break;
case 'f':
specify_style (OUTPUT_FORWARD_ED);
break;
case 'F':
add_regexp (&function_regexp_list, optarg);
break;
case 'h':
break;
case 'H':
heuristic = 1;
break;
case 'i':
ignore_case_flag = 1;
ignore_some_changes = 1;
ignore_some_line_changes = 1;
break;
case 'I':
add_regexp (&ignore_regexp_list, optarg);
ignore_some_changes = 1;
break;
case 'l':
paginate_flag = 1;
#if !defined(SIGCHLD) && defined(SIGCLD)
#define SIGCHLD SIGCLD
#endif
#ifdef SIGCHLD
signal (SIGCHLD, SIG_DFL);
#endif
break;
case 'L':
if (!file_label[0])
file_label[0] = optarg;
else if (!file_label[1])
file_label[1] = optarg;
else
fatal ("too many file label options");
break;
case 'n':
specify_style (OUTPUT_RCS);
break;
case 'N':
entire_new_file_flag = 1;
break;
case 'o':
specify_style (OUTPUT_NORMAL);
break;
case 'p':
show_c_function = 1;
add_regexp (&function_regexp_list, "^[_a-zA-Z$]");
break;
case 'P':
unidirectional_new_file_flag = 1;
break;
case 'q':
no_details_flag = 1;
break;
case 'r':
recursive = 1;
break;
case 's':
print_file_same_flag = 1;
break;
case 'S':
dir_start_file = optarg;
break;
case 't':
tab_expand_flag = 1;
break;
case 'T':
tab_align_flag = 1;
break;
case 'u':
specify_style (OUTPUT_UNIFIED);
break;
case 'v':
printf ("diff - GNU diffutils version %s\n", version_string);
exit (0);
case 'w':
ignore_all_space_flag = 1;
ignore_some_changes = 1;
ignore_some_line_changes = 1;
break;
case 'x':
add_exclude (optarg);
break;
case 'X':
if (add_exclude_file (optarg) != 0)
pfatal_with_name (optarg);
break;
case 'y':
specify_style (OUTPUT_SDIFF);
break;
case 'W':
if (ck_atoi (optarg, &width) || width <= 0)
fatal ("column width must be a positive integer");
break;
case 129:
sdiff_left_only = 1;
break;
case 130:
sdiff_skip_common_lines = 1;
break;
case 131:
specify_style (OUTPUT_SDIFF);
sdiff_help_sdiff = 1;
break;
case 132:
case 133:
case 134:
specify_style (OUTPUT_IFDEF);
if (specify_format (&line_format[c - 132], optarg) != 0)
error ("conflicting line format", 0, 0);
break;
case 135:
specify_style (OUTPUT_IFDEF);
{
int i, err = 0;
for (i = 0; i < sizeof (line_format) / sizeof (*line_format); i++)
err |= specify_format (&line_format[i], optarg);
if (err)
error ("conflicting line format", 0, 0);
}
break;
case 136:
case 137:
case 138:
case 139:
specify_style (OUTPUT_IFDEF);
if (specify_format (&group_format[c - 136], optarg) != 0)
error ("conflicting group format", 0, 0);
break;
case 140:
if (ck_atoi (optarg, &horizon_lines) || horizon_lines < 0)
fatal ("horizon must be a nonnegative integer");
break;
case 141:
usage ();
check_stdout ();
exit (0);
case 142:
#if HAVE_SETMODE
binary_I_O = 1;
setmode (STDOUT_FILENO, O_BINARY);
#endif
break;
default:
try_help (0);
}
prev = c;
}
if (argc - optind != 2)
try_help (argc - optind < 2 ? "missing operand" : "extra operand");
{
int t = tab_expand_flag ? 1 : TAB_WIDTH;
int off = (width + t + GUTTER_WIDTH_MINIMUM) / (2*t)  *  t;
sdiff_half_width = max (0, min (off - GUTTER_WIDTH_MINIMUM, width - off)),
sdiff_column2_offset = sdiff_half_width ? off : width;
}
if (show_c_function && output_style != OUTPUT_UNIFIED)
specify_style (OUTPUT_CONTEXT);
if (output_style != OUTPUT_CONTEXT && output_style != OUTPUT_UNIFIED)
context = 0;
else if (context == -1)
context = 3;
if (output_style == OUTPUT_IFDEF)
{
int i;
for (i = 0; i < sizeof (line_format) / sizeof (*line_format); i++)
if (!line_format[i])
line_format[i] = "%l\n";
if (!group_format[OLD])
group_format[OLD]
= group_format[UNCHANGED] ? group_format[UNCHANGED] : "%<";
if (!group_format[NEW])
group_format[NEW]
= group_format[UNCHANGED] ? group_format[UNCHANGED] : "%>";
if (!group_format[UNCHANGED])
group_format[UNCHANGED] = "%=";
if (!group_format[CHANGED])
group_format[CHANGED] = concat (group_format[OLD],
group_format[NEW], "");
}
no_diff_means_no_output =
(output_style == OUTPUT_IFDEF ?
(!*group_format[UNCHANGED]
|| (strcmp (group_format[UNCHANGED], "%=") == 0
&& !*line_format[UNCHANGED]))
: output_style == OUTPUT_SDIFF ? sdiff_skip_common_lines : 1);
switch_string = option_list (argv + 1, optind - 1);
val = compare_files (0, argv[optind], 0, argv[optind + 1], 0);
print_message_queue ();
check_stdout ();
exit (val);
return val;
}
static void
add_regexp (reglist, pattern)
struct regexp_list **reglist;
char const *pattern;
{
struct regexp_list *r;
char const *m;
r = (struct regexp_list *) xmalloc (sizeof (*r));
bzero (r, sizeof (*r));
r->buf.fastmap = xmalloc (256);
m = re_compile_pattern (pattern, strlen (pattern), &r->buf);
if (m != 0)
error ("%s: %s", pattern, m);
r->next = *reglist;
*reglist = r;
}
static void
try_help (reason)
char const *reason;
{
if (reason)
error ("%s", reason, 0);
error ("Try `%s --help' for more information.", program_name, 0);
exit (2);
}
static void
check_stdout ()
{
if (ferror (stdout) || fclose (stdout) != 0)
fatal ("write error");
}
static char const * const option_help[] = {
"-i  --ignore-case  Consider upper- and lower-case to be the same.",
"-w  --ignore-all-space  Ignore all white space.",
"-b  --ignore-space-change  Ignore changes in the amount of white space.",
"-B  --ignore-blank-lines  Ignore changes whose lines are all blank.",
"-I RE  --ignore-matching-lines=RE  Ignore changes whose lines all match RE.",
#if HAVE_SETMODE
"--binary  Read and write data in binary mode.",
#endif
"-a  --text  Treat all files as text.\n",
"-c  -C NUM  --context[=NUM]  Output NUM (default 2) lines of copied context.",
"-u  -U NUM  --unified[=NUM]  Output NUM (default 2) lines of unified context.",
"  -NUM  Use NUM context lines.",
"  -L LABEL  --label LABEL  Use LABEL instead of file name.",
"  -p  --show-c-function  Show which C function each change is in.",
"  -F RE  --show-function-line=RE  Show the most recent line matching RE.",
"-q  --brief  Output only whether files differ.",
"-e  --ed  Output an ed script.",
"-n  --rcs  Output an RCS format diff.",
"-y  --side-by-side  Output in two columns.",
"  -w NUM  --width=NUM  Output at most NUM (default 130) characters per line.",
"  --left-column  Output only the left column of common lines.",
"  --suppress-common-lines  Do not output common lines.",
"-DNAME  --ifdef=NAME  Output merged file to show `#ifdef NAME' diffs.",
"--GTYPE-group-format=GFMT  Similar, but format GTYPE input groups with GFMT.",
"--line-format=LFMT  Similar, but format all input lines with LFMT.",
"--LTYPE-line-format=LFMT  Similar, but format LTYPE input lines with LFMT.",
"  LTYPE is `old', `new', or `unchanged'.  GTYPE is LTYPE or `changed'.",
"  GFMT may contain:",
"    %<  lines from FILE1",
"    %>  lines from FILE2",
"    %=  lines common to FILE1 and FILE2",
"    %[-][WIDTH][.[PREC]]{doxX}LETTER  printf-style spec for LETTER",
"      LETTERs are as follows for new group, lower case for old group:",
"        F  first line number",
"        L  last line number",
"        N  number of lines = L-F+1",
"        E  F-1",
"        M  L+1",
"  LFMT may contain:",
"    %L  contents of line",
"    %l  contents of line, excluding any trailing newline",
"    %[-][WIDTH][.[PREC]]{doxX}n  printf-style spec for input line number",
"  Either GFMT or LFMT may contain:",
"    %%  %",
"    %c'C'  the single character C",
"    %c'\\OOO'  the character with octal code OOO\n",
"-l  --paginate  Pass the output through `pr' to paginate it.",
"-t  --expand-tabs  Expand tabs to spaces in output.",
"-T  --initial-tab  Make tabs line up by prepending a tab.\n",
"-r  --recursive  Recursively compare any subdirectories found.",
"-N  --new-file  Treat absent files as empty.",
"-P  --unidirectional-new-file  Treat absent first files as empty.",
"-s  --report-identical-files  Report when two files are the same.",
"-x PAT  --exclude=PAT  Exclude files that match PAT.",
"-X FILE  --exclude-from=FILE  Exclude files that match any pattern in FILE.",
"-S FILE  --starting-file=FILE  Start with FILE when comparing directories.\n",
"--horizon-lines=NUM  Keep NUM lines of the common prefix and suffix.",
"-d  --minimal  Try hard to find a smaller set of changes.",
"-H  --speed-large-files  Assume large files and many scattered small changes.\n",
"-v  --version  Output version info.",
"--help  Output this help.",
0
};
static void
usage ()
{
char const * const *p;
printf ("Usage: %s [OPTION]... FILE1 FILE2\n\n", program_name);
for (p = option_help;  *p;  p++)
printf ("  %s\n", *p);
printf ("\nIf FILE1 or FILE2 is `-', read standard input.\n");
}
static int
specify_format (var, value)
char **var;
char *value;
{
int err = *var ? strcmp (*var, value) : 0;
*var = value;
return err;
}
static void
specify_style (style)
enum output_style style;
{
if (output_style != OUTPUT_NORMAL
&& output_style != style)
error ("conflicting specifications of output style", 0, 0);
output_style = style;
}
static char const *
filetype (st)
struct stat const *st;
{
if (S_ISREG (st->st_mode))
{
if (st->st_size == 0)
return "regular empty file";
return "regular file";
}
if (S_ISDIR (st->st_mode)) return "directory";
#ifdef S_ISBLK
if (S_ISBLK (st->st_mode)) return "block special file";
#endif
#ifdef S_ISCHR
if (S_ISCHR (st->st_mode)) return "character special file";
#endif
#ifdef S_ISFIFO
if (S_ISFIFO (st->st_mode)) return "fifo";
#endif
#ifdef S_TYPEISMQ
if (S_TYPEISMQ (st)) return "message queue";
#endif
#ifdef S_TYPEISSEM
if (S_TYPEISSEM (st)) return "semaphore";
#endif
#ifdef S_TYPEISSHM
if (S_TYPEISSHM (st)) return "shared memory object";
#endif
#ifdef S_ISSOCK
if (S_ISSOCK (st->st_mode)) return "socket";
#endif
return "weird file";
}
static int
compare_files (dir0, name0, dir1, name1, depth)
char const *dir0, *dir1;
char const *name0, *name1;
int depth;
{
struct file_data inf[2];
register int i;
int val;
int same_files;
int failed = 0;
char *free0 = 0, *free1 = 0;
if (! ((name0 != 0 && name1 != 0)
|| (unidirectional_new_file_flag && name1 != 0)
|| entire_new_file_flag))
{
char const *name = name0 == 0 ? name1 : name0;
char const *dir = name0 == 0 ? dir1 : dir0;
message ("Only in %s: %s\n", dir, name);
return 1;
}
bzero (inf, sizeof (inf));
inf[0].desc = name0 == 0 ? -1 : -2;
inf[1].desc = name1 == 0 ? -1 : -2;
if (name0 == 0)
name0 = name1;
if (name1 == 0)
name1 = name0;
inf[0].name = dir0 == 0 ? name0 : (free0 = dir_file_pathname (dir0, name0));
inf[1].name = dir1 == 0 ? name1 : (free1 = dir_file_pathname (dir1, name1));
for (i = 0; i <= 1; i++)
{
if (inf[i].desc != -1)
{
int stat_result;
if (i && filename_cmp (inf[i].name, inf[0].name) == 0)
{
inf[i].stat = inf[0].stat;
stat_result = 0;
}
else if (strcmp (inf[i].name, "-") == 0)
{
inf[i].desc = STDIN_FILENO;
stat_result = fstat (STDIN_FILENO, &inf[i].stat);
if (stat_result == 0 && S_ISREG (inf[i].stat.st_mode))
{
off_t pos = lseek (STDIN_FILENO, (off_t) 0, SEEK_CUR);
if (pos == -1)
stat_result = -1;
else
{
if (pos <= inf[i].stat.st_size)
inf[i].stat.st_size -= pos;
else
inf[i].stat.st_size = 0;
time (&inf[i].stat.st_mtime);
}
}
}
else
stat_result = stat (inf[i].name, &inf[i].stat);
if (stat_result != 0)
{
perror_with_name (inf[i].name);
failed = 1;
}
else
{
inf[i].dir_p = S_ISDIR (inf[i].stat.st_mode) && inf[i].desc != 0;
if (inf[1 - i].desc == -1)
{
inf[1 - i].dir_p = inf[i].dir_p;
inf[1 - i].stat.st_mode = inf[i].stat.st_mode;
}
}
}
}
if (! failed && depth == 0 && inf[0].dir_p != inf[1].dir_p)
{
int fnm_arg = inf[0].dir_p;
int dir_arg = 1 - fnm_arg;
char const *fnm = inf[fnm_arg].name;
char const *dir = inf[dir_arg].name;
char const *p = filename_lastdirchar (fnm);
char const *filename = inf[dir_arg].name
= dir_file_pathname (dir, p ? p + 1 : fnm);
if (strcmp (fnm, "-") == 0)
fatal ("can't compare - to a directory");
if (stat (filename, &inf[dir_arg].stat) != 0)
{
perror_with_name (filename);
failed = 1;
}
else
inf[dir_arg].dir_p = S_ISDIR (inf[dir_arg].stat.st_mode);
}
if (failed)
{
val = 2;
}
else if ((same_files = inf[0].desc != -1 && inf[1].desc != -1
&& 0 < same_file (&inf[0].stat, &inf[1].stat))
&& no_diff_means_no_output)
{
val = 0;
}
else if (inf[0].dir_p & inf[1].dir_p)
{
if (output_style == OUTPUT_IFDEF)
fatal ("-D option not supported with directories");
if (depth > 0 && !recursive)
{
message ("Common subdirectories: %s and %s\n",
inf[0].name, inf[1].name);
val = 0;
}
else
{
val = diff_dirs (inf, compare_files, depth);
}
}
else if ((inf[0].dir_p | inf[1].dir_p)
|| (depth > 0
&& (! S_ISREG (inf[0].stat.st_mode)
|| ! S_ISREG (inf[1].stat.st_mode))))
{
if (inf[0].desc == -1 || inf[1].desc == -1)
{
if ((inf[0].dir_p | inf[1].dir_p)
&& recursive
&& (entire_new_file_flag
|| (unidirectional_new_file_flag && inf[0].desc == -1)))
val = diff_dirs (inf, compare_files, depth);
else
{
char const *dir = (inf[0].desc == -1) ? dir1 : dir0;
message ("Only in %s: %s\n", dir, name0);
val = 1;
}
}
else
{
message5 ("File %s is a %s while file %s is a %s\n",
inf[0].name, filetype (&inf[0].stat),
inf[1].name, filetype (&inf[1].stat));
val = 1;
}
}
else if ((no_details_flag & ~ignore_some_changes)
&& inf[0].stat.st_size != inf[1].stat.st_size
&& (inf[0].desc == -1 || S_ISREG (inf[0].stat.st_mode))
&& (inf[1].desc == -1 || S_ISREG (inf[1].stat.st_mode)))
{
message ("Files %s and %s differ\n", inf[0].name, inf[1].name);
val = 1;
}
else
{
if (inf[0].desc == -2)
if ((inf[0].desc = open (inf[0].name, O_RDONLY, 0)) < 0)
{
perror_with_name (inf[0].name);
failed = 1;
}
if (inf[1].desc == -2)
if (same_files)
inf[1].desc = inf[0].desc;
else if ((inf[1].desc = open (inf[1].name, O_RDONLY, 0)) < 0)
{
perror_with_name (inf[1].name);
failed = 1;
}
#if HAVE_SETMODE
if (binary_I_O)
for (i = 0; i <= 1; i++)
if (0 <= inf[i].desc)
setmode (inf[i].desc, O_BINARY);
#endif
val = failed ? 2 : diff_2_files (inf, depth);
if (inf[0].desc >= 0 && close (inf[0].desc) != 0)
{
perror_with_name (inf[0].name);
val = 2;
}
if (inf[1].desc >= 0 && inf[0].desc != inf[1].desc
&& close (inf[1].desc) != 0)
{
perror_with_name (inf[1].name);
val = 2;
}
}
if (val == 0 && !inf[0].dir_p)
{
if (print_file_same_flag)
message ("Files %s and %s are identical\n",
inf[0].name, inf[1].name);
}
else
fflush (stdout);
if (free0)
free (free0);
if (free1)
free (free1);
return val;
}