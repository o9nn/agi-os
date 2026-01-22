#include <errno.h>
#include <limits.h>
#include <locale.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#ifndef _WIN32
#include <unistd.h>
#else
#include <io.h>
#endif
#ifdef _MSC_VER
#define LINK_GRAMMAR_DLL_EXPORT 0
#endif
#include "parser-utilities.h"
#include "command-line.h"
#include "lg_readline.h"
#define DISPLAY_MAX 1024
static int batch_errors = 0;
static int verbosity = 0;
static char * debug = (char *)"";
static char * test = (char *)"";
static bool isatty_io;
static const char prompt[] = "linkparser> ";
static const char *use_prompt(int verbosity_level)
{
return (0 == verbosity_level)? "" : prompt;
}
static void set_default_parse_options(Parse_Options opts)
{
parse_options_set_max_parse_time(opts, 30);
parse_options_set_linkage_limit(opts, 1000);
parse_options_set_min_null_count(opts, 0);
parse_options_set_max_null_count(opts, 0);
parse_options_set_short_length(opts, 16);
parse_options_set_islands_ok(opts, false);
parse_options_set_display_morphology(opts, false);
parse_options_set_spell_guess(opts, 7);
}
typedef enum
{
UNGRAMMATICAL = '*',
PARSE_WITH_DISJUNCT_COST_GT_0 = ':',
NO_LABEL = ' '
} Label;
static char *fget_input_string(const char *uprompt, FILE *in, FILE *out,
bool tty, bool check_return)
{
static char *pline;
static char input_string[MAX_INPUT_LINE];
static bool input_pending = false;
if (input_pending)
{
input_pending = false;
return pline;
}
pline = input_string;
if (get_line(uprompt, &pline, MAX_INPUT_LINE, in, out, tty) != 1)
return NULL;
if (check_return)
{
if (('\0' == pline[0]) || ('\r' == pline[0]) || ('\n' == pline[0]))
return (char *)"\n";
if ((in == stdin) || ('!' == pline[0]))
input_pending = true;
return (char *)"x";
}
return pline;
}
static void process_linkage(Linkage linkage, Command_Options* copts)
{
char * string;
ConstituentDisplayStyle mode;
if (!linkage) return;
if (copts->display_bad)
{
string = linkage_print_pp_msgs(linkage);
fprintf(stdout, "%s\n", string);
linkage_free_pp_msgs(string);
}
if (copts->display_on)
{
#ifdef SIGWINCH
set_screen_width(copts);
#endif
string = linkage_print_diagram(linkage, copts->display_walls, copts->screen_width);
fprintf(stdout, "%s", string);
linkage_free_diagram(string);
}
if ((mode = copts->display_constituents))
{
string = linkage_print_constituent_tree(linkage, mode);
if (string != NULL)
{
fprintf(stdout, "%s\n", string);
linkage_free_constituent_tree_str(string);
}
else
{
copts->display_constituents = 0;
prt_error("Error: Can't generate constituents.\n"
"Constituent processing has been turned off.\n");
}
}
if (copts->display_links)
{
string = linkage_print_links_and_domains(linkage);
fprintf(stdout, "%s", string);
linkage_free_links_and_domains(string);
}
if (copts->display_disjuncts)
{
string = linkage_print_disjuncts(linkage);
fprintf(stdout, "%s\n", string);
linkage_free_disjuncts(string);
}
if (copts->display_postscript)
{
string = linkage_print_postscript(linkage,
copts->display_walls, copts->display_ps_header);
fprintf(stdout, "%s\n", string);
linkage_free_postscript(string);
}
}
static void print_parse_statistics(Sentence sent, Parse_Options opts,
Command_Options* copts)
{
if (sentence_num_linkages_found(sent) > 0)
{
if (sentence_num_linkages_found(sent) >
parse_options_get_linkage_limit(opts))
{
fprintf(stdout, "Found %d linkage%s (%d of %d random " \
"linkages had no P.P. violations)",
sentence_num_linkages_found(sent),
sentence_num_linkages_found(sent) == 1 ? "" : "s",
sentence_num_valid_linkages(sent),
sentence_num_linkages_post_processed(sent));
}
else
{
if ((sentence_num_valid_linkages(sent) > 0) || copts->display_bad)
{
fprintf(stdout, "Found %d linkage%s (%d had no P.P. violations)",
sentence_num_linkages_post_processed(sent),
sentence_num_linkages_post_processed(sent) == 1 ? "" : "s",
sentence_num_valid_linkages(sent));
}
}
if (sentence_null_count(sent) > 0)
{
fprintf(stdout, " at null count %d", sentence_null_count(sent));
}
fprintf(stdout, "\n");
}
}
static const char *test_enabled(const char *list, const char *feature)
{
const char *pos = strstr(list, feature);
if (pos == NULL) return NULL;
const char *check_end = pos + strlen(feature);
if ((pos == list) || (pos[-1] == ','))
{
if ((*check_end == ',') || (*check_end == ':')) return check_end;
if (*check_end == '\0') return ",";
}
return test_enabled(check_end, feature);
}
static int auto_next_linkage_test(const char *test_opt)
{
const char *auto_next_linkage_pos =
test_enabled(test_opt, "auto-next-linkage");
int max_display = 0;
if (auto_next_linkage_pos == NULL) return 0;
if (':' == auto_next_linkage_pos[0])
max_display = atoi(auto_next_linkage_pos + 1);
if (max_display != 0) return max_display;
return DISPLAY_MAX;
}
static const char *process_some_linkages(FILE *in, Sentence sent,
Command_Options* copts)
{
int i, num_to_query, num_to_display, num_displayed;
Linkage linkage;
Parse_Options opts = copts->popts;
int display_max = DISPLAY_MAX;
bool auto_next_linkage = false;
i = auto_next_linkage_test(test);
if (i != 0)
{
display_max = i;
auto_next_linkage = true;
}
if (verbosity > 0) print_parse_statistics(sent, opts, copts);
num_to_query = sentence_num_linkages_post_processed(sent);
if (!copts->display_bad)
{
num_to_display = MIN(sentence_num_valid_linkages(sent),
display_max);
}
else
{
num_to_display = MIN(num_to_query, display_max);
}
for (i=0, num_displayed=0; i<num_to_query; i++)
{
if ((sentence_num_violations(sent, i) > 0) &&
!copts->display_bad)
{
continue;
}
linkage = linkage_create(i, sent, opts);
if ((sentence_num_violations(sent, i) > 0) &&
!copts->display_bad)
{
continue;
}
if (!linkage)
{
if (verbosity > 0)
{
if (0 == i)
fprintf(stdout, "No linkages found.\n");
else
fprintf(stdout, "No more linkages.\n");
}
break;
}
if (verbosity > 0)
{
if ((sentence_num_valid_linkages(sent) == 1) &&
!copts->display_bad)
{
fprintf(stdout, "\tUnique linkage, ");
}
else if (copts->display_bad &&
(sentence_num_violations(sent, i) > 0))
{
fprintf(stdout, "\tLinkage %d (bad), ", num_displayed+1);
}
else
{
fprintf(stdout, "\tLinkage %d, ", num_displayed+1);
}
fprintf(stdout, "cost vector = (UNUSED=%d DIS=%5.2f LEN=%d)\n",
linkage_unused_word_cost(linkage),
linkage_disjunct_cost(linkage),
linkage_link_cost(linkage));
}
process_linkage(linkage, copts);
linkage_delete(linkage);
if (++num_displayed < num_to_display)
{
if (!auto_next_linkage)
{
if ((verbosity > 0) && (!copts->batch_mode) && isatty_io)
{
fprintf(stdout, "Press RETURN for the next linkage.\n");
fflush(stdout);
}
char *rc = fget_input_string(use_prompt(verbosity),  stdin, stdout,
isatty_io, true);
if ((NULL == rc) || (*rc != '\n')) return rc;
}
}
else
{
break;
}
}
return "x";
}
static int there_was_an_error(Label label, Sentence sent, Parse_Options opts)
{
if (sentence_num_valid_linkages(sent) > 0) {
if (label == UNGRAMMATICAL) {
batch_errors++;
return UNGRAMMATICAL;
}
if ((sentence_disjunct_cost(sent, 0) == 0.0F) &&
(label == PARSE_WITH_DISJUNCT_COST_GT_0)) {
batch_errors++;
return PARSE_WITH_DISJUNCT_COST_GT_0;
}
} else {
if (label != UNGRAMMATICAL) {
batch_errors++;
return UNGRAMMATICAL;
}
}
return 0;
}
static void batch_process_some_linkages(Label label,
Sentence sent,
Command_Options* copts)
{
Parse_Options opts = copts->popts;
if (there_was_an_error(label, sent, opts))
{
if (sentence_num_valid_linkages(sent) > 0) {
Linkage linkage = NULL;
int i;
for (i=0; i<sentence_num_linkages_post_processed(sent); i++)
{
if (0 == sentence_num_violations(sent, i))
{
linkage = linkage_create(i, sent, opts);
break;
}
}
process_linkage(linkage, copts);
linkage_delete(linkage);
}
fprintf(stdout, "+++++ error %d\n", batch_errors);
}
else
{
if (test_enabled(test, "batch-print-parse-statistics"))
{
print_parse_statistics(sent, opts, copts);
}
}
}
static int special_command(char *input_string, Command_Options* copts, Dictionary dict)
{
if (input_string[0] == COMMENT_CHAR) return 'c';
if (input_string[0] == '!')
return issue_special_command(input_string+1, copts, dict);
return 'n';
}
static Label strip_off_label(char * input_string)
{
Label c;
c = (Label) input_string[0];
switch(c) {
case UNGRAMMATICAL:
case PARSE_WITH_DISJUNCT_COST_GT_0:
input_string[0] = ' ';
return c;
case NO_LABEL:
default:
return NO_LABEL;
}
}
static int divert_stdio(FILE *from, FILE *to)
{
const int origfd = dup(fileno(from));
dup2(fileno(to), fileno(from));
return origfd;
}
#if 0
static void restore_stdio(FILE *from, int origfd)
{
dup2(fileno(from), origfd);
}
#endif
static const char *fbasename(const char *fpath)
{
const char *progf, *progb;
if ((NULL == fpath) || ('\0' == fpath[0])) return "(null)";
progf = strrchr(fpath, '/');
if (NULL == progf)
progb = strrchr(fpath, '\\');
else
progb = strchr(progf, '\\');
if (NULL != progb) return progb + 1;
if (NULL == progf) return fpath;
return progf + 1;
}
static Dictionary dictionary_setup(const char *language, bool quiet,
Parse_Options opts)
{
Dictionary dict;
int save_verbosity = parse_options_get_verbosity(opts);
if (quiet && (save_verbosity == 1))
parse_options_set_verbosity(opts, 0);
if (language && *language)
{
dict = dictionary_create_lang(language);
if (dict == NULL)
{
prt_error("Fatal error: Unable to open dictionary.\n");
return NULL;
}
}
else
{
dict = dictionary_create_default_lang();
if (dict == NULL)
{
prt_error("Fatal error: Unable to open default dictionary.\n");
return NULL;
}
}
parse_options_set_verbosity(opts, save_verbosity);
return dict;
}
static void print_usage(FILE *out, char *argv0, Command_Options *copts, int exit_value)
{
fprintf(out, "Usage: %s [language|dictionary location]\n"
"                   [-<special \"!\" command>]\n"
"                   [--version]\n", fbasename(argv0));
fprintf(out, "\nSpecial commands are:\n");
if (stdout != out) divert_stdio(stdout, out);
issue_special_command("var", copts, NULL);
if (out == stdout) print_url_info();
exit(exit_value);
}
int main(int argc, char * argv[])
{
FILE            *input_fh = stdin;
Dictionary      dict;
const char     *language = NULL;
int             num_linkages;
Label           label = NO_LABEL;
Command_Options *copts;
Parse_Options   opts;
bool batch_in_progress = false;
isatty_io = isatty(fileno(stdin)) && isatty(fileno(stdout));
argv = ms_windows_setup(argc);
if ((argc > 1) && (argv[1][0] != '-')) {
language = argv[1];
}
copts = command_options_create();
if (copts == NULL || copts->popts == NULL)
{
prt_error("Fatal error: unable to create parse options\n");
exit(-1);
}
opts = copts->popts;
const char * const debug_vars[] = { "verbosity", "debug", "test" };
const int debug_vars_min_len[] = { 1, 2, 2 };
unsigned long long argv_done = 0;
int quiet_start = 0;
for (int i = 1; i < argc; i++)
{
if (strcmp("--help", argv[i]) == 0)
{
print_usage(stdout, argv[0], copts, 0);
}
if (strcmp("--version", argv[i]) == 0)
{
printf("Version: %s\n", linkgrammar_get_version());
printf("%s\n", linkgrammar_get_configuration());
exit(0);
}
if ((strcmp("--quiet", argv[i]) == 0) ||
(strcmp("--silent", argv[i]) == 0))
{
quiet_start = i;
}
}
for (int i = 1; i < argc; i++)
{
if (i == quiet_start) continue;
if (argv[i][0] == '-')
{
char *var = strdup(argv[i] + ((argv[i][1] != '-') ? 1 : 2));
char *eqpos = strchr(var, '=');
if (eqpos != NULL)
{
*eqpos = '\0';
for (size_t j = 0; j < sizeof(debug_vars)/sizeof(debug_vars[0]); j++)
{
if (eqpos - var < debug_vars_min_len[j]) continue;
if (strncasecmp(debug_vars[j], var, eqpos - var) != 0) continue;
*eqpos = '=';
if (0 > issue_special_command(var, copts, NULL))
print_usage(stderr, argv[0], copts, -1);
if (i < (int)sizeof(argv_done) * CHAR_BIT)
argv_done |= (1LL<<(i-1));
else
prt_error("Warning: Too many arguments (%d).\n", i);
break;
}
}
free(var);
}
}
dict = dictionary_setup(language, quiet_start > 0, opts);
if (dict == NULL) exit(-1);
set_default_parse_options(opts);
const char *panic_max_cost_str =
linkgrammar_get_dict_define(dict, LG_PANIC_DISJUNCT_COST);
if (panic_max_cost_str != NULL)
{
const char *locale =  setlocale(LC_NUMERIC, "C");
char *err;
float panic_max_cost = strtof(panic_max_cost_str, &err);
setlocale(LC_NUMERIC, locale);
if ('\0' == *err)
{
copts->panic.max_cost = panic_max_cost;
}
else
{
prt_error("Warning: Unparsable "LG_PANIC_DISJUNCT_COST " \"%s\" "
"in the dictionary\n", panic_max_cost_str);
}
}
parse_options_set_disjunct_cost(opts,
linkgrammar_get_dict_max_disjunct_cost(dict));
save_default_opts(copts);
for (int i = 1; i < argc; i++)
{
if (i == quiet_start) continue;
if ((i < (int)sizeof(argv_done) * CHAR_BIT) &&
(argv_done & (1LL<<(i-1)))) continue;
if (argv[i][0] == '-')
{
const char *var = argv[i] + ((argv[i][1] != '-') ? 1 : 2);
if ((var[0] != '!') && (0 > issue_special_command(var, copts, NULL)))
print_usage(stderr, argv[0], copts, -1);
}
else if (i != 1)
{
prt_error("Fatal error: Unknown argument '%s'.\n", argv[i]);
print_usage(stderr, argv[0], copts, -1);
}
}
for (int i = 1; i < argc; i++)
{
if ((argv[i][0] == '-') && (argv[i][1] == '!'))
{
if (0 > issue_special_command(argv[i]+1, copts, dict))
print_usage(stderr, argv[0], copts, -1);
}
}
if ((parse_options_get_verbosity(opts)) > 0 && (quiet_start == 0))
{
prt_error("Info: Dictionary version %s, locale %s\n",
linkgrammar_get_dict_version(dict),
linkgrammar_get_dict_locale(dict));
prt_error("Info: Library version %s. Enter \"!help\" for help.\n",
linkgrammar_get_version());
}
put_opts_in_local_vars(copts);
initialize_screen_width(copts);
if (isatty_io)
{
find_history_filepath(dictionary_get_lang(dict), argv[0], "link-parser");
}
while (true)
{
char *input_string;
Sentence sent = NULL;
fflush(stderr);
verbosity = parse_options_get_verbosity(opts);
debug = parse_options_get_debug(opts);
test = parse_options_get_test(opts);
input_string = fget_input_string(use_prompt(verbosity), input_fh, stdout,
isatty_io, false);
if (NULL == input_string)
{
if (ferror(input_fh))
prt_error("Error: Read: %s\n", strerror(errno));
if (input_fh == stdin) break;
fclose (input_fh);
input_fh = stdin;
continue;
}
for (char *p = &input_string[strlen(input_string)-1];
(p > input_string) && strchr(WHITESPACE, *p); p--)
{
*p = '\0';
}
if (strspn(input_string, WHITESPACE) == strlen(input_string))
continue;
set_screen_width(copts);
int command = special_command(input_string, copts, dict);
if ('e' == command) break;
if ('c' == command) continue;
if (-1 == command) continue;
if ('k' == command)
{
dictionary_clear_cache(dict);
continue;
}
if ('f' == command)
{
char *command_end = &input_string[strcspn(input_string, WHITESPACE)];
char *filename = &command_end[strspn(command_end, WHITESPACE)];
if (filename[0] == '\0')
{
prt_error("Error: Missing file name argument\n");
continue;
}
char *eh_filename = expand_homedir(filename);
struct stat statbuf;
if ((0 == stat(eh_filename, &statbuf)) && statbuf.st_mode & S_IFDIR)
{
errno = EISDIR;
goto open_error;
}
input_fh = fopen(eh_filename, "r");
if (NULL == input_fh)
{
input_fh = stdin;
goto open_error;
}
free(eh_filename);
continue;
open_error:
prt_error("Error: Cannot open %s: %s\n", eh_filename, strerror(errno));
free(eh_filename);
continue;
}
if (!copts->batch_mode) batch_in_progress = false;
if ('\0' != test[0] && !test_enabled(test, "@"))
{
if (!batch_in_progress && !auto_next_linkage_test(test))
{
fflush(stdout);
prt_error("Warning: Tests enabled: %s\n", test);
if (copts->batch_mode) batch_in_progress = true;
}
}
if (copts->echo_on)
{
printf("%s\n", input_string);
}
if (copts->batch_mode || auto_next_linkage_test(test))
{
label = strip_off_label(input_string);
}
if (copts->display_bad)
parse_options_set_perform_pp_prune(opts, false);
else
parse_options_set_perform_pp_prune(opts, true);
sent = sentence_create(input_string, dict);
if (sentence_split(sent, opts) < 0)
{
sentence_delete(sent);
sent = NULL;
continue;
}
if (0 != copts->display_wordgraph)
{
const char *wg_display_flags = "";
switch (copts->display_wordgraph)
{
case 1:
break;
case 2:
wg_display_flags = "sl";
break;
case 3:
{
const char *s = test_enabled(test, "wg");
if ((NULL != s) && (':' == s[0])) wg_display_flags = s;
}
break;
default:
prt_error("Warning: wordgraph=%d: Unknown value, using 1\n",
copts->display_wordgraph);
copts->display_wordgraph = 1;
}
sentence_display_wordgraph(sent, wg_display_flags);
}
bool one_step_parse = !copts->batch_mode && copts->allow_null &&
test_enabled(test, "one-step-parse");
int max_null_count = one_step_parse ? sentence_length(sent) : 0;
parse_options_set_min_null_count(opts, 0);
parse_options_set_max_null_count(opts, max_null_count);
parse_options_reset_resources(opts);
num_linkages = sentence_parse(sent, opts);
if (num_linkages < 0)
{
sentence_delete(sent);
sent = NULL;
continue;
}
if (!(copts->panic_mode && parse_options_resources_exhausted(opts)))
{
#if 0
if (num_linkages == 0)
{
parse_options_set_disjunct_cost(opts, 4.5);
num_linkages = sentence_parse(sent, opts);
if (num_linkages < 0) continue;
}
#endif
if ((num_linkages == 0) && (!copts->batch_mode))
{
if (copts->display_bad)
{
num_linkages = sentence_num_linkages_found(sent);
}
}
if (!one_step_parse && num_linkages == 0)
{
if (!copts->batch_mode)
{
if (verbosity > 0)
fprintf(stdout, "No complete linkages found.\n");
if (copts->allow_null)
{
parse_options_set_min_null_count(opts, 1);
parse_options_set_max_null_count(opts, sentence_length(sent));
num_linkages = sentence_parse(sent, opts);
if (num_linkages < 0)
{
sentence_delete(sent);
sent = NULL;
continue;
}
}
}
}
}
if (verbosity > 0)
{
if (parse_options_timer_expired(opts))
fprintf(stdout, "Timer is expired!\n");
if (parse_options_memory_exhausted(opts))
fprintf(stdout, "Memory is exhausted!\n");
}
if (copts->panic_mode && parse_options_resources_exhausted(opts))
{
batch_errors++;
if (verbosity > 0)
{
fprintf(stdout, "Entering \"panic\" mode...\n");
}
setup_panic_parse_options(copts, sentence_length(sent));
(void)sentence_parse(sent, opts);
if (verbosity > 0)
{
if (parse_options_timer_expired(copts->popts))
fprintf(stdout, "Panic timer is expired!\n");
}
put_local_vars_in_opts(copts);
}
if (verbosity > 1) parse_options_print_total_time(opts);
#ifndef SIGWINCH
set_screen_width(copts);
#endif
const char *rc = "";
if (copts->batch_mode)
{
batch_process_some_linkages(label, sent, copts);
}
else
{
rc = process_some_linkages(input_fh, sent, copts);
}
fflush(stdout);
sentence_delete(sent);
sent = NULL;
if ((NULL == rc) && (input_fh == stdin)) break;
}
if (copts->batch_mode)
{
fprintf(stderr,
"%d error%s.\n", batch_errors, (batch_errors==1) ? "" : "s");
}
command_options_delete(copts);
dictionary_delete(dict);
printf ("Bye.\n");
return 0;
}