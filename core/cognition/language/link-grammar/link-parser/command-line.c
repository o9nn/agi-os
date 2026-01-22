#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <wctype.h>
#include "command-line.h"
#include "parser-utilities.h"
static struct
{
int verbosity;
char * debug;
char * test;
char * dialect;
int timeout;
int memory;
int linkage_limit;
int islands_ok;
int repeatable_rand;
int spell_guess;
int short_length;
int batch_mode;
int panic_mode;
int allow_null;
#if USE_SAT_SOLVER
int use_sat_solver;
#endif
int echo_on;
Cost_Model_type cost_model;
float max_cost;
int screen_width;
int display_on;
ConstituentDisplayStyle display_constituents;
int display_postscript;
int display_ps_header;
int display_bad;
int display_links;
int display_walls;
int display_disjuncts;
int display_morphology;
int display_wordgraph;
panic_options panic;
} local, saved_defaults;
static const char *value_type[] =
{
"(integer) ", "(Boolean) ", "(float) ", "(string) ", "(command) ", ""
};
static int clear_cmd(const Switch*, int);
static int exit_cmd(const Switch*, int);
static int file_cmd(const Switch*, int);
static int help_cmd(const Switch*, int);
static int info_cmd(const Switch*, int);
static int panic_variables_cmd(const Switch*, int);
static int variables_cmd(const Switch*, int);
Switch default_switches[] =
{
{"bad",        Bool, "Display of bad linkages",         &local.display_bad},
{"batch",      Bool, "Batch mode",                      &local.batch_mode},
{"constituents", Int,  "Generate constituent output",   &local.display_constituents},
{"cost-model", Int,  UNDOC "Cost model used for ranking", &local.cost_model},
{"cost-max",   Float, "Largest cost to be considered",  &local.max_cost},
{"debug",      String, "Comma-separated function names to debug", &local.debug},
{"dialect",    String, "Comma-separated dialects",      &local.dialect},
{"disjuncts",  Bool, "Display of disjuncts used",       &local.display_disjuncts},
{"echo",       Bool, "Echoing of input sentence",       &local.echo_on},
{"graphics",   Bool, "Graphical display of linkage",    &local.display_on},
{"islands-ok", Bool, "Use of null-linked islands",      &local.islands_ok},
{"limit",      Int,  "The maximum linkages processed",  &local.linkage_limit},
{"links",      Bool, "Display of complete link data",   &local.display_links},
{"memory",     Int,  UNDOC "Max memory allowed",        &local.memory},
{"morphology", Bool, "Display word morphology",         &local.display_morphology},
{"null",       Bool, "Allow null links",                &local.allow_null},
{"panic",      Bool, "Use of \"panic mode\"",           &local.panic_mode},
{"panic_short", Int, "Max length of all links",     &local.panic.short_length},
{"panic_cost-max", Float, "Largest cost to be considered", &local.panic.max_cost},
{"panic_limit",     Int, "The maximum linkages processed", &local.panic.linkage_limit},
{"panic_max-null-count", Int, "Max number of null links allowed", &local.panic.max_null_count},
{"panic_spell",     Int, "Up to this many spell-guesses per unknown word", &local.panic.spell_guess},
{"panic_timeout",   Int, "Abort panic parsing after this many seconds", &local.panic.timeout},
{"postscript", Bool, "Generate postscript output",      &local.display_postscript},
{"ps-header",  Bool, "Generate postscript header",      &local.display_ps_header},
{"rand",       Bool, "Use repeatable random numbers",   &local.repeatable_rand},
{"short",      Int,  "Max length of short links",       &local.short_length},
#if defined HAVE_HUNSPELL || defined HAVE_ASPELL
{"spell",      Int, "Up to this many spell-guesses per unknown word", &local.spell_guess},
#endif
{"test",       String, "Comma-separated test features", &local.test},
{"timeout",    Int,  "Abort parsing after this many seconds", &local.timeout},
#ifdef USE_SAT_SOLVER
{"use-sat",    Bool, "Use Boolean SAT-based parser",    &local.use_sat_solver},
#endif
{"verbosity",  Int,  "Level of detail in output",       &local.verbosity},
{"walls",      Bool, "Display wall words",              &local.display_walls},
{"width",      Int,  "The width of the display",        &local.screen_width},
{"wordgraph",  Int,  "Display sentence word-graph",     &local.display_wordgraph},
{"clear",      Cmd,  "Clear the AtomSpace cache",              clear_cmd},
{"exit",       Cmd,  "Exit the program",                       exit_cmd},
{"file",       Cmd,  "Read input from the specified filename", file_cmd},
{"help",       Cmd,  "List the commands and what they do",     help_cmd},
{"panic_variables", Cmd,  "List panic mode setup",  panic_variables_cmd},
{"quit",       Cmd,  UNDOC "Exit the program",                 exit_cmd},
{"variables",  Cmd,  "List user-settable variables and their functions", variables_cmd},
{"!",          Cmd,  UNDOC "Print information on dictionary words", info_cmd},
{NULL,         Cmd,  NULL,                                     NULL}
};
void save_default_opts(Command_Options *copts)
{
put_opts_in_local_vars(copts);
saved_defaults = local;
saved_defaults.test = (char *)"";
saved_defaults.debug = (char *)"";
saved_defaults.dialect = (char *)"";
saved_defaults.verbosity = 1;
}
static void restore_default_local_vars(void)
{
local = saved_defaults;
}
int get_verbosity(void)
{
return local.verbosity;
}
static void clean_up_string(char * s)
{
char * x, * y;
wchar_t p;
size_t len, w;
mbstate_t state;
memset (&state, 0, sizeof(mbstate_t));
len = strlen(s);
y = x = s;
while(*x != '\0')
{
w = mbrtowc(&p, x, len, &state);
if (0 == w) break;
if (0 > (ssize_t)w)
{
prt_error("Error: Unable to process UTF8 command input string.\n");
break;
}
len -= w;
if (!iswspace(p)) {
while(w) { *y = *x; x++; y++; w--; }
} else {
x += w;
}
}
*y = '\0';
}
static bool is_numerical_rhs(char *s)
{
wchar_t p;
size_t len, w;
mbstate_t state;
memset (&state, 0, sizeof(mbstate_t));
len = strlen(s);
if (*s=='+' || *s == '-') s++;
if (*s == '\0') return false;
for (; *s != '\0'; s+=w)
{
w = mbrtowc(&p, s, len, &state);
if (0 == w) break;
if (0 > (ssize_t)w)
{
prt_error("Error: Unable to process UTF8 command input string.\n");
break;
}
len -= w;
if (!iswdigit(p)) return false;
}
return true;
}
static int ival(Switch s)
{
return *((int *) s.ptr);
}
static void setival(Switch s, int val)
{
*((int *) s.ptr) = val;
}
static const char *switch_value_description(const Switch *as)
{
if (Bool == as->param_type)
return ival(*as) ? " (On)" : " (Off)";
if (Int == as->param_type)
return (-1 == ival(*as)) ? " (Unlimited)" : "";
return "";
}
static const char *switch_value_string(const Switch *as)
{
static char buf[128];
switch (as->param_type)
{
case Float:
snprintf(buf, sizeof(buf), "%.3f", *((float *)as->ptr));
break;
case Bool:
case Int:
snprintf(buf, sizeof(buf), "%d", ival(*as));
break;
case String:
#if 0
if ((NULL == *(char **)as->ptr) || ('\0' == **(char **)as->ptr))
strcpy(buf, "(not set)");
else
#endif
snprintf(buf, sizeof(buf), "%s", *(char **)as->ptr);
break;
case Cmd:
buf[0] = '\0';
break;
default:
snprintf(buf, sizeof(buf), "Unknown type %d\n", (int)as->param_type);
}
return buf;
}
#define HELPFILE_BASE "command-help-"
#define HELPFILE_EXT ".txt"
#define HELPFILE_LANG_TEMPLATE "LL"
#define HELPFILE_LANG_TEMPLATE_SIZE (sizeof(HELPFILE_LANG_TEMPLATE)-1)
#define HELPFILE_TEMPLATE_SIZE \
(sizeof(HELPFILE_BASE HELPFILE_EXT)+HELPFILE_LANG_TEMPLATE_SIZE)
#define DEFAULT_HELP_LANG "en"
static FILE *help_file;
static void close_help_file(void)
{
fclose(help_file);
}
static const char *get_next_locale(void)
{
enum state
{Initial_s, Language_s, Next_language_s, Default_language_s, Final_s};
static int state = Initial_s;
static char *language;
char *lc_all;
const char *lang = NULL;
while (1)
{
switch (state)
{
case Initial_s:
lc_all = getenv("LC_ALL");
if ((NULL != lc_all) && (0 == strcmp(lc_all, "C")))
{
state = Default_language_s;
continue;
}
if ((NULL == lc_all) || ('\0' == *lc_all))
{
lang = getenv("LANG");
if ((NULL != lang) && (0 == strcmp(lang, "C")))
{
state = Default_language_s;
continue;
}
}
state = Language_s;
continue;
case Language_s:
language = getenv("LANGUAGE");
if ((language == NULL) || ('\0' == *language))
{
language = NULL;
state = Default_language_s;
if ((NULL != lang) && ('\0' != *lang))
{
return lang;
}
continue;
}
state = Next_language_s;
language = strdup(language);
return strtok(language, ":");
break;
case Next_language_s:
{
char *next_language = strtok(NULL, ":");
if (NULL == next_language)
{
state = Default_language_s;
continue;
}
return next_language;
}
break;
case Default_language_s:
state = Final_s;
return DEFAULT_HELP_LANG;
case Final_s:
free(language);
state = Initial_s;
break;
}
break;
}
return NULL;
}
static FILE *open_help_file(int verbosity)
{
char *help_filename;
if (NULL != help_file)
{
rewind(help_file);
return help_file;
}
atexit(close_help_file);
help_filename = malloc(HELPFILE_TEMPLATE_SIZE);
strcpy(help_filename, HELPFILE_BASE);
char *ll_pos = &help_filename[strlen(help_filename)];
strcpy(ll_pos, HELPFILE_LANG_TEMPLATE  HELPFILE_EXT);
const char *ll;
while ((ll = get_next_locale()))
{
if (NULL != help_file)
continue;
memcpy(ll_pos, ll, HELPFILE_LANG_TEMPLATE_SIZE);
help_file = linkgrammar_open_data_file(help_filename);
}
if ((NULL == help_file) && (verbosity == D_USER_FILES))
{
prt_error("Error: Cannot open help file '%s': %s\n",
help_filename, strerror(errno));
}
free(help_filename);
return help_file;
}
void display_1line_help(const Switch *sp, bool is_completion)
{
int undoc = !!(UNDOC[0] == sp->description[0]);
int vtw = 0;
int vnw = 0;
bool display_eq = is_completion && (Cmd != sp->param_type);
const char *value = "";
if (is_completion)
{
vtw = 10;
vnw = 18;
if (Cmd != sp->param_type)
value = switch_value_string(sp);
}
int n;
n = printf("%s%s%s", sp->string, display_eq ? "=" : " ", value);
if (is_completion) printf("%*s", MAX(0, vnw - n), "");
printf("%*s- %s\n", vtw, value_type[sp->param_type], sp->description+undoc);
}
static void display_help(const Switch *sp, Command_Options *copts)
{
char line[MAX_INPUT_LINE];
display_1line_help(sp, false);
if (Cmd != sp->param_type)
{
printf("Current value: %s\n", switch_value_string(sp));
restore_default_local_vars();
printf("Default value: %s\n", switch_value_string(sp));
put_opts_in_local_vars(copts);
}
FILE *hf = open_help_file(local.verbosity);
if (NULL == hf)
{
prt_error("Warning: Help file not found\n");
return;
}
bool help_found = false;
while (!help_found && (NULL != fgets(line, sizeof(line), hf)))
{
if ('[' != line[0]) continue;
{
#define CMDTAG_SEP ", \t]"
char *t = strtok(line+1, CMDTAG_SEP);
if (NULL == t) continue;
do {
if (0 == strcasecmp(t, sp->string))
{
help_found = true;
break;
}
t = strtok(NULL, CMDTAG_SEP);
} while (NULL != t);
}
}
if (ferror(hf))
{
prt_error("Error: Reading help file: %s\n", strerror(errno));
return;
}
if (feof(hf))
{
if (local.verbosity == D_USER_FILES)
prt_error("Error: Cannot find command \"%s\" in help file\n",
sp->string);
}
else
{
help_found = false;
bool issue_blank_line = false;
while (NULL != fgets(line, sizeof(line), hf))
{
const size_t len = strlen(line);
if ((MAX_INPUT_LINE == len-1) && '\n' != line[MAX_INPUT_LINE-2])
{
prt_error("Warning: Help-file text line too long at offset %ld\n",
ftell(hf));
}
if (COMMENT_CHAR == line[0]) continue;
if ('[' == line[0]) break;
if (strspn(line, WHITESPACE) == len)
{
issue_blank_line = true;
continue;
}
if (!help_found)
{
printf("\n");
help_found = true;
}
if (issue_blank_line)
{
issue_blank_line = false;
printf("\n");
}
printf("%s", line);
}
if (!help_found)
prt_error("Info: No help text found for command \"%s\"\n", sp->string);
}
}
#define URL_LINE "%-20s %s\n"
void print_url_info(void)
{
printf("\n");
#ifdef OVERVIEW
printf(URL_LINE, "Overview:", OVERVIEW);
#endif
#ifdef PACKAGE_URL
printf(URL_LINE, "Home page:", PACKAGE_URL);
printf(URL_LINE, "Documentation:", PACKAGE_URL"/dict/");
#endif
#ifdef DISCUSSION_GROUP
printf(URL_LINE, "Discussion group:", DISCUSSION_GROUP);
#endif
#ifdef PACKAGE_BUGREPORT
printf(URL_LINE, "Report bugs to:" , PACKAGE_BUGREPORT"/issues");
#endif
}
static int help_cmd(const Switch *uc, int n)
{
printf("Special commands always begin with \"!\".  Command and variable names\n");
printf("can be abbreviated.  Here is a list of the commands:\n\n");
printf(" !help command    Show a detailed help for the given command.\n");
for (int i = 0; uc[i].string != NULL; i++)
{
if (Cmd != uc[i].param_type) continue;
if (UNDOC[0] == uc[i].description[0]) continue;
printf(" !%-15s ", uc[i].string);
printf("%s.\n", uc[i].description);
}
printf("\n");
printf(" !!<string>       Print all the dictionary words that match <string>.\n");
printf("                  A wildcard * may be used to find multiple matches.\n");
printf("                  Issue \"!help !\" for more details.\n");
printf("\n");
printf(" !<var>           Toggle the specified Boolean variable.\n");
printf(" !<var>=<val>     Assign that value to that variable.\n");
print_url_info();
return 'c';
}
void setup_panic_parse_options(Command_Options *copts, int sentence_length)
{
Parse_Options opts = copts->popts;
parse_options_reset_resources(opts);
parse_options_set_max_parse_time(opts, local.panic.timeout);
parse_options_set_disjunct_cost(opts,
MAX(local.max_cost, local.panic.max_cost));
if (copts->allow_null)
{
parse_options_set_max_null_count(opts,
MIN(sentence_length, local.panic.max_null_count));
}
parse_options_set_short_length(opts,
MIN(local.short_length, local.panic.short_length));
parse_options_set_all_short_connectors(opts, 1);
parse_options_set_spell_guess(opts,
MIN(local.spell_guess, local.panic.spell_guess));
if (local.verbosity > 1)
{
int n = fprintf(stdout, "Panic mode setup: ");
fprintf(stdout,
"!cost-max=%.2f !limit=%d !timeout=%d " "!spell=%d !short=%d\n"
"%*s(all_short=%d min_null_count=%d max_null_count=%d)\n",
parse_options_get_disjunct_cost(opts),
parse_options_get_linkage_limit(opts),
parse_options_get_max_parse_time(opts),
parse_options_get_spell_guess(opts),
parse_options_get_short_length(opts),
n, "",
parse_options_get_all_short_connectors(opts),
parse_options_get_min_null_count(opts),
parse_options_get_max_null_count(opts));
}
}
static int panic_variables_cmd(const Switch *uc, int n)
{
printf(" Variable             Controls                                      Value\n");
printf(" --------             --------                                      -----\n");
for (int i = 0; uc[i].string != NULL; i++)
{
if (Cmd == uc[i].param_type) continue;
if (strncasecmp("panic_", uc[i].string, 6) != 0) continue;
if (UNDOC[0] == uc[i].description[0]) continue;
printf(" %-21s", uc[i].string);
printf("%-*s", FIELD_WIDTH(uc[i].description, 46), uc[i].description);
printf("%5s", switch_value_string(&uc[i]));
printf("%s\n", switch_value_description(&uc[i]));
}
printf("\n");
printf("Set a variable as so: \"!panic_timeout=10\".\n");
printf("Get detailed help on a variable with: \"!help var\".\n");
return 'c';
}
static int variables_cmd(const Switch *uc, int n)
{
printf(" Variable     Controls                                          Value\n");
printf(" --------     --------                                          -----\n");
for (int i = 0; uc[i].string != NULL; i++)
{
if (Cmd == uc[i].param_type) continue;
if (strncasecmp("panic_", uc[i].string, 6) == 0) continue;
if (UNDOC[0] == uc[i].description[0]) continue;
printf(" %-13s", uc[i].string);
printf("%-*s", FIELD_WIDTH(uc[i].description, 46), uc[i].description);
printf("%5s", switch_value_string(&uc[i]));
printf("%s\n", switch_value_description(&uc[i]));
}
printf("\n");
printf("Toggle a Boolean variable as so: \"!batch\"; ");
printf("Set a variable as so: \"!width=100\".\n");
printf("Get detailed help on a variable with: \"!help var\".\n");
return 'c';
}
static int exit_cmd(const Switch *uc, int n)
{
return 'e';
}
static int clear_cmd(const Switch *uc, int n)
{
return 'k';
}
static int file_cmd(const Switch *uc, int n)
{
return 'f';
}
static int info_cmd(const Switch *uc, int n)
{
return 'c';
}
const char helpmsg[] = "Type \"!help\" or \"!variables\".";
static int handle_help_command(const Switch *as, char *line,
Command_Options *copts)
{
char *dupline = strdup(line);
int count, j;
int rc = 0;
char *s = strtok(dupline, WHITESPACE);
if ((s != NULL) && strncasecmp(s, "help", strlen(s)) == 0)
{
s = strtok(NULL, WHITESPACE);
if (s != NULL)
{
j = -1;
count = 0;
for (int i = 0; as[i].string != NULL; i++)
{
if (strncasecmp(s, as[i].string, strlen(s)) == 0)
{
j = i;
if (strlen(as[i].string) == strlen(s))
{
count = 1;
break;
}
count++;
}
}
if (count == 1)
{
display_help(&as[j], copts);
rc = 'c';
}
else
{
rc = -1;
if (count > 1)
prt_error("Error: Ambiguous command: \"%s\".  %s\n", s, helpmsg);
else
prt_error("Error: Undefined command: \"%s\".  %s\n", s, helpmsg);
}
}
}
free(dupline);
return rc;
}
static int x_issue_special_command(char * line, Command_Options *copts, Dictionary dict)
{
char *s, *x, *y;
int count, j;
const Switch *as = default_switches;
line = &line[strspn(line, WHITESPACE)];
if (NULL != dict)
{
int rc = handle_help_command(as, line, copts);
if (rc != 0) return rc;
}
s = line;
if (s[0] == '!')
{
Parse_Options opts = copts->popts;
char *out;
out = dict_display_word_info(dict, s+1, opts);
if (NULL != out)
{
printf("%s\n", out);
free(out);
out = dict_display_word_expr(dict, s+1, opts);
if (NULL != out)
{
printf("%s", out);
free(out);
}
else
{
prt_error("Error: '%s': Internal Error: Missing expression.\n", s+1);
}
}
else
{
printf("Token \"%s\" matches nothing in the dictionary.\n", s+1);
}
return 'c';
}
if (strchr(s, '=') == NULL)
{
j = -1;
count = 0;
for (int i = 0; as[i].string != NULL; i++)
{
if (((Bool == as[i].param_type) || (Cmd == as[i].param_type)) &&
strncasecmp(s, as[i].string, strcspn(s, WHITESPACE)) == 0)
{
j = i;
if (strlen(as[i].string) == strcspn(s, WHITESPACE))
{
count = 1;
break;
}
if (UNDOC[0] == as[i].description[0]) continue;
count++;
}
}
if (count > 1)
{
prt_error("Error: Ambiguous command \"%s\".  %s\n", s, helpmsg);
return -1;
}
if (count == 1)
{
if (Bool == as[j].param_type)
{
size_t junk = strcspn(s, WHITESPACE);
if (junk != strlen(s))
{
prt_error("Error: Junk after a boolean variable: \"%s\".  %s\n",
&s[junk], helpmsg);
return -1;
}
setival(as[j], (0 == ival(as[j])));
int undoc = !!(UNDOC[0] == as[j].description[0]);
printf("%s turned %s.\n",
as[j].description+undoc, (ival(as[j]))? "on" : "off");
return 'c';
}
return ((int (*)(const Switch*, int)) (as[j].ptr))(as, j);
}
}
clean_up_string(line);
for (x=s; (*x != '=') && (*x != '\0'); x++)
;
if (*x == '=')
{
*x = '\0';
y = x+1;
x = s;
j = -1;
count = 0;
for (int i = 0; as[i].string != NULL; i++)
{
if (Cmd == as[i].param_type) continue;
if (strncasecmp(x, as[i].string, strlen(x)) == 0)
{
j = i;
if (strlen(as[i].string) == strlen(x))
{
count = 1;
break;
}
if (UNDOC[0] == as[i].description[0]) continue;
count ++;
}
}
if (j<0)
{
prt_error("Error: There is no user variable called \"%s\".\n", x);
return -1;
}
if (count > 1)
{
prt_error("Error: Ambiguous variable \"%s\".  %s\n", x, helpmsg);
return -1;
}
if ((as[j].param_type == Int) || (as[j].param_type == Bool))
{
int val = -1;
if (is_numerical_rhs(y)) val = atoi(y);
if ((0 == strcasecmp(y, "true")) || (0 == strcasecmp(y, "t"))) val = 1;
if ((0 == strcasecmp(y, "false")) || (0 == strcasecmp(y, "f"))) val = 0;
if ((val < 0) ||
((as[j].param_type == Bool) && (val != 0) && (val != 1)))
{
prt_error("Error: Invalid value \"%s\" for variable \"%s\". %s\n",
y, as[j].string, helpmsg);
return -1;
}
setival(as[j], val);
printf("%s set to %d\n", as[j].string, val);
return 'c';
}
else
if (as[j].param_type == Float)
{
char *err;
float val = strtof(y, &err);
if (('\0' == *y) || ('\0' != *err))
{
prt_error("Error: Invalid value \"%s\" for variable \"%s\". %s\n",
y, as[j].string, helpmsg);
return -1;
}
*((float *) as[j].ptr) = val;
printf("%s set to %5.3f\n", as[j].string, val);
return 'c';
}
else
if (as[j].param_type == String)
{
*((char **) as[j].ptr) = y;
printf("%s set to %s\n", (char *)as[j].string, y);
return 'c';
}
else
{
prt_error("Error: Internal error: Unknown variable type %d\n",
(int)as[j].param_type);
return -1;
}
}
j = -1;
count = 0;
for (int i = 0; as[i].string != NULL; i++)
{
if ((Bool != as[i].param_type) && (Cmd != as[i].param_type) &&
strncasecmp(s, as[i].string, strlen(s)) == 0)
{
if ((UNDOC[0] == as[i].description[0]) &&
(strlen(as[i].string) != strlen(s))) continue;
j = i;
count++;
}
}
if (0 < count)
{
prt_error("Error: Variable \"%s\" requires a value.  Try \"!help %s\".\n",
as[j].string, as[j].string);
return -1;
}
prt_error("Error: I can't interpret \"%s\" as a command.  Try \"!help\" or \"!variables\".\n", line);
return -1;
}
void put_opts_in_local_vars(Command_Options* copts)
{
Parse_Options opts = copts->popts;
local.verbosity = parse_options_get_verbosity(opts);
local.debug = parse_options_get_debug(opts);
local.dialect = parse_options_get_dialect(opts);
local.test = parse_options_get_test(opts);
local.timeout = parse_options_get_max_parse_time(opts);;
local.memory = parse_options_get_max_memory(opts);;
local.linkage_limit = parse_options_get_linkage_limit(opts);
local.islands_ok = parse_options_get_islands_ok(opts);
local.repeatable_rand = parse_options_get_repeatable_rand(opts);
local.spell_guess = parse_options_get_spell_guess(opts);
local.short_length = parse_options_get_short_length(opts);
local.cost_model = parse_options_get_cost_model_type(opts);
local.max_cost = parse_options_get_disjunct_cost(opts);
#if USE_SAT_SOLVER
local.use_sat_solver = parse_options_get_use_sat_parser(opts);
#endif
local.screen_width = (int)copts->screen_width;
local.echo_on = copts->echo_on;
local.batch_mode = copts->batch_mode;
local.panic_mode = copts->panic_mode;
local.allow_null = copts->allow_null;
local.display_on = copts->display_on;
local.display_walls = copts->display_walls;
local.display_postscript = copts->display_postscript;
local.display_ps_header = copts->display_ps_header;
local.display_constituents = copts->display_constituents;
local.display_wordgraph = copts->display_wordgraph;
local.display_bad = copts->display_bad;
local.display_disjuncts = copts->display_disjuncts;
local.display_links = copts->display_links;
local.display_morphology = parse_options_get_display_morphology(opts);
local.panic = copts->panic;
}
void put_local_vars_in_opts(Command_Options* copts)
{
Parse_Options opts = copts->popts;
parse_options_set_verbosity(opts, local.verbosity);
parse_options_set_debug(opts, local.debug);
parse_options_set_test(opts, local.test);
parse_options_set_dialect(opts, local.dialect);
parse_options_set_max_parse_time(opts, local.timeout);
parse_options_set_max_memory(opts, local.memory);
parse_options_set_linkage_limit(opts, local.linkage_limit);
parse_options_set_islands_ok(opts, local.islands_ok);
parse_options_set_repeatable_rand(opts, local.repeatable_rand);
parse_options_set_spell_guess(opts, local.spell_guess);
parse_options_set_short_length(opts, local.short_length);
parse_options_set_cost_model_type(opts, local.cost_model);
parse_options_set_disjunct_cost(opts, local.max_cost);
#if USE_SAT_SOLVER
parse_options_set_use_sat_parser(opts, local.use_sat_solver);
#endif
parse_options_set_display_morphology(opts, local.display_morphology);
copts->screen_width = (size_t)local.screen_width;
copts->echo_on = local.echo_on;
copts->batch_mode = local.batch_mode;
copts->panic_mode = local.panic_mode;
copts->allow_null = local.allow_null;
copts->display_on = local.display_on;
copts->display_walls = local.display_walls;
copts->display_postscript = local.display_postscript;
copts->display_ps_header = local.display_ps_header;
copts->display_constituents = local.display_constituents;
copts->display_wordgraph = local.display_wordgraph;
copts->display_bad = local.display_bad;
copts->display_disjuncts = local.display_disjuncts;
copts->display_links = local.display_links;
copts->panic = local.panic;
}
int issue_special_command(const char * line, Command_Options* opts, Dictionary dict)
{
int rc;
put_opts_in_local_vars(opts);
char *cline = strdup(line);
rc = x_issue_special_command(cline, opts, dict);
put_local_vars_in_opts(opts);
put_opts_in_local_vars(opts);
free(cline);
return rc;
}
Command_Options* command_options_create(void)
{
Command_Options* co = malloc(sizeof (Command_Options));
co->popts = parse_options_create();
co->screen_width = 16381;
co->allow_null = true;
co->batch_mode = false;
co->echo_on = false;
co->panic_mode = true;
co->display_on = true;
co->display_walls = false;
co->display_postscript = false;
co->display_ps_header = false;
co->display_constituents = NO_DISPLAY;
co->display_bad = false;
co->display_disjuncts = false;
co->display_links = false;
co->display_wordgraph = 0;
co->panic.max_cost = 4.0f;
co->panic.linkage_limit = 1000;
co->panic.max_null_count = 10;
co->panic.short_length = 12;
co->panic.spell_guess = 0;
co->panic.timeout = 30;
return co;
}
void command_options_delete(Command_Options* co)
{
parse_options_delete(co->popts);
free(co);
}