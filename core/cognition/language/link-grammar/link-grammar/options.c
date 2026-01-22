#include <string.h>
#include "api-structures.h"
#include "connectors.h"
#include "dict-common/dict-common.h"
#include "parse/parse.h"
#include "resources.h"
unsigned int global_rand_state = 0;
int verbosity = D_USER_BASIC;
char * debug = (char *)"";
char * test = (char *)"";
Parse_Options parse_options_create(void)
{
Parse_Options po;
init_memusage();
po = (Parse_Options) malloc(sizeof(struct Parse_Options_s));
verbosity = po->verbosity = D_USER_BASIC;
debug = po->debug = (char *)"";
test = po->test = (char *)"";
po->max_disjuncts = UNINITIALIZED_MAX_DISJUNCTS;
po->disjunct_cost = UNINITIALIZED_MAX_DISJUNCT_COST;
po->min_null_count = 0;
po->max_null_count = 0;
po->islands_ok = false;
#if USE_SAT_SOLVER
po->use_sat_solver = false;
#endif
po->linkage_limit = 100;
po->use_spell_guess = 0;
po->cost_model.compare_fn = &VDAL_compare_linkages;
po->cost_model.type = VDAL;
po->short_length = 16;
po->all_short = false;
po->perform_pp_prune = true;
po->twopass_length = 30;
po->repeatable_rand = true;
po->resources = resources_create();
po->display_morphology = true;
po->dialect = (dialect_info){ .conf = strdup("") };
return po;
}
static void free_dialect_info(dialect_info *);
int parse_options_delete(Parse_Options opts)
{
resources_delete(opts->resources);
free_dialect_info(&opts->dialect);
free(opts);
return 0;
}
void parse_options_set_cost_model_type(Parse_Options opts, Cost_Model_type cm)
{
switch(cm) {
case VDAL:
opts->cost_model.type = VDAL;
opts->cost_model.compare_fn = &VDAL_compare_linkages;
break;
default:
prt_error("Error: Illegal cost model: %d\n", (int)cm);
}
}
Cost_Model_type parse_options_get_cost_model_type(Parse_Options opts)
{
return opts->cost_model.type;
}
void parse_options_set_perform_pp_prune(Parse_Options opts, bool dummy)
{
opts->perform_pp_prune = dummy;
}
bool parse_options_get_perform_pp_prune(Parse_Options opts) {
return opts->perform_pp_prune;
}
void parse_options_set_verbosity(Parse_Options opts, int dummy)
{
opts->verbosity = dummy;
verbosity = opts->verbosity;
}
int parse_options_get_verbosity(Parse_Options opts) {
return opts->verbosity;
}
void parse_options_set_debug(Parse_Options opts, const char * dummy)
{
static char buff[256];
size_t len = strlen(dummy);
if (0 == strcmp(dummy, opts->debug)) return;
if (0 == len)
{
buff[0] = '\0';
}
else
{
buff[0] = ',';
strncpy(buff+1, dummy, sizeof(buff)-2);
if (len < sizeof(buff)-2)
{
buff[len+1] = ',';
buff[len+2] = '\0';
}
else
{
buff[sizeof(buff)-1] = '\0';
}
}
opts->debug = buff;
debug = opts->debug;
}
char * parse_options_get_debug(Parse_Options opts) {
static char buff[256];
char *buffp = buff;
strcpy(buff, opts->debug);
if (buffp[0] == ',')
buffp++;
if ((buffp[0] != '\0') && buffp[strlen(buffp)-1] == ',')
buffp[strlen(buffp)-1] = '\0';
return buffp;
}
void parse_options_set_test(Parse_Options opts, const char * dummy)
{
static char buff[256];
size_t len = strlen(dummy);
if (0 == strcmp(dummy, opts->test)) return;
if (0 == len)
{
buff[0] = '\0';
}
else
{
buff[0] = ',';
strncpy(buff+1, dummy, sizeof(buff)-2);
if (len < sizeof(buff)-2)
{
buff[len+1] = ',';
buff[len+2] = '\0';
}
else
{
buff[sizeof(buff)-1] = '\0';
}
}
opts->test = buff;
test = opts->test;
}
char * parse_options_get_test(Parse_Options opts) {
static char buff[256];
char *buffp = buff;
strcpy(buff, opts->test);
if (buffp[0] == ',')
buffp++;
if ((buffp[0] != '\0') && buffp[strlen(buffp)-1] == ',')
buffp[strlen(buffp)-1] = '\0';
return buffp;
}
void parse_options_set_use_sat_parser(Parse_Options opts, bool dummy) {
#ifdef USE_SAT_SOLVER
opts->use_sat_solver = dummy;
#else
if (dummy && (verbosity > D_USER_BASIC))
{
prt_error("Error: Cannot enable the Boolean SAT parser; "
"this library was built without SAT solver support.\n");
}
#endif
}
bool parse_options_get_use_sat_parser(Parse_Options opts) {
#if USE_SAT_SOLVER
return opts->use_sat_solver;
#else
return false;
#endif
}
void parse_options_set_linkage_limit(Parse_Options opts, int dummy)
{
opts->linkage_limit = dummy;
}
int parse_options_get_linkage_limit(Parse_Options opts)
{
return opts->linkage_limit;
}
void parse_options_set_disjunct_cost(Parse_Options opts, float dummy)
{
opts->disjunct_cost = dummy;
}
float parse_options_get_disjunct_cost(Parse_Options opts)
{
return opts->disjunct_cost;
}
void parse_options_set_min_null_count(Parse_Options opts, int val) {
opts->min_null_count = val;
}
int parse_options_get_min_null_count(Parse_Options opts) {
return opts->min_null_count;
}
void parse_options_set_max_null_count(Parse_Options opts, int val) {
opts->max_null_count = val;
}
int parse_options_get_max_null_count(Parse_Options opts) {
return opts->max_null_count;
}
void parse_options_set_islands_ok(Parse_Options opts, bool dummy) {
opts->islands_ok = dummy;
}
bool parse_options_get_islands_ok(Parse_Options opts) {
return opts->islands_ok;
}
void parse_options_set_spell_guess(Parse_Options opts, int dummy) {
#if defined HAVE_HUNSPELL || defined HAVE_ASPELL
opts->use_spell_guess = dummy;
#else
if (dummy && (verbosity > D_USER_BASIC))
{
prt_error("Error: Cannot enable spell guess; "
"this library was built without spell guess support.\n");
}
#endif
}
int parse_options_get_spell_guess(Parse_Options opts) {
return opts->use_spell_guess;
}
void parse_options_set_short_length(Parse_Options opts, int short_length) {
opts->short_length = MIN(short_length, UNLIMITED_LEN);
}
int parse_options_get_short_length(Parse_Options opts) {
return opts->short_length;
}
void parse_options_set_all_short_connectors(Parse_Options opts, bool val) {
opts->all_short = val;
}
bool parse_options_get_all_short_connectors(Parse_Options opts) {
return opts->all_short;
}
void parse_options_set_repeatable_rand(Parse_Options opts, bool val)
{
opts->repeatable_rand = val;
if (val) global_rand_state = 0;
else if (0 == global_rand_state) global_rand_state = 42;
}
bool parse_options_get_repeatable_rand(Parse_Options opts) {
return opts->repeatable_rand;
}
void parse_options_set_max_parse_time(Parse_Options opts, int dummy) {
opts->resources->max_parse_time = dummy;
}
int parse_options_get_max_parse_time(Parse_Options opts) {
return opts->resources->max_parse_time;
}
void parse_options_set_max_memory(Parse_Options opts, int dummy) {
opts->resources->max_memory = dummy;
}
int parse_options_get_max_memory(Parse_Options opts) {
return opts->resources->max_memory;
}
int parse_options_get_display_morphology(Parse_Options opts) {
return opts->display_morphology;
}
void parse_options_set_display_morphology(Parse_Options opts, int dummy) {
opts->display_morphology = dummy;
}
bool parse_options_timer_expired(Parse_Options opts) {
return resources_timer_expired(opts->resources);
}
bool parse_options_memory_exhausted(Parse_Options opts) {
return resources_memory_exhausted(opts->resources);
}
bool parse_options_resources_exhausted(Parse_Options opts) {
return (resources_exhausted(opts->resources));
}
void parse_options_reset_resources(Parse_Options opts) {
resources_reset(opts->resources);
}
static void free_dialect_info(dialect_info *dinfo)
{
if (dinfo == NULL) return;
free(dinfo->cost_table);
dinfo->cost_table = NULL;
free((void *)dinfo->conf);
}
char * parse_options_get_dialect(Parse_Options opts)
{
return opts->dialect.conf;
}
void parse_options_set_dialect(Parse_Options opts, const char *dconf)
{
if (0 == strcmp(dconf, opts->dialect.conf)) return;
free_dialect_info(&opts->dialect);
opts->dialect.conf = strdup(dconf);
}