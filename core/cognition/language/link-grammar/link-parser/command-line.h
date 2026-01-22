#ifndef _COMMAND_LINE_
#define _COMMAND_LINE_
#include <link-grammar/link-includes.h>
#define D_USER_FILES 4
#define COMMENT_CHAR '%'
#define WHITESPACE " \t\v\r\n"
#define FIELD_WIDTH(str, width) (int)((width)+strlen(str)-utf8_strwidth(str))
#define INITIAL_SCREEN_WIDTH 16381
#if !defined(MIN)
#define MIN(X,Y)  (((X) < (Y)) ? (X) : (Y))
#endif
#if !defined(MAX)
#define MAX(X,Y)  (((X) > (Y)) ? (X) : (Y))
#endif
typedef struct
{
float max_cost;
int linkage_limit;
int max_null_count;
int short_length;
int spell_guess;
int timeout;
} panic_options;
typedef struct {
Parse_Options popts;
panic_options panic;
unsigned int screen_width;
bool batch_mode;
bool allow_null;
bool echo_on;
bool panic_mode;
bool display_on;
bool display_walls;
bool display_postscript;
bool display_ps_header;
ConstituentDisplayStyle display_constituents;
bool display_bad;
bool display_disjuncts;
bool display_links;
int  display_wordgraph;
} Command_Options;
void put_local_vars_in_opts(Command_Options *);
void put_opts_in_local_vars(Command_Options *);
void setup_panic_parse_options(Command_Options *, int);
typedef enum
{
Int,
Bool,
Float,
String,
Cmd,
} ParamType;
typedef struct
{
const char *string;
ParamType param_type;
const char *description;
void *ptr;
} Switch;
void save_default_opts(Command_Options*);
int issue_special_command(const char*, Command_Options*, Dictionary);
Command_Options* command_options_create(void);
void command_options_delete(Command_Options*);
void display_1line_help(const Switch *, bool);
void print_url_info(void);
#define UNDOC "\1"
#endif