#include <stdio.h>
#include <argp.h>
#include <string.h>
#include "priv.h"
#define OPT_SLACK			600
#define OPT_JUMP_DOWN_ON_INPUT		601
#define OPT_NO_JUMP_DOWN_ON_INPUT	602
#define OPT_JUMP_DOWN_ON_OUTPUT		603
#define OPT_NO_JUMP_DOWN_ON_OUTPUT	604
#define OPT_VISUAL_BELL			605
#define OPT_AUDIBLE_BELL		606
#define OPT_MOUSE_SHOW			607
#define OPT_MOUSE_HIDE			608
#define OPT_MOUSE_SENS			609
#define DEFAULT_SLACK 100
#define DEFAULT_SLACK_STRING STRINGIFY(DEFAULT_SLACK)
#define STRINGIFY(x) STRINGIFY_1(x)
#define STRINGIFY_1(x) #x
#define DEFAULT_MOUSE_SENS 3.0
#define DEFAULT_MOUSE_SENS_STRING STRINGIFY(DEFAULT_MOUSE_SENS)
int _cons_slack = DEFAULT_SLACK;
int _cons_jump_down_on_input = 1;
int _cons_jump_down_on_output;
char *cons_file;
bell_type_t _cons_visual_bell = BELL_VISUAL;
bell_type_t _cons_audible_bell = BELL_AUDIBLE;
int _cons_show_mouse = CONS_EVT_MOUSE_MOVE;
int _cons_hide_mouse = CONS_EVT_KEYPRESS;
float _cons_mouse_sens = DEFAULT_MOUSE_SENS;
static const struct argp_option
startup_options[] =
{
{ "slack", OPT_SLACK, "RECORDS", 0, "Max number of records the client is"
" allowed to lag behind the server (default " DEFAULT_SLACK_STRING ")" },
{ "jump-down-on-input", OPT_JUMP_DOWN_ON_INPUT, NULL, 0,
"End scrollback when something is entered (default)" },
{ "no-jump-down-on-input", OPT_NO_JUMP_DOWN_ON_INPUT, NULL, 0,
"End scrollback when something is entered" },
{ "jump-down-on-output", OPT_JUMP_DOWN_ON_OUTPUT, NULL, 0,
"End scrollback when something is printed" },
{ "no-jump-down-on-output", OPT_NO_JUMP_DOWN_ON_OUTPUT, NULL, 0,
"End scrollback when something is printed (default)" },
{ "visual-bell", OPT_VISUAL_BELL, "BELL", 0, "Visual bell: on (default), "
"off, visual, audible" },
{ "audible-bell", OPT_AUDIBLE_BELL, "BELL", 0, "Audible bell: on (default), "
"off, visual, audible" },
{ "mouse-show-on", OPT_MOUSE_SHOW, "EVENTS", 0, "One or more of the events"
" mousemove, mousebutton, keypress, output (default is mousemove), if one"
" of these events occur the mouse cursor will be made visible" },
{ "mouse-hide-on", OPT_MOUSE_HIDE, "EVENTS", 0, "One or more of the events"
" mousemove, mousebutton, keypress, output (default is keypress), if one"
" of these events occur the mouse cursor will be hidden " },
{ "mouse-sensitivity", OPT_MOUSE_SENS, "SENSITIVITY", 0, "The mouse"
" sensitivity (default " DEFAULT_MOUSE_SENS_STRING ").  A lower value"
" means more sensitive" },
{ 0, 0 }
};
static const char args_doc[] = "CONSOLE";
static const char doc[] = "A console client.";
static error_t
parse_startup_opt (int opt, char *arg, struct argp_state *state)
{
int parse_events (char *events)
{
char *evtstr = strdupa (events);
char *tok = strtok (evtstr, ",");
int evmask = 0;
while (tok)
{
if (!strcasecmp ("mousemove", tok))
evmask |= CONS_EVT_MOUSE_MOVE;
else if (!strcasecmp ("mousebutton", tok))
evmask |= CONS_EVT_MOUSE_BUTTON;
else if (!strcasecmp ("keypress", tok))
evmask |= CONS_EVT_KEYPRESS;
else if (!strcasecmp ("output", tok))
evmask |= CONS_EVT_OUTPUT;
else
argp_error (state, "The event can be one of: MOUSEMOVE,"
" MOUSEBUTTON, KEYPRESS or OUTPUT");
tok = strtok (NULL, ",");
}
return evmask;
}
switch (opt)
{
case OPT_SLACK:
_cons_slack = atoi (arg);
break;
case OPT_JUMP_DOWN_ON_INPUT:
_cons_jump_down_on_input = 1;
break;
case OPT_NO_JUMP_DOWN_ON_INPUT:
_cons_jump_down_on_input = 0;
break;
case OPT_JUMP_DOWN_ON_OUTPUT:
_cons_jump_down_on_output = 1;
break;
case OPT_NO_JUMP_DOWN_ON_OUTPUT:
_cons_jump_down_on_output = 0;
break;
case OPT_AUDIBLE_BELL:
if (!strcasecmp ("on", arg) || !strcasecmp ("audible", arg))
_cons_audible_bell = BELL_AUDIBLE;
else if (!strcasecmp ("off", arg))
_cons_audible_bell = BELL_OFF;
else if (!strcasecmp ("visual", arg))
_cons_audible_bell = BELL_VISUAL;
else
argp_error (state, "The audible bell can be one of: on, off, visual, "
"audible");
break;
case OPT_VISUAL_BELL:
if (!strcasecmp ("on", arg) || !strcasecmp ("visual", arg))
_cons_visual_bell = BELL_VISUAL;
else if (!strcasecmp ("off", arg))
_cons_visual_bell = BELL_OFF;
else if (!strcasecmp ("audible", arg))
_cons_visual_bell = BELL_AUDIBLE;
else
argp_error (state, "The visual bell can be one of: on, off, visual, "
"audible");
break;
case OPT_MOUSE_SHOW:
_cons_show_mouse = parse_events (arg);
break;
case OPT_MOUSE_HIDE:
_cons_hide_mouse = parse_events (arg);
break;
case OPT_MOUSE_SENS:
{
char *tail;
errno = 0;
_cons_mouse_sens = strtod (arg, &tail);
if (tail == NULL || tail == arg || *tail != '\0')
argp_error (state, "SENSITIVITY is not a number: %s", arg);
if (errno)
argp_error (state, "Overflow in argument SENSITIVITY %s", arg);
break;
}
case ARGP_KEY_ARG:
if (state->arg_num > 0)
argp_error (state, "Too many non option arguments");
cons_file = arg;
break;
case ARGP_KEY_NO_ARGS:
argp_error (state, "Filename of console server missing");
break;
default:
return ARGP_ERR_UNKNOWN;
}
return 0;
}
const struct argp
cons_startup_argp =
{
startup_options, parse_startup_opt, args_doc, doc
};