#ifndef __PARSE_H__
#define __PARSE_H__
#include <errno.h>
#include <argp.h>
extern error_t
parse_strlist (char *arg,
error_t (*add_fn)(const char *str, struct argp_state *state),
const char *(*default_fn)(struct argp_state *state),
const char *type_name, struct argp_state *state);
extern error_t
parse_numlist (char *arg,
error_t (*add_fn)(unsigned num, struct argp_state *state),
int (*default_fn)(struct argp_state *state),
int (*lookup_fn)(const char *str, struct argp_state *state),
const char *type_name, struct argp_state *state);
extern int
parse_enum (const char *arg,
const char *(*choice_fn)(unsigned n),
const char *kind, int allow_mismatches,
struct argp_state *state);
#endif