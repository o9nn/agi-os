#ifndef IMAP_ARG_H
#define IMAP_ARG_H
#include "array.h"
#define IS_ASTRING_CHAR(c) (IS_ATOM_CHAR(c) || IS_RESP_SPECIAL(c))
#define IS_ATOM_CHAR(c) (!IS_ATOM_SPECIAL(c))
#define IS_ATOM_SPECIAL(c) \
((unsigned char)(c) <= 0x20 || (unsigned char)(c) >= 0x7f || \
(c) == '(' || (c) == ')' || (c) == '{' || IS_LIST_WILDCARD(c) || \
IS_QUOTED_SPECIAL(c) || IS_RESP_SPECIAL(c))
#define IS_LIST_WILDCARD(c) ((c) == '%' || (c) == '*')
#define IS_QUOTED_SPECIAL(c) ((c) == '\"' || (c) == '\\')
#define IS_RESP_SPECIAL(c) ((c) == ']')
enum imap_arg_type {
IMAP_ARG_NIL = 0,
IMAP_ARG_ATOM,
IMAP_ARG_STRING,
IMAP_ARG_LIST,
IMAP_ARG_LITERAL,
IMAP_ARG_LITERAL_SIZE,
IMAP_ARG_LITERAL_SIZE_NONSYNC,
IMAP_ARG_EOL
};
ARRAY_DEFINE_TYPE(imap_arg_list, struct imap_arg);
struct imap_arg {
enum imap_arg_type type;
struct imap_arg *parent;
size_t str_len;
union {
const char *str;
uoff_t literal_size;
ARRAY_TYPE(imap_arg_list) list;
} _data;
bool literal8:1;
};
#define IMAP_ARG_TYPE_IS_ASTRING(type) \
((type) == IMAP_ARG_ATOM || \
(type) == IMAP_ARG_STRING || \
(type) == IMAP_ARG_LITERAL)
#define IMAP_ARG_IS_ASTRING(arg) \
IMAP_ARG_TYPE_IS_ASTRING((arg)->type)
#define IMAP_ARG_IS_NSTRING(arg) \
(IMAP_ARG_IS_ASTRING(arg) || (arg)->type == IMAP_ARG_NIL)
#define IMAP_ARG_IS_EOL(arg) \
((arg)->type == IMAP_ARG_EOL)
bool imap_arg_get_atom(const struct imap_arg *arg, const char **str_r)
ATTR_WARN_UNUSED_RESULT;
bool imap_arg_get_quoted(const struct imap_arg *arg, const char **str_r)
ATTR_WARN_UNUSED_RESULT;
bool imap_arg_get_string(const struct imap_arg *arg, const char **str_r)
ATTR_WARN_UNUSED_RESULT;
bool imap_arg_get_astring(const struct imap_arg *arg, const char **str_r)
ATTR_WARN_UNUSED_RESULT;
bool imap_arg_get_nstring(const struct imap_arg *arg, const char **str_r)
ATTR_WARN_UNUSED_RESULT;
bool imap_arg_get_literal_size(const struct imap_arg *arg, uoff_t *size_r)
ATTR_WARN_UNUSED_RESULT;
bool imap_arg_get_list(const struct imap_arg *arg,
const struct imap_arg **list_r)
ATTR_WARN_UNUSED_RESULT;
bool imap_arg_get_list_full(const struct imap_arg *arg,
const struct imap_arg **list_r,
unsigned int *list_count_r) ATTR_WARN_UNUSED_RESULT;
const char *imap_arg_as_astring(const struct imap_arg *arg);
const char *imap_arg_as_nstring(const struct imap_arg *arg);
uoff_t imap_arg_as_literal_size(const struct imap_arg *arg);
const struct imap_arg *imap_arg_as_list(const struct imap_arg *arg);
bool imap_arg_atom_equals(const struct imap_arg *arg, const char *str);
#endif