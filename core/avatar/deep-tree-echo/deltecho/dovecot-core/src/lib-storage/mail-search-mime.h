#ifndef MAIL_SEARCH_MIMEPART_H
#define MAIL_SEARCH_MIMEPART_H
enum mail_search_mime_arg_type {
SEARCH_MIME_OR,
SEARCH_MIME_SUB,
SEARCH_MIME_SIZE_EQUAL,
SEARCH_MIME_SIZE_LARGER,
SEARCH_MIME_SIZE_SMALLER,
SEARCH_MIME_DESCRIPTION,
SEARCH_MIME_DISPOSITION_TYPE,
SEARCH_MIME_DISPOSITION_PARAM,
SEARCH_MIME_ENCODING,
SEARCH_MIME_ID,
SEARCH_MIME_LANGUAGE,
SEARCH_MIME_LOCATION,
SEARCH_MIME_MD5,
SEARCH_MIME_TYPE,
SEARCH_MIME_SUBTYPE,
SEARCH_MIME_PARAM,
SEARCH_MIME_HEADER,
SEARCH_MIME_BODY,
SEARCH_MIME_TEXT,
SEARCH_MIME_CC,
SEARCH_MIME_BCC,
SEARCH_MIME_FROM,
SEARCH_MIME_IN_REPLY_TO,
SEARCH_MIME_MESSAGE_ID,
SEARCH_MIME_REPLY_TO,
SEARCH_MIME_SENDER,
SEARCH_MIME_SENTBEFORE,
SEARCH_MIME_SENTON,
SEARCH_MIME_SENTSINCE,
SEARCH_MIME_SUBJECT,
SEARCH_MIME_TO,
SEARCH_MIME_PARENT,
SEARCH_MIME_CHILD,
SEARCH_MIME_DEPTH_EQUAL,
SEARCH_MIME_DEPTH_MIN,
SEARCH_MIME_DEPTH_MAX,
SEARCH_MIME_INDEX,
SEARCH_MIME_FILENAME_IS,
SEARCH_MIME_FILENAME_CONTAINS,
SEARCH_MIME_FILENAME_BEGINS,
SEARCH_MIME_FILENAME_ENDS
};
struct mail_search_mime_arg {
struct mail_search_mime_arg *next;
enum mail_search_mime_arg_type type;
union {
struct mail_search_mime_arg *subargs;
const char *str;
time_t time;
uoff_t size;
unsigned int number;
} value;
void *context;
const char *field_name;
bool match_not:1;
bool match_always:1;
bool nonmatch_always:1;
int result;
};
struct mail_search_mime_part {
struct mail_search_mime_arg *args;
bool simplified:1;
};
typedef void
mail_search_mime_foreach_callback_t(struct mail_search_mime_arg *arg,
void *context);
bool mail_search_mime_parts_equal(const struct mail_search_mime_part *mpart1,
const struct mail_search_mime_part *mpart2);
bool mail_search_mime_arg_equals(const struct mail_search_mime_arg *arg1,
const struct mail_search_mime_arg *arg2);
bool mail_search_mime_arg_one_equals(const struct mail_search_mime_arg *arg1,
const struct mail_search_mime_arg *arg2);
struct mail_search_mime_part *
mail_search_mime_part_dup(pool_t pool,
const struct mail_search_mime_part *mpart);
struct mail_search_mime_arg *
mail_search_mime_arg_dup(pool_t pool,
const struct mail_search_mime_arg *arg);
void mail_search_mime_args_reset(struct mail_search_mime_arg *args,
bool full_reset);
int mail_search_mime_args_foreach(struct mail_search_mime_arg *args,
mail_search_mime_foreach_callback_t *callback,
void *context) ATTR_NULL(3);
#define mail_search_mime_args_foreach(args, callback, context) \
mail_search_mime_args_foreach(args - \
CALLBACK_TYPECHECK(callback, void (*)( \
struct mail_search_mime_arg *, typeof(context))), \
(mail_search_mime_foreach_callback_t *)callback, context)
void mail_search_mime_simplify(struct mail_search_mime_part *args);
bool mail_search_mime_part_to_imap(string_t *dest,
const struct mail_search_mime_part *mpart, const char **error_r);
bool mail_search_mime_arg_to_imap(string_t *dest,
const struct mail_search_mime_arg *arg, const char **error_r);
#endif