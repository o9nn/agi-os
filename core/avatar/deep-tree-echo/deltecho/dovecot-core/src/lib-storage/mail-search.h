#ifndef MAIL_SEARCH_H
#define MAIL_SEARCH_H
#include "seq-range-array.h"
#include "mail-types.h"
#include "mail-thread.h"
struct mail_search_mime_part;
enum mail_search_arg_type {
SEARCH_OR,
SEARCH_SUB,
SEARCH_ALL,
SEARCH_SEQSET,
SEARCH_UIDSET,
SEARCH_FLAGS,
SEARCH_KEYWORDS,
SEARCH_BEFORE,
SEARCH_ON,
SEARCH_SINCE,
SEARCH_SMALLER,
SEARCH_LARGER,
SEARCH_HEADER,
SEARCH_HEADER_ADDRESS,
SEARCH_HEADER_COMPRESS_LWSP,
SEARCH_BODY,
SEARCH_TEXT,
SEARCH_MODSEQ,
SEARCH_SAVEDATESUPPORTED,
SEARCH_INTHREAD,
SEARCH_GUID,
SEARCH_MAILBOX,
SEARCH_MAILBOX_GUID,
SEARCH_MAILBOX_GLOB,
SEARCH_REAL_UID,
SEARCH_MIMEPART
};
enum mail_search_date_type {
MAIL_SEARCH_DATE_TYPE_SENT = 1,
MAIL_SEARCH_DATE_TYPE_RECEIVED,
MAIL_SEARCH_DATE_TYPE_SAVED
};
enum mail_search_arg_flag {
MAIL_SEARCH_ARG_FLAG_UTC_TIMES	= 0x01,
};
enum mail_search_modseq_type {
MAIL_SEARCH_MODSEQ_TYPE_ANY = 0,
MAIL_SEARCH_MODSEQ_TYPE_PRIVATE,
MAIL_SEARCH_MODSEQ_TYPE_SHARED
};
struct mail_search_modseq {
uint64_t modseq;
enum mail_search_modseq_type type;
};
struct mail_search_arg {
struct mail_search_arg *next;
enum mail_search_arg_type type;
struct {
struct mail_search_arg *subargs;
ARRAY_TYPE(seq_range) seqset;
const char *str;
time_t time;
uoff_t size;
enum mail_flags flags;
enum mail_search_arg_flag search_flags;
enum mail_search_date_type date_type;
enum mail_thread_type thread_type;
struct mail_search_modseq *modseq;
struct mail_search_result *search_result;
struct mail_search_mime_part *mime_part;
} value;
struct {
struct mail_search_args *search_args;
struct mail_keywords *keywords;
struct imap_match_glob *mailbox_glob;
} initialized;
void *context;
const char *hdr_field_name;
bool match_not:1;
bool match_always:1;
bool nonmatch_always:1;
bool fuzzy:1;
bool no_fts:1;
int result;
};
struct mail_search_args {
int refcount, init_refcount;
pool_t pool;
struct mailbox *box;
struct mail_search_arg *args;
bool simplified:1;
bool have_inthreads:1;
bool stop_on_nonmatch:1;
bool fts_expanded:1;
};
#define ARG_SET_RESULT(arg, res) \
STMT_START { \
(arg)->result = !(arg)->match_not ? (res) : \
((res) == -1 ? -1 : ((res) == 0 ? 1 : 0)); \
} STMT_END
typedef void mail_search_foreach_callback_t(struct mail_search_arg *arg,
void *context);
void mail_search_args_init(struct mail_search_args *args,
struct mailbox *box, bool change_sets,
const ARRAY_TYPE(seq_range) *search_saved_uidset)
ATTR_NULL(4);
void mail_search_arg_init(struct mail_search_args *args,
struct mail_search_arg *arg);
void mail_search_args_deinit(struct mail_search_args *args);
void mail_search_arg_deinit(struct mail_search_arg *arg);
void mail_search_arg_one_deinit(struct mail_search_arg *arg);
void mail_search_args_seq2uid(struct mail_search_args *args);
bool mail_search_args_equal(const struct mail_search_args *args1,
const struct mail_search_args *args2);
bool mail_search_arg_equals(const struct mail_search_arg *arg1,
const struct mail_search_arg *arg2);
int mail_search_arg_equals_p(const struct mail_search_arg *const *arg1,
const struct mail_search_arg *const *arg2);
bool mail_search_arg_one_equals(const struct mail_search_arg *arg1,
const struct mail_search_arg *arg2);
void mail_search_args_ref(struct mail_search_args *args);
void mail_search_args_unref(struct mail_search_args **args);
struct mail_search_args *
mail_search_args_dup(const struct mail_search_args *args);
struct mail_search_arg *
mail_search_arg_dup(pool_t pool, const struct mail_search_arg *arg);
void mail_search_args_reset(struct mail_search_arg *args, bool full_reset);
int mail_search_args_foreach(struct mail_search_arg *args,
mail_search_foreach_callback_t *callback,
void *context) ATTR_NULL(3);
#define mail_search_args_foreach(args, callback, context) \
mail_search_args_foreach(args - \
CALLBACK_TYPECHECK(callback, void (*)( \
struct mail_search_arg *, typeof(context))), \
(mail_search_foreach_callback_t *)callback, context)
const char *const *
mail_search_args_analyze(struct mail_search_arg *args,
bool *have_headers, bool *have_body);
bool mail_search_args_match_mailbox(struct mail_search_args *args,
const char *vname, char sep);
void mail_search_args_simplify(struct mail_search_args *args);
bool mail_search_args_to_imap(string_t *dest, const struct mail_search_arg *args,
const char **error_r);
bool mail_search_arg_to_imap(string_t *dest, const struct mail_search_arg *arg,
const char **error_r);
void mail_search_args_to_cmdline(string_t *dest,
const struct mail_search_arg *args);
void mail_search_args_result_serialize(const struct mail_search_args *args,
buffer_t *dest);
void mail_search_args_result_deserialize(struct mail_search_args *args,
const unsigned char *data,
size_t size);
struct mail_search_seqset_iter *
mail_search_seqset_iter_init(struct mail_search_args *search_args,
uint32_t messages_count, unsigned int batch_size);
bool mail_search_seqset_iter_next(struct mail_search_seqset_iter *iter);
void mail_search_seqset_iter_deinit(struct mail_search_seqset_iter **iter);
#endif