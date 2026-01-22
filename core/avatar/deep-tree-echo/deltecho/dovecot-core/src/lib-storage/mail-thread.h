#ifndef MAIL_THREAD_H
#define MAIL_THREAD_H
struct mailbox;
struct mail_search_args;
struct mail_thread_context;
enum mail_thread_type {
MAIL_THREAD_NONE,
MAIL_THREAD_ORDEREDSUBJECT,
MAIL_THREAD_REFERENCES,
MAIL_THREAD_REFS
};
struct mail_thread_child_node {
uint32_t idx;
uint32_t uid;
time_t sort_date;
};
ARRAY_DEFINE_TYPE(mail_thread_child_node, struct mail_thread_child_node);
bool mail_thread_type_parse(const char *str, enum mail_thread_type *type_r);
const char *mail_thread_type_to_str(enum mail_thread_type type);
int mail_thread_init(struct mailbox *box, struct mail_search_args *args,
struct mail_thread_context **ctx_r) ATTR_NULL(2);
void mail_thread_deinit(struct mail_thread_context **ctx);
struct mail_thread_iterate_context *
mail_thread_iterate_init(struct mail_thread_context *ctx,
enum mail_thread_type thread_type, bool write_seqs);
const struct mail_thread_child_node *
mail_thread_iterate_next(struct mail_thread_iterate_context *iter,
struct mail_thread_iterate_context **child_iter_r);
unsigned int
mail_thread_iterate_count(struct mail_thread_iterate_context *iter);
int mail_thread_iterate_deinit(struct mail_thread_iterate_context **iter);
#endif