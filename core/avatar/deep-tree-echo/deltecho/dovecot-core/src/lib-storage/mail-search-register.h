#ifndef MAIL_SEARCH_REGISTER_H
#define MAIL_SEARCH_REGISTER_H
struct mail_search_arg;
struct mail_search_build_context;
struct mail_search_register_arg {
const char *key;
struct mail_search_arg *
(*build)(struct mail_search_build_context *ctx);
};
typedef struct mail_search_arg *
mail_search_register_fallback_t(struct mail_search_build_context *ctx,
const char *key);
struct mail_search_register *mail_search_register_init(void);
void mail_search_register_deinit(struct mail_search_register **reg);
void mail_search_register_add(struct mail_search_register *reg,
const struct mail_search_register_arg *arg,
unsigned int count);
void mail_search_register_fallback(struct mail_search_register *reg,
mail_search_register_fallback_t *fallback);
const struct mail_search_register_arg *
mail_search_register_get(struct mail_search_register *reg,
unsigned int *count_r);
const struct mail_search_register_arg *
mail_search_register_find(struct mail_search_register *reg, const char *key);
bool mail_search_register_get_fallback(struct mail_search_register *reg,
mail_search_register_fallback_t **fallback_r);
struct mail_search_register *mail_search_register_get_imap(void);
struct mail_search_register *mail_search_register_get_human(void);
#endif