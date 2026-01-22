#ifndef MAIL_SEARCH_MIME_REGISTER_H
#define MAIL_SEARCH_MIME_REGISTER_H
struct mail_search_mime_arg;
struct mail_search_mime_build_context;
struct mail_search_mime_register_arg {
const char *key;
struct mail_search_mime_arg *
(*build)(struct mail_search_mime_build_context *ctx);
};
void mail_search_mime_register_deinit(void);
void mail_search_mime_register_add(
const struct mail_search_mime_register_arg *arg,
unsigned int count);
const struct mail_search_mime_register_arg *
mail_search_mime_register_get(unsigned int *count_r);
const struct mail_search_mime_register_arg *
mail_search_mime_register_find(const char *key);
#endif