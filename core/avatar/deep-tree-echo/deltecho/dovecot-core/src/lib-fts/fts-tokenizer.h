#ifndef FTS_TOKENIZER_H
#define FTS_TOKENIZER_H
extern const struct fts_tokenizer *fts_tokenizer_email_address;
extern const struct fts_tokenizer *fts_tokenizer_generic;
void fts_tokenizers_init(void);
void fts_tokenizers_deinit(void);
const struct fts_tokenizer *fts_tokenizer_find(const char *name);
int fts_tokenizer_create(const struct fts_tokenizer *tok_class,
struct fts_tokenizer *parent,
const char *const *settings,
struct fts_tokenizer **tokenizer_r,
const char **error_r);
void fts_tokenizer_ref(struct fts_tokenizer *tok);
void fts_tokenizer_unref(struct fts_tokenizer **tok);
void fts_tokenizer_reset(struct fts_tokenizer *tok);
int fts_tokenizer_next(struct fts_tokenizer *tok,
const unsigned char *data, size_t size,
const char **token_r, const char **error_r);
int fts_tokenizer_final(struct fts_tokenizer *tok, const char **token_r,
const char **error_r);
const char *fts_tokenizer_name(const struct fts_tokenizer *tok);
#endif