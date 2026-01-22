#ifndef FTS_FILTER_H
#define FTS_FILTER_H
struct fts_language;
struct fts_filter;
extern const struct fts_filter *fts_filter_stopwords;
extern const struct fts_filter *fts_filter_stemmer_snowball;
extern const struct fts_filter *fts_filter_normalizer_icu;
extern const struct fts_filter *fts_filter_lowercase;
extern const struct fts_filter *fts_filter_english_possessive;
extern const struct fts_filter *fts_filter_contractions;
void fts_filters_init(void);
void fts_filters_deinit(void);
void fts_filter_register(const struct fts_filter *filter_class);
const struct fts_filter *fts_filter_find(const char *name);
int fts_filter_create(const struct fts_filter *filter_class,
struct fts_filter *parent,
const struct fts_language *lang,
const char *const *settings,
struct fts_filter **filter_r,
const char **error_r);
void fts_filter_ref(struct fts_filter *filter);
void fts_filter_unref(struct fts_filter **filter);
int fts_filter_filter(struct fts_filter *filter, const char **token,
const char **error_r);
#endif