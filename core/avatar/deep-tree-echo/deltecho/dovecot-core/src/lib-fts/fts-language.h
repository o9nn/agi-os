#ifndef FTS_LANGUAGE_H
#define FTS_LANGUAGE_H
struct fts_language_list;
enum fts_language_result {
FTS_LANGUAGE_RESULT_SHORT,
FTS_LANGUAGE_RESULT_UNKNOWN,
FTS_LANGUAGE_RESULT_OK,
FTS_LANGUAGE_RESULT_ERROR
};
struct fts_language {
const char *name;
};
ARRAY_DEFINE_TYPE(fts_language, const struct fts_language *);
extern const struct fts_language fts_language_data;
void fts_languages_init(void);
void fts_languages_deinit(void);
void fts_language_register(const char *name);
const struct fts_language *fts_language_find(const char *name);
int fts_language_list_init(const char *const *settings,
struct fts_language_list **list_r,
const char **error_r);
void fts_language_list_deinit(struct fts_language_list **list);
void fts_language_list_add(struct fts_language_list *list,
const struct fts_language *lang);
bool fts_language_list_add_names(struct fts_language_list *list,
const char *names,
const char **unknown_name_r);
const ARRAY_TYPE(fts_language) *
fts_language_list_get_all(struct fts_language_list *list);
const struct fts_language *
fts_language_list_get_first(struct fts_language_list *list);
enum fts_language_result
fts_language_detect(struct fts_language_list *list,
const unsigned char *text, size_t size,
const struct fts_language **lang_r,
const char **error_r);
#endif