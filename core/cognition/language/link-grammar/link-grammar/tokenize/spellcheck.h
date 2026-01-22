#ifndef _SPELLCHECK_H
#define _SPELLCHECK_H
#include "api-types.h"
#if (defined HAVE_HUNSPELL) || (defined HAVE_ASPELL)
void * spellcheck_create(const char * lang);
void spellcheck_destroy(void *);
bool spellcheck_test(void *, const char * word);
int spellcheck_suggest(void * chk, char ***sug, const char * word);
void spellcheck_free_suggest(void * chk, char **sug, int size);
#else
static inline void * spellcheck_create(const char * lang) { return NULL; }
static inline void spellcheck_destroy(void * chk) {}
static inline bool spellcheck_test(void * chk, const char * word) { return false; }
static inline int spellcheck_suggest(void * chk, char ***sug, const char * word) { return 0; }
static inline void spellcheck_free_suggest(void * chk, char **sug, int size) {}
#endif
#endif