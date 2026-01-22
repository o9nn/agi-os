#ifndef _LINK_GRAMMAR_ERROR_H_
#define _LINK_GRAMMAR_ERROR_H_
#include "link-includes.h"
#include "externs.h"
#include "utilities.h"
#define D_USER_BASIC 1
#define D_USER_TIMES 2
#define D_USER_INFO 3
#define D_USER_FILES 4
#define D_USER_MAX 4
#define D_DICT 10
#define D_SPEC 100
typedef struct
{
Sentence sent;
} err_ctxt;
void err_msgc(err_ctxt *, lg_error_severity, const char *fmt, ...) GNUC_PRINTF(3,4);
#define err_msg(...) err_msgc(NULL, __VA_ARGS__)
const char *feature_enabled(const char *, ...);
void debug_msg(int, int, char, const char[], const char[], const char *fmt, ...)
GNUC_PRINTF(6,7);
bool verbosity_check(int, int, char, const char[], const char[], const char *);
const char *syserror_msg(int);
void lg_lib_failure(void);
#define lgdebug(level, ...) \
do { \
if (verbosity >= (level)) \
debug_msg(level, verbosity, STRINGIFY(level)[0], __func__, __FILE__, \
__VA_ARGS__); \
} \
while(0)
#define verbosity_level(level, ...) \
((verbosity >= (level)) && \
verbosity_check(level, verbosity, STRINGIFY(level)[0], __func__, __FILE__, \
"" __VA_ARGS__))
#define test_enabled(feature) \
(('\0' != test[0]) ? feature_enabled(test, feature, NULL) : NULL)
extern void (* assert_failure_trap)(void);
#define FILELINE __FILE__ ":" STRINGIFY(__LINE__)
NORETURN
void assert_failure(const char[], const char[], const char *, const char *, ...)
GNUC_PRINTF(4,5);
#undef assert
#define assert(ex, ...) \
do { \
if (!(ex)) assert_failure(#ex, __func__, FILELINE, __VA_ARGS__); \
} \
while(0)
#ifdef DEBUG
#define dassert assert
#else
#define dassert(...)
#endif
#endif