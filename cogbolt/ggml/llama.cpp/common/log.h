#pragma once
#include "ggml.h"
#define LOG_CLR_TO_EOL "\033[K\r"
#define LOG_COL_DEFAULT "\033[0m"
#define LOG_COL_BOLD "\033[1m"
#define LOG_COL_RED "\033[31m"
#define LOG_COL_GREEN "\033[32m"
#define LOG_COL_YELLOW "\033[33m"
#define LOG_COL_BLUE "\033[34m"
#define LOG_COL_MAGENTA "\033[35m"
#define LOG_COL_CYAN "\033[36m"
#define LOG_COL_WHITE "\033[37m"
#ifndef __GNUC__
# define LOG_ATTRIBUTE_FORMAT(...)
#elif defined(__MINGW32__) && !defined(__clang__)
# define LOG_ATTRIBUTE_FORMAT(...) __attribute__((format(gnu_printf, __VA_ARGS__)))
#else
# define LOG_ATTRIBUTE_FORMAT(...) __attribute__((format(printf, __VA_ARGS__)))
#endif
#define LOG_DEFAULT_DEBUG 1
#define LOG_DEFAULT_LLAMA 0
extern int common_log_verbosity_thold;
void common_log_set_verbosity_thold(int verbosity);
struct common_log;
struct common_log * common_log_init();
struct common_log * common_log_main();
void common_log_pause (struct common_log * log);
void common_log_resume(struct common_log * log);
void common_log_free (struct common_log * log);
LOG_ATTRIBUTE_FORMAT(3, 4)
void common_log_add(struct common_log * log, enum ggml_log_level level, const char * fmt, ...);
void common_log_set_file (struct common_log * log, const char * file);
void common_log_set_colors (struct common_log * log, bool colors);
void common_log_set_prefix (struct common_log * log, bool prefix);
void common_log_set_timestamps(struct common_log * log, bool timestamps);
#define LOG_TMPL(level, verbosity, ...) \
do { \
if ((verbosity) <= common_log_verbosity_thold) { \
common_log_add(common_log_main(), (level), __VA_ARGS__); \
} \
} while (0)
#define LOG(...) LOG_TMPL(GGML_LOG_LEVEL_NONE, 0, __VA_ARGS__)
#define LOGV(verbosity, ...) LOG_TMPL(GGML_LOG_LEVEL_NONE, verbosity, __VA_ARGS__)
#define LOG_INF(...) LOG_TMPL(GGML_LOG_LEVEL_INFO, 0, __VA_ARGS__)
#define LOG_WRN(...) LOG_TMPL(GGML_LOG_LEVEL_WARN, 0, __VA_ARGS__)
#define LOG_ERR(...) LOG_TMPL(GGML_LOG_LEVEL_ERROR, 0, __VA_ARGS__)
#define LOG_DBG(...) LOG_TMPL(GGML_LOG_LEVEL_DEBUG, LOG_DEFAULT_DEBUG, __VA_ARGS__)
#define LOG_CNT(...) LOG_TMPL(GGML_LOG_LEVEL_CONT, 0, __VA_ARGS__)
#define LOG_INFV(verbosity, ...) LOG_TMPL(GGML_LOG_LEVEL_INFO, verbosity, __VA_ARGS__)
#define LOG_WRNV(verbosity, ...) LOG_TMPL(GGML_LOG_LEVEL_WARN, verbosity, __VA_ARGS__)
#define LOG_ERRV(verbosity, ...) LOG_TMPL(GGML_LOG_LEVEL_ERROR, verbosity, __VA_ARGS__)
#define LOG_DBGV(verbosity, ...) LOG_TMPL(GGML_LOG_LEVEL_DEBUG, verbosity, __VA_ARGS__)
#define LOG_CNTV(verbosity, ...) LOG_TMPL(GGML_LOG_LEVEL_CONT, verbosity, __VA_ARGS__)