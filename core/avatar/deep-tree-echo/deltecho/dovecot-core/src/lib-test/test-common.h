#ifndef TEST_COMMON_H
#define TEST_COMMON_H
#ifdef HAVE_VALGRIND_VALGRIND_H
# include <valgrind/valgrind.h>
# define ON_VALGRIND ((bool) RUNNING_ON_VALGRIND)
#else
# define ON_VALGRIND FALSE
#endif
struct istream *test_istream_create(const char *data);
struct istream *test_istream_create_data(const void *data, size_t size);
void test_istream_set_size(struct istream *input, uoff_t size);
void test_istream_set_allow_eof(struct istream *input, bool allow);
void test_istream_set_max_buffer_size(struct istream *input, size_t size);
struct ostream *test_ostream_create(buffer_t *output);
struct ostream *test_ostream_create_nonblocking(buffer_t *output,
size_t max_internal_buffer_size);
void test_ostream_set_max_output_size(struct ostream *output, size_t max_size);
void test_begin(const char *name);
#define test_failed(reason) test_assert_failed(reason, __FILE__, __LINE__)
#define test_assert(code) STMT_START { \
if (!(code)) test_assert_failed(#code, __FILE__, __LINE__); \
} STMT_END
#define test_assert_idx(code, i) STMT_START { \
if (!(code)) test_assert_failed_idx(#code, __FILE__, __LINE__, i); \
} STMT_END
#define test_assert_strcmp(s1, s2) STMT_START { \
test_assert_strcmp_idx(s1, s2, LLONG_MIN); \
} STMT_END
#define test_assert_strcmp_idx(_s1, _s2, i) STMT_START { \
const char *_temp_s1 = (_s1); \
const char *_temp_s2 = (_s2); \
if ((null_strcmp(_temp_s1,_temp_s2) != 0)) \
test_assert_failed_strcmp_idx("strcmp(" #_s1 "," #_s2 ")", \
__FILE__, __LINE__, _temp_s1, _temp_s2, i); \
} STMT_END
#define test_assert_cmp_bool(_bool_value1, _op, _value2) \
test_assert_cmp((unsigned int) _bool_value1, _op, (unsigned int _bool_value2))
#define test_assert_cmp(_value1, _op, _value2) \
test_assert_cmp_idx(_value1, _op, _value2, LLONG_MIN)
#define test_assert_cmp_idx(_value1, _op, _value2, _idx) STMT_START { \
intmax_t _temp_value1 = (_value1); \
intmax_t _temp_value2 = (_value2); \
if (!(_value1 _op _value2)) \
test_assert_failed_cmp_intmax_idx( \
#_value1 " " #_op " " #_value2, \
__FILE__, __LINE__, _temp_value1, _temp_value2, \
#_op, _idx); \
} STMT_END
#define test_assert_ucmp(_value1, _op, _value2) \
test_assert_ucmp_idx(_value1, _op, _value2, LLONG_MIN)
#define test_assert_ucmp_idx(_value1, _op, _value2, _idx) STMT_START { \
uintmax_t _temp_value1 = (_value1); \
uintmax_t _temp_value2 = (_value2); \
if (!(_value1 _op _value2)) \
test_assert_failed_ucmp_intmax_idx( \
#_value1 " " #_op " " #_value2, \
__FILE__, __LINE__, _temp_value1, _temp_value2, \
#_op, _idx); \
} STMT_END
#ifdef STATIC_CHECKER
# define ATTR_STATIC_CHECKER_NORETURN ATTR_NORETURN
#else
# define ATTR_STATIC_CHECKER_NORETURN
#endif
void test_assert_failed(const char *code, const char *file, unsigned int line)
ATTR_STATIC_CHECKER_NORETURN;
void test_assert_failed_idx(const char *code, const char *file, unsigned int line, long long i)
ATTR_STATIC_CHECKER_NORETURN;
void test_assert_failed_strcmp_idx(const char *code, const char *file, unsigned int line,
const char * src, const char * dst, long long i)
ATTR_STATIC_CHECKER_NORETURN;
void test_assert_failed_cmp_intmax_idx(const char *code, const char *file,
unsigned int line,
intmax_t src, intmax_t dst,
const char *op, long long i)
ATTR_STATIC_CHECKER_NORETURN;
void test_assert_failed_ucmp_intmax_idx(const char *code, const char *file,
unsigned int line,
uintmax_t src, uintmax_t dst,
const char *op, long long i)
ATTR_STATIC_CHECKER_NORETURN;
bool test_has_failed(void);
void test_expect_errors(unsigned int expected);
void test_expect_error_string(const char *substr);
void test_expect_error_string_n_times(const char *substr, unsigned int times);
void test_expect_no_more_errors(void);
void test_end(void);
void test_out(const char *name, bool success);
void test_out_reason(const char *name, bool success, const char *reason)
ATTR_NULL(3);
void test_out_quiet(const char *name, bool success);
void test_out_reason_quiet(const char *name, bool success, const char *reason)
ATTR_NULL(3);
int test_run(void (*const test_functions[])(void)) ATTR_WARN_UNUSED_RESULT;
struct named_test {
const char *name;
void (*func)(void);
};
int test_run_named(const struct named_test tests[], const char *match) ATTR_WARN_UNUSED_RESULT;
#define TEST_DECL(x) void x(void);
#define TEST_NAMELESS(x) x,
#define TEST_NAMED(x) { .name = #x , .func = x },
enum fatal_test_state {
FATAL_TEST_FINISHED,
FATAL_TEST_FAILURE,
FATAL_TEST_ABORT,
};
typedef enum fatal_test_state test_fatal_func_t(unsigned int stage);
typedef void test_fatal_callback_t(void *context);
struct named_fatal {
const char *name;
test_fatal_func_t *func;
};
int test_run_with_fatals(void (*const test_functions[])(void),
test_fatal_func_t *const fatal_functions[]);
int test_run_named_with_fatals(const char *match, const struct named_test tests[],
const struct named_fatal fatals[]);
void test_expect_fatal_string(const char *substr);
void test_fatal_set_callback(test_fatal_callback_t *callback, void *context);
#define test_fatal_set_callback(callback, context) \
test_fatal_set_callback(1 ? (test_fatal_callback_t *)callback : \
CALLBACK_TYPECHECK(callback, void (*)(typeof(context))), \
context)
#define FATAL_DECL(x) enum fatal_test_state x(unsigned int);
#define FATAL_NAMELESS(x) x,
#define FATAL_NAMED(x) { .name = #x , .func = x },
void test_forked_end(void);
void test_exit(int status) ATTR_NORETURN;
int test_create_temp_fd(void);
#endif