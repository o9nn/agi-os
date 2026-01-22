#ifndef TEST_SUBPROCESS_H
#define TEST_SUBPROCESS_H
#define TEST_SIGNALS_DEFAULT_TIMEOUT_MS 10000
struct test_subprocess;
void test_subprocess_fork(int (*func)(void *), void *context,
bool continue_test);
#define test_subprocess_fork(func, context, continue_test) \
test_subprocess_fork( \
(int(*)(void*))func, \
(TRUE ? context : \
CALLBACK_TYPECHECK(func, int(*)(typeof(context)))), \
continue_test)
void test_subprocess_kill_all(unsigned int timeout_secs);
void test_subprocess_set_cleanup_callback(void (*callback)(void));
void test_subprocess_notify_signal_send(int signo, pid_t pid);
void test_subprocess_notify_signal_send_parent(int signo);
void test_subprocess_notify_signal_all(int signo);
void test_subprocess_notify_signal_reset(int signo);
void test_subprocess_notify_signal_wait(int signo, unsigned int timeout_msecs);
void test_subprocesses_init(bool debug);
void test_subprocesses_deinit(void);
#endif