#ifndef __SIGNAL_H
#define __SIGNAL_H
#pragma lib "/$M/lib/ape/libap.a"
typedef int sig_atomic_t;
#define SIG_DFL ((void (*)())0)
#define SIG_ERR ((void (*)())-1)
#define SIG_IGN ((void (*)())1)
#define SIGHUP 1
#define SIGINT 2
#define SIGQUIT 3
#define SIGILL 4
#define SIGABRT 5
#define SIGFPE 6
#define SIGKILL 7
#define SIGSEGV 8
#define SIGPIPE 9
#define SIGALRM 10
#define SIGTERM 11
#define SIGUSR1 12
#define SIGUSR2 13
#define SIGBUS 14
#define SIGCHLD 15
#define SIGCONT 16
#define SIGSTOP 17
#define SIGTSTP 18
#define SIGTTIN 19
#define SIGTTOU 20
#ifdef _BSD_EXTENSION
#define NSIG 21
#endif
#ifdef __cplusplus
extern "C" {
#endif
extern void (*signal(int, void (*)()))();
extern int raise(int);
#ifdef __cplusplus
}
#endif
#ifdef _POSIX_SOURCE
typedef long sigset_t;
struct sigaction {
void (*sa_handler)();
sigset_t sa_mask;
int sa_flags;
};
#define SA_NOCLDSTOP 1
#define SIG_BLOCK 1
#define SIG_UNBLOCK 2
#define SIG_SETMASK 3
#ifdef __cplusplus
extern "C" {
#endif
#ifdef __TYPES_H
extern int kill(pid_t, int);
#endif
extern int sigemptyset(sigset_t *);
extern int sigfillset(sigset_t *);
extern int sigaddset(sigset_t *, int);
extern int sigdelset(sigset_t *, int);
extern int sigismember(const sigset_t *, int);
extern int sigaction(int, const struct sigaction *, struct sigaction *);
extern int sigprocmask(int, sigset_t *, sigset_t *);
extern int sigpending(sigset_t *);
extern int sigsuspend(const sigset_t *);
#ifdef __cplusplus
}
#endif
#endif
#endif