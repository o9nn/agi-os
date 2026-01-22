#ifndef _ASMi386_SIGNAL_H
#define _ASMi386_SIGNAL_H
typedef unsigned long sigset_t;
#define _NSIG             32
#define NSIG		_NSIG
#define SIGHUP		 1
#define SIGINT		 2
#define SIGQUIT		 3
#define SIGILL		 4
#define SIGTRAP		 5
#define SIGABRT		 6
#define SIGIOT		 6
#define SIGBUS		 7
#define SIGFPE		 8
#define SIGKILL		 9
#define SIGUSR1		10
#define SIGSEGV		11
#define SIGUSR2		12
#define SIGPIPE		13
#define SIGALRM		14
#define SIGTERM		15
#define SIGSTKFLT	16
#define SIGCHLD		17
#define SIGCONT		18
#define SIGSTOP		19
#define SIGTSTP		20
#define SIGTTIN		21
#define SIGTTOU		22
#define SIGURG		23
#define SIGXCPU		24
#define SIGXFSZ		25
#define SIGVTALRM	26
#define SIGPROF		27
#define SIGWINCH	28
#define SIGIO		29
#define SIGPOLL		SIGIO
#define SIGPWR		30
#define	SIGUNUSED	31
#define SA_NOCLDSTOP	1
#define SA_SHIRQ	0x04000000
#define SA_STACK	0x08000000
#define SA_RESTART	0x10000000
#define SA_INTERRUPT	0x20000000
#define SA_NOMASK	0x40000000
#define SA_ONESHOT	0x80000000
#ifdef __KERNEL__
#define SA_PROBE SA_ONESHOT
#define SA_SAMPLE_RANDOM SA_RESTART
#endif
#define SIG_BLOCK          0
#define SIG_UNBLOCK        1
#define SIG_SETMASK        2
typedef void (*__sighandler_t)(int);
#define SIG_DFL	((__sighandler_t)0)
#define SIG_IGN	((__sighandler_t)1)
#define SIG_ERR	((__sighandler_t)-1)
struct sigaction {
__sighandler_t sa_handler;
sigset_t sa_mask;
unsigned long sa_flags;
void (*sa_restorer)(void);
};
#ifdef __KERNEL__
#include <asm/sigcontext.h>
#endif
#endif