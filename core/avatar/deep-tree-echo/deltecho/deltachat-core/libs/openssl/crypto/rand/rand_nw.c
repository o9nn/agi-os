#include "cryptlib.h"
#include <openssl/rand.h>
#include "rand_lcl.h"
#if defined (OPENSSL_SYS_NETWARE)
# if defined(NETWARE_LIBC)
#  include <nks/thread.h>
# else
#  include <nwthread.h>
# endif
extern int GetProcessSwitchCount(void);
# if !defined(NETWARE_LIBC) || (CURRENT_NDK_THRESHOLD < 509220000)
extern void *RunningProcess;
extern unsigned long GetSuperHighResolutionTimer(void);
# endif
int RAND_poll(void)
{
unsigned long l;
unsigned long tsc;
int i;
l = GetProcessSwitchCount();
RAND_add(&l, sizeof(l), 1);
l = (unsigned long)RunningProcess;
RAND_add(&l, sizeof(l), 1);
for (i = 2; i < ENTROPY_NEEDED; i++) {
# ifdef __MWERKS__
asm {
rdtsc mov tsc, eax}
# elif defined(__GNUC__) && __GNUC__>=2 && !defined(OPENSSL_NO_ASM) && !defined(OPENSSL_NO_INLINE_ASM)
asm volatile ("rdtsc":"=a" (tsc)::"edx");
# endif
RAND_add(&tsc, sizeof(tsc), 1);
l = GetSuperHighResolutionTimer();
RAND_add(&l, sizeof(l), 0);
# if defined(NETWARE_LIBC)
NXThreadYield();
# else
ThreadSwitchWithDelay();
# endif
}
return 1;
}
#endif