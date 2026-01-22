#include <linux/config.h>
struct pt_regs;
struct kbd_struct;
struct tty_struct;
void handle_sysrq(int, struct pt_regs *, struct kbd_struct *, struct tty_struct *);
extern int emergency_sync_scheduled;
#define EMERG_SYNC 1
#define EMERG_REMOUNT 2
void do_emergency_sync(void);
#ifdef CONFIG_MAGIC_SYSRQ
#define CHECK_EMERGENCY_SYNC			\
if (emergency_sync_scheduled)		\
do_emergency_sync();
#else
#define CHECK_EMERGENCY_SYNC
#endif
extern int sysrq_enabled;