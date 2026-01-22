#ifndef _I386_LOCK_H_
#define _I386_LOCK_H_
#define SIMPLE_LOCK_INITIALIZER(l) \
{.lock_data = 0}
#if NCPUS > 1
#include <i386/smp.h>
#ifdef __GNUC__
#define _simple_lock_xchg_(lock, new_val) \
({ natural_t _old_val_; \
asm volatile("xchg %0, %2" \
: "=r" (_old_val_) \
: "0" ((natural_t)(new_val)), "m" (*(lock)) : "memory" \
); \
_old_val_; \
})
#define simple_lock_init(l) \
((l)->lock_data = 0)
#define _simple_lock(l) \
({ \
while(_simple_lock_xchg_(l, 1)) \
while (*(volatile natural_t *)&(l)->lock_data) \
cpu_pause(); \
0; \
})
#define _simple_unlock(l) \
(_simple_lock_xchg_(l, 0))
#define _simple_lock_try(l) \
(!_simple_lock_xchg_(l, 1))
#define bit_lock(bit, l) \
({ \
asm volatile("	jmp	1f	\n\
0: btl %0, %1 \n\
jb 0b \n\
1: lock \n\
btsl %0, %1 \n\
jb 0b" \
: \
: "r" ((int)(bit)), "m" (*(volatile int *)(l)) : "memory"); \
0; \
})
#define bit_unlock(bit, l) \
({ \
asm volatile("	lock		\n\
btrl %0, %1" \
: \
: "r" ((int)(bit)), "m" (*(volatile int *)(l)) : "memory"); \
0; \
})
#define i_bit_set(bit, l) \
({ \
asm volatile("	lock		\n\
btsl %0, %1" \
: \
: "r" ((int)(bit)), "m" (*(l)) ); \
0; \
})
#define i_bit_clear(bit, l) \
({ \
asm volatile("	lock		\n\
btrl %0, %1" \
: \
: "r" ((int)(bit)), "m" (*(l)) ); \
0; \
})
#endif
extern void simple_lock_pause(void);
#endif
#endif