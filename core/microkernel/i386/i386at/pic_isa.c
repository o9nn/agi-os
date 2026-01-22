#include <sys/types.h>
#include <i386/ipl.h>
#include <i386/pic.h>
#include <i386/fpu.h>
#include <i386/hardclock.h>
#include <i386at/kd.h>
interrupt_handler_fn ivect[NINTR] = {
(interrupt_handler_fn)hardclock,
kdintr,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
fpintr,
intnull,
intnull,
};