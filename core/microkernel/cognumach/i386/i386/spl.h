#ifndef _MACHINE_SPL_H_
#define _MACHINE_SPL_H_
typedef int spl_t;
extern spl_t (splhi)(void);
extern spl_t (spl0)(void);
extern spl_t (spl1)(void);
extern spl_t (splsoftclock)(void);
extern spl_t (spl2)(void);
extern spl_t (spl3)(void);
extern spl_t (spl4)(void);
extern spl_t (splnet)(void);
extern spl_t (splhdw)(void);
extern spl_t (spl5)(void);
extern spl_t (splbio)(void);
extern spl_t (spldcm)(void);
extern spl_t (spl6)(void);
extern spl_t (spltty)(void);
extern spl_t (splimp)(void);
extern spl_t (splvm)(void);
#define assert_splvm() assert(splvm() == SPL7)
extern spl_t (spl7)(void);
extern spl_t (splclock)(void);
extern spl_t (splsched)(void);
#define assert_splsched() assert(splsched() == SPL7)
extern spl_t (splhigh)(void);
extern spl_t (splx)(spl_t n);
extern spl_t (splx_cli)(spl_t n);
extern void splon (unsigned long n);
extern unsigned long sploff (void);
extern void setsoftclock (void);
extern int spl_init;
#include <i386/ipl.h>
#endif