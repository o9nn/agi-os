#ifndef	_KERN_LOCK_H_
#define	_KERN_LOCK_H_
#include <mach/boolean.h>
#include <mach/machine/vm_types.h>
#include <machine/spl.h>
#include <machine/lock.h>
#if NCPUS > 1
#if MACH_LOCK_MON == 0
#define simple_lock_nocheck	_simple_lock
#define simple_lock_try_nocheck	_simple_lock_try
#define simple_unlock_nocheck	_simple_unlock
#else
#define simple_lock_nocheck	simple_lock
#define simple_lock_try_nocheck	simple_lock_try
#define simple_unlock_nocheck	simple_unlock
#endif
#endif
#define MACH_SLOCKS	((NCPUS > 1) || MACH_LDEBUG)
struct slock {
volatile natural_t lock_data;
struct {} is_a_simple_lock;
};
#define simple_lock_assert(l)	(void) &(l)->is_a_simple_lock
typedef struct slock	simple_lock_data_t;
typedef struct slock	*simple_lock_t;
#if	MACH_SLOCKS
#define	decl_simple_lock_data(class,name) \
class	simple_lock_data_t	name;
#define	def_simple_lock_data(class,name) \
class	simple_lock_data_t	name = SIMPLE_LOCK_INITIALIZER(&name);
#define	def_simple_lock_irq_data(class,name) \
class	simple_lock_irq_data_t	name = { SIMPLE_LOCK_INITIALIZER(&name.lock) };
#define	simple_lock_addr(lock)	(simple_lock_assert(&(lock)),	\
&(lock))
#define	simple_lock_irq_addr(l)	(simple_lock_irq_assert(&(l)),	\
&(l)->lock)
#if	(NCPUS > 1)
#define	simple_lock_taken(lock)		(simple_lock_assert(lock),	\
1)
#define check_simple_locks()
#define check_simple_locks_enable()
#define check_simple_locks_disable()
#else
extern void		simple_lock_init(simple_lock_t);
extern void		_simple_lock(simple_lock_t,
const char *, const char *);
extern void		_simple_unlock(simple_lock_t);
extern boolean_t	_simple_lock_try(simple_lock_t,
const char *, const char *);
#define XSTR(x)		#x
#define STR(x)		XSTR(x)
#define LOCATION	__FILE__ ":" STR(__LINE__)
#define simple_lock_nocheck(lock)	_simple_lock((lock), #lock, LOCATION)
#define simple_lock_try_nocheck(lock)	_simple_lock_try((lock), #lock, LOCATION)
#define simple_unlock_nocheck(lock)	_simple_unlock((lock))
#define simple_lock_pause()
#define simple_lock_taken(lock)		(simple_lock_assert(lock),	\
(lock)->lock_data)
extern void		check_simple_locks(void);
extern void		check_simple_locks_enable(void);
extern void		check_simple_locks_disable(void);
#endif
#else
struct simple_lock_data_empty { struct {} is_a_simple_lock; };
struct simple_lock_irq_data_empty { struct simple_lock_data_empty slock; };
#define	decl_simple_lock_data(class,name)	\
class struct simple_lock_data_empty name;
#define	def_simple_lock_data(class,name)	\
class struct simple_lock_data_empty name;
#define	def_simple_lock_irq_data(class,name)	\
class struct simple_lock_irq_data_empty name;
#define	simple_lock_addr(lock)		(simple_lock_assert(&(lock)),	\
(simple_lock_t)0)
#define	simple_lock_irq_addr(lock)	(simple_lock_irq_assert(&(lock)),	\
(simple_lock_t)0)
#define simple_lock_init(l)	simple_lock_assert(l)
#define simple_lock_nocheck(l)		simple_lock_assert(l)
#define simple_unlock_nocheck(l)	simple_lock_assert(l)
#define simple_lock_try_nocheck(l)	(simple_lock_assert(l),		\
TRUE)
#define simple_lock_taken(l)	(simple_lock_assert(l),		\
1)
#define check_simple_locks()
#define check_simple_locks_enable()
#define check_simple_locks_disable()
#define simple_lock_pause()
#endif
#define decl_mutex_data(class,name)	decl_simple_lock_data(class,name)
#define def_mutex_data(class,name)	def_simple_lock_data(class,name)
#define	mutex_try(l)			simple_lock_try(l)
#define	mutex_lock(l)			simple_lock(l)
#define	mutex_unlock(l)			simple_unlock(l)
#define	mutex_init(l)			simple_lock_init(l)
struct lock {
struct thread	*thread;
unsigned int	read_count:16,
want_upgrade:1,
want_write:1,
waiting:1,
can_sleep:1,
recursion_depth:12,
:0;
#if MACH_LDEBUG
struct thread	*writer;
#endif
decl_simple_lock_data(,interlock)
};
typedef struct lock	lock_data_t;
typedef struct lock	*lock_t;
extern void		lock_init(lock_t, boolean_t);
extern void		lock_sleepable(lock_t, boolean_t);
extern void		lock_write(lock_t);
extern void		lock_read(lock_t);
extern void		lock_done(lock_t);
extern boolean_t	lock_read_to_write(lock_t);
extern void		lock_write_to_read(lock_t);
extern boolean_t	lock_try_write(lock_t);
extern boolean_t	lock_try_read(lock_t);
extern boolean_t	lock_try_read_to_write(lock_t);
#define	lock_read_done(l)	lock_done(l)
#define	lock_write_done(l)	lock_done(l)
extern void		lock_set_recursive(lock_t);
extern void		lock_clear_recursive(lock_t);
#if	! MACH_LDEBUG
#define have_read_lock(l)	1
#define have_write_lock(l)	1
#define lock_check_no_interrupts()
#else
#define have_read_lock(l)	((l)->read_count > 0)
#define have_write_lock(l)	((l)->writer == current_thread())
extern unsigned long in_interrupt[NCPUS];
#define lock_check_no_interrupts()	assert(!in_interrupt[cpu_number()])
#endif
#define have_lock(l)		(have_read_lock(l) || have_write_lock(l))
#if MACH_LOCK_MON == 0
#define simple_lock(l)		\
MACRO_BEGIN \
lock_check_no_interrupts(); \
simple_lock_nocheck(l); \
MACRO_END
#define simple_lock_try(l)	({ \
lock_check_no_interrupts(); \
simple_lock_try_nocheck(l); \
})
#define simple_unlock(l)	\
MACRO_BEGIN \
lock_check_no_interrupts(); \
simple_unlock_nocheck(l); \
MACRO_END
#endif
struct slock_irq {
struct slock slock;
};
#define simple_lock_irq_assert(l)	simple_lock_assert(&(l)->slock)
typedef struct slock_irq	simple_lock_irq_data_t;
typedef struct slock_irq	*simple_lock_irq_t;
#define	decl_simple_lock_irq_data(class,name) \
class	simple_lock_irq_data_t	name;
#define simple_lock_init_irq(l) simple_lock_init(&(l)->slock)
#define simple_lock_irq(l)	({ \
spl_t __s = splhigh(); \
simple_lock_nocheck(&(l)->slock); \
__s; \
})
#define simple_unlock_irq(s, l)	\
MACRO_BEGIN \
simple_unlock_nocheck(&(l)->slock); \
splx(s); \
MACRO_END
#if	MACH_KDB
extern void db_show_all_slocks(void);
#endif
extern void lip(void);
#endif