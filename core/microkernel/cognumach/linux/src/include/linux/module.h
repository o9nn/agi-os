#ifndef _LINUX_MODULE_H
#define _LINUX_MODULE_H
#ifdef __GENKSYMS__
#  define _set_ver(sym,vers) sym
#  undef  MODVERSIONS
#  define MODVERSIONS
#else
# if defined(MODVERSIONS) && !defined(MODULE) && defined(EXPORT_SYMTAB)
#   define _set_ver(sym,vers) sym
#   include <linux/modversions.h>
# endif
#endif
#define MOD_UNINITIALIZED 0
#define MOD_RUNNING 1
#define MOD_DELETED 2
#define MOD_MAX_NAME 64
#define MOD_AUTOCLEAN 0x40000000
#define MOD_VISITED   0x20000000
#define SYM_MAX_NAME 60
struct kernel_sym {
unsigned long value;
char name[SYM_MAX_NAME];
};
struct module_ref {
struct module *module;
struct module_ref *next;
};
struct internal_symbol {
void *addr;
const char *name;
};
struct symbol_table {
int size;
int n_symbols;
int n_refs;
struct internal_symbol symbol[0];
struct module_ref ref[0];
};
struct module {
struct module *next;
struct module_ref *ref;
struct symbol_table *symtab;
const char *name;
int size;
void *addr;
int state;
void (*cleanup)(void);
};
struct mod_routines {
int (*init)(void);
void (*cleanup)(void);
};
#define GET_USE_COUNT(module)	(* (long *) (module)->addr)
#ifdef MODULE
extern long mod_use_count_;
#define MOD_INC_USE_COUNT      (mod_use_count_++, mod_use_count_ |= MOD_VISITED)
#define MOD_DEC_USE_COUNT      (mod_use_count_--, mod_use_count_ |= MOD_VISITED)
#define MOD_IN_USE	       ((mod_use_count_ & ~(MOD_AUTOCLEAN | MOD_VISITED)) != 0)
#ifndef __NO_VERSION__
#include <linux/version.h>
char kernel_version[]=UTS_RELEASE;
#endif
#if defined(MODVERSIONS) && !defined(__GENKSYMS__)
int Using_Versions;
#endif
#else
#define EXPORT_SYMBOL(sym)
#define MOD_INC_USE_COUNT	do { } while (0)
#define MOD_DEC_USE_COUNT	do { } while (0)
#define MOD_IN_USE		1
#define SET_MODULE_OWNER(dev)	do{ } while(0)
#endif
#define register_symtab(symtab)
#endif