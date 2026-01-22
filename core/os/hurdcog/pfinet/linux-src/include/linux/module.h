#ifndef _LINUX_MODULE_H
#define _LINUX_MODULE_H
#include <linux/config.h>
#ifdef __GENKSYMS__
#  define _set_ver(sym) sym
#  undef  MODVERSIONS
#  define MODVERSIONS
#else
# if !defined(MODVERSIONS) && defined(EXPORT_SYMTAB)
#   define _set_ver(sym) sym
#   include <linux/modversions.h>
# endif
#endif
#include <asm/atomic.h>
struct exception_table_entry;
struct kernel_sym
{
unsigned long value;
char name[60];
};
struct module_symbol
{
unsigned long value;
const char *name;
};
struct module_ref
{
struct module *dep;
struct module *ref;
struct module_ref *next_ref;
};
struct module_persist;
struct module
{
unsigned long size_of_struct;
struct module *next;
const char *name;
unsigned long size;
union
{
atomic_t usecount;
long pad;
} uc;
unsigned long flags;
unsigned nsyms;
unsigned ndeps;
struct module_symbol *syms;
struct module_ref *deps;
struct module_ref *refs;
int (*init)(void);
void (*cleanup)(void);
const struct exception_table_entry *ex_table_start;
const struct exception_table_entry *ex_table_end;
#ifdef __alpha__
unsigned long gp;
#endif
const struct module_persist *persist_start;
const struct module_persist *persist_end;
int (*can_unload)(void);
};
struct module_info
{
unsigned long addr;
unsigned long size;
unsigned long flags;
long usecount;
};
#define MOD_UNINITIALIZED	0
#define MOD_RUNNING		1
#define MOD_DELETED		2
#define MOD_AUTOCLEAN		4
#define MOD_VISITED  		8
#define MOD_USED_ONCE		16
#define MOD_JUST_FREED		32
#define QM_MODULES	1
#define QM_DEPS		2
#define QM_REFS		3
#define QM_SYMBOLS	4
#define QM_INFO		5
#define mod_member_present(mod,member) 					\
((unsigned long)(&((struct module *)0L)->member + 1)		\
<= (mod)->size_of_struct)
#define GET_USE_COUNT(module)	(atomic_read(&(module)->uc.usecount))
#define __MOD_INC_USE_COUNT(mod)					\
(atomic_inc(&(mod)->uc.usecount), (mod)->flags |= MOD_VISITED|MOD_USED_ONCE)
#define __MOD_DEC_USE_COUNT(mod)					\
(atomic_dec(&(mod)->uc.usecount), (mod)->flags |= MOD_VISITED)
#define __MOD_IN_USE(mod)						\
(mod_member_present((mod), can_unload) && (mod)->can_unload	\
? (mod)->can_unload() : atomic_read(&(mod)->uc.usecount))
#define __MODULE_STRING_1(x)	#x
#define __MODULE_STRING(x)	__MODULE_STRING_1(x)
extern unsigned long get_module_symbol(char *, char *);
#if defined(MODULE) && !defined(__GENKSYMS__)
#define MODULE_AUTHOR(name)						   \
const char __module_author[] __attribute__((section(".modinfo"))) = 	   \
"author=" name
#define MODULE_DESCRIPTION(desc)					   \
const char __module_description[] __attribute__((section(".modinfo"))) =   \
"description=" desc
#define MODULE_SUPPORTED_DEVICE(dev)					   \
const char __module_device[] __attribute__((section(".modinfo"))) = 	   \
"device=" dev
#define MODULE_PARM(var,type)			\
const char __module_parm_##var[]		\
__attribute__((section(".modinfo"))) =		\
"parm_" __MODULE_STRING(var) "=" type
#define MODULE_PARM_DESC(var,desc)		\
const char __module_parm_desc_##var[]		\
__attribute__((section(".modinfo"))) =		\
"parm_desc_" __MODULE_STRING(var) "=" desc
__asm__(".section .modinfo\n\t.previous");
extern struct module __this_module;
#define MOD_INC_USE_COUNT	__MOD_INC_USE_COUNT(&__this_module)
#define MOD_DEC_USE_COUNT	__MOD_DEC_USE_COUNT(&__this_module)
#define MOD_IN_USE		__MOD_IN_USE(&__this_module)
#ifndef __NO_VERSION__
#include <linux/version.h>
const char __module_kernel_version[] __attribute__((section(".modinfo"))) =
"kernel_version=" UTS_RELEASE;
#ifdef MODVERSIONS
const char __module_using_checksums[] __attribute__((section(".modinfo"))) =
"using_checksums=1";
#endif
#endif
#else
#define MODULE_AUTHOR(name)
#define MODULE_DESCRIPTION(desc)
#define MODULE_SUPPORTED_DEVICE(name)
#define MODULE_PARM(var,type)
#define MODULE_PARM_DESC(var,desc)
#ifndef __GENKSYMS__
#define MOD_INC_USE_COUNT	do { } while (0)
#define MOD_DEC_USE_COUNT	do { } while (0)
#define MOD_IN_USE		1
extern struct module *module_list;
#endif
#endif
#if defined(__GENKSYMS__)
#elif !defined(AUTOCONF_INCLUDED)
#define __EXPORT_SYMBOL(sym,str)   error config_must_be_included_before_module
#define EXPORT_SYMBOL(var)	   error config_must_be_included_before_module
#define EXPORT_SYMBOL_NOVERS(var)  error config_must_be_included_before_module
#elif !defined(CONFIG_MODULES)
#define __EXPORT_SYMBOL(sym,str)
#define EXPORT_SYMBOL(var)
#define EXPORT_SYMBOL_NOVERS(var)
#elif !defined(EXPORT_SYMTAB)
#define __EXPORT_SYMBOL(sym,str)   error EXPORT_SYMTAB_not_defined
#define EXPORT_SYMBOL(var)	   error EXPORT_SYMTAB_not_defined
#define EXPORT_SYMBOL_NOVERS(var)  error EXPORT_SYMTAB_not_defined
#else
#define __EXPORT_SYMBOL(sym, str)			\
const char __kstrtab_##sym[]				\
__attribute__((section(".kstrtab"))) = str;		\
const struct module_symbol __ksymtab_##sym 		\
__attribute__((section("__ksymtab"))) =			\
{ (unsigned long)&sym, __kstrtab_##sym }
#if defined(MODVERSIONS) || !defined(CONFIG_MODVERSIONS)
#define EXPORT_SYMBOL(var)  __EXPORT_SYMBOL(var, __MODULE_STRING(var))
#else
#define EXPORT_SYMBOL(var)  __EXPORT_SYMBOL(var, __MODULE_STRING(__VERSIONED_SYMBOL(var)))
#endif
#define EXPORT_SYMBOL_NOVERS(var)  __EXPORT_SYMBOL(var, __MODULE_STRING(var))
#endif
#ifdef MODULE
#define EXPORT_NO_SYMBOLS  __asm__(".section __ksymtab\n.previous")
#else
#define EXPORT_NO_SYMBOLS
#endif
#endif