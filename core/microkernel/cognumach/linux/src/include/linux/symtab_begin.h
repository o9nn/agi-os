#include <linux/linkage.h>
#ifdef MODVERSIONS
# undef _set_ver
# undef X
# define _basic_version(symaddr,symstr)  symaddr, symstr
# define _alias_version(really,symaddr,symstr)  (void *) & really , symstr
# ifndef __GENKSYMS__
#  ifdef MODULE
#    define _set_ver(sym,ver) \
(void *) & sym ## _R ## ver, SYMBOL_NAME_STR(sym) "_R" #ver
#  else
#    define _set_ver(sym,ver) \
(void *) & sym, SYMBOL_NAME_STR(sym) "_R" #ver
#  endif
#  define X(sym) { _basic_version(sym) }
#  define Xalias(really,sym) { _alias_version(really,sym) }
# endif
#else
# define X(sym) { (void *) & sym, SYMBOL_NAME_STR(sym)}
# define Xalias(really,sym) { (void *) & really, SYMBOL_NAME_STR(sym)}
#endif
#define XNOVERS(sym) { (void *) & sym, SYMBOL_NAME_STR(sym)}
#define EMPTY {0,0}
0, 0, 0, {