#ifdef MODVERSIONS
#undef _set_ver
#if defined(MODULE) && !defined(__GENKSYMS__)
#define _set_ver(sym,vers) sym ## _R ## vers
#else
#define _set_ver(a,b) a
#endif
#endif
#undef X
#undef EMPTY
{ (void *)0, (char *)0 }
},
{ { (struct module *)0, (struct module_ref *)0 } }