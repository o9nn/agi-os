#ifdef HAVE_CYTHON
#ifdef _GNU_SOURCE
#pragma push_macro("_POSIX_C_SOURCE")
#pragma push_macro("_XOPEN_SOURCE")
#undef _POSIX_C_SOURCE
#undef _XOPEN_SOURCE
#endif
#include <Python.h>
#include <frameobject.h>
#ifdef _GNU_SOURCE
#pragma pop_macro("_POSIX_C_SOURCE")
#pragma pop_macro("_XOPEN_SOURCE")
#endif
#endif