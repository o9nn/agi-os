#ifndef _GNU_SOURCE
# define _GNU_SOURCE
#endif
#include <stdio.h>
#include "cryptlib.h"
#include <openssl/dso.h>
#ifndef DSO_DLFCN
DSO_METHOD *DSO_METHOD_dlfcn(void)
{
return NULL;
}
#else
# ifdef HAVE_DLFCN_H
# ifdef __osf__
# define __EXTENSIONS__
# endif
# include <dlfcn.h>
# define HAVE_DLINFO 1
# if defined(_AIX) || defined(__CYGWIN__) || \
defined(__SCO_VERSION__) || defined(_SCO_ELF) || \
(defined(__osf__) && !defined(RTLD_NEXT)) || \
(defined(__OpenBSD__) && !defined(RTLD_SELF)) || \
defined(__ANDROID__)
# undef HAVE_DLINFO
# endif
# endif
# define DSO_MAX_TRANSLATED_SIZE 256
static int dlfcn_load(DSO *dso);
static int dlfcn_unload(DSO *dso);
static void *dlfcn_bind_var(DSO *dso, const char *symname);
static DSO_FUNC_TYPE dlfcn_bind_func(DSO *dso, const char *symname);
# if 0
static int dlfcn_unbind(DSO *dso, char *symname, void *symptr);
static int dlfcn_init(DSO *dso);
static int dlfcn_finish(DSO *dso);
static long dlfcn_ctrl(DSO *dso, int cmd, long larg, void *parg);
# endif
static char *dlfcn_name_converter(DSO *dso, const char *filename);
static char *dlfcn_merger(DSO *dso, const char *filespec1,
const char *filespec2);
static int dlfcn_pathbyaddr(void *addr, char *path, int sz);
static void *dlfcn_globallookup(const char *name);
static DSO_METHOD dso_meth_dlfcn = {
"OpenSSL 'dlfcn' shared library method",
dlfcn_load,
dlfcn_unload,
dlfcn_bind_var,
dlfcn_bind_func,
# if 0
NULL,
NULL,
# endif
NULL,
dlfcn_name_converter,
dlfcn_merger,
NULL,
NULL,
dlfcn_pathbyaddr,
dlfcn_globallookup
};
DSO_METHOD *DSO_METHOD_dlfcn(void)
{
return (&dso_meth_dlfcn);
}
# if defined(__OpenBSD__) || defined(__NetBSD__)
# ifdef DL_LAZY
# define DLOPEN_FLAG DL_LAZY
# else
# ifdef RTLD_NOW
# define DLOPEN_FLAG RTLD_NOW
# else
# define DLOPEN_FLAG 0
# endif
# endif
# else
# ifdef OPENSSL_SYS_SUNOS
# define DLOPEN_FLAG 1
# else
# define DLOPEN_FLAG RTLD_NOW
# endif
# endif
static int dlfcn_load(DSO *dso)
{
void *ptr = NULL;
char *filename = DSO_convert_filename(dso, NULL);
int flags = DLOPEN_FLAG;
if (filename == NULL) {
DSOerr(DSO_F_DLFCN_LOAD, DSO_R_NO_FILENAME);
goto err;
}
# ifdef RTLD_GLOBAL
if (dso->flags & DSO_FLAG_GLOBAL_SYMBOLS)
flags |= RTLD_GLOBAL;
# endif
ptr = dlopen(filename, flags);
if (ptr == NULL) {
DSOerr(DSO_F_DLFCN_LOAD, DSO_R_LOAD_FAILED);
ERR_add_error_data(4, "filename(", filename, "): ", dlerror());
goto err;
}
if (!sk_void_push(dso->meth_data, (char *)ptr)) {
DSOerr(DSO_F_DLFCN_LOAD, DSO_R_STACK_ERROR);
goto err;
}
dso->loaded_filename = filename;
return (1);
err:
if (filename != NULL)
OPENSSL_free(filename);
if (ptr != NULL)
dlclose(ptr);
return (0);
}
static int dlfcn_unload(DSO *dso)
{
void *ptr;
if (dso == NULL) {
DSOerr(DSO_F_DLFCN_UNLOAD, ERR_R_PASSED_NULL_PARAMETER);
return (0);
}
if (sk_void_num(dso->meth_data) < 1)
return (1);
ptr = sk_void_pop(dso->meth_data);
if (ptr == NULL) {
DSOerr(DSO_F_DLFCN_UNLOAD, DSO_R_NULL_HANDLE);
sk_void_push(dso->meth_data, ptr);
return (0);
}
dlclose(ptr);
return (1);
}
static void *dlfcn_bind_var(DSO *dso, const char *symname)
{
void *ptr, *sym;
if ((dso == NULL) || (symname == NULL)) {
DSOerr(DSO_F_DLFCN_BIND_VAR, ERR_R_PASSED_NULL_PARAMETER);
return (NULL);
}
if (sk_void_num(dso->meth_data) < 1) {
DSOerr(DSO_F_DLFCN_BIND_VAR, DSO_R_STACK_ERROR);
return (NULL);
}
ptr = sk_void_value(dso->meth_data, sk_void_num(dso->meth_data) - 1);
if (ptr == NULL) {
DSOerr(DSO_F_DLFCN_BIND_VAR, DSO_R_NULL_HANDLE);
return (NULL);
}
sym = dlsym(ptr, symname);
if (sym == NULL) {
DSOerr(DSO_F_DLFCN_BIND_VAR, DSO_R_SYM_FAILURE);
ERR_add_error_data(4, "symname(", symname, "): ", dlerror());
return (NULL);
}
return (sym);
}
static DSO_FUNC_TYPE dlfcn_bind_func(DSO *dso, const char *symname)
{
void *ptr;
union {
DSO_FUNC_TYPE sym;
void *dlret;
} u;
if ((dso == NULL) || (symname == NULL)) {
DSOerr(DSO_F_DLFCN_BIND_FUNC, ERR_R_PASSED_NULL_PARAMETER);
return (NULL);
}
if (sk_void_num(dso->meth_data) < 1) {
DSOerr(DSO_F_DLFCN_BIND_FUNC, DSO_R_STACK_ERROR);
return (NULL);
}
ptr = sk_void_value(dso->meth_data, sk_void_num(dso->meth_data) - 1);
if (ptr == NULL) {
DSOerr(DSO_F_DLFCN_BIND_FUNC, DSO_R_NULL_HANDLE);
return (NULL);
}
u.dlret = dlsym(ptr, symname);
if (u.dlret == NULL) {
DSOerr(DSO_F_DLFCN_BIND_FUNC, DSO_R_SYM_FAILURE);
ERR_add_error_data(4, "symname(", symname, "): ", dlerror());
return (NULL);
}
return u.sym;
}
static char *dlfcn_merger(DSO *dso, const char *filespec1,
const char *filespec2)
{
char *merged;
if (!filespec1 && !filespec2) {
DSOerr(DSO_F_DLFCN_MERGER, ERR_R_PASSED_NULL_PARAMETER);
return (NULL);
}
if (!filespec2 || (filespec1 != NULL && filespec1[0] == '/')) {
merged = OPENSSL_malloc(strlen(filespec1) + 1);
if (!merged) {
DSOerr(DSO_F_DLFCN_MERGER, ERR_R_MALLOC_FAILURE);
return (NULL);
}
strcpy(merged, filespec1);
}
else if (!filespec1) {
merged = OPENSSL_malloc(strlen(filespec2) + 1);
if (!merged) {
DSOerr(DSO_F_DLFCN_MERGER, ERR_R_MALLOC_FAILURE);
return (NULL);
}
strcpy(merged, filespec2);
} else {
int spec2len, len;
spec2len = strlen(filespec2);
len = spec2len + strlen(filespec1);
if (spec2len && filespec2[spec2len - 1] == '/') {
spec2len--;
len--;
}
merged = OPENSSL_malloc(len + 2);
if (!merged) {
DSOerr(DSO_F_DLFCN_MERGER, ERR_R_MALLOC_FAILURE);
return (NULL);
}
strcpy(merged, filespec2);
merged[spec2len] = '/';
strcpy(&merged[spec2len + 1], filespec1);
}
return (merged);
}
# ifdef OPENSSL_SYS_MACOSX
# define DSO_ext ".dylib"
# define DSO_extlen 6
# else
# define DSO_ext ".so"
# define DSO_extlen 3
# endif
static char *dlfcn_name_converter(DSO *dso, const char *filename)
{
char *translated;
int len, rsize, transform;
len = strlen(filename);
rsize = len + 1;
transform = (strstr(filename, "/") == NULL);
if (transform) {
rsize += DSO_extlen;
if ((DSO_flags(dso) & DSO_FLAG_NAME_TRANSLATION_EXT_ONLY) == 0)
rsize += 3;
}
translated = OPENSSL_malloc(rsize);
if (translated == NULL) {
DSOerr(DSO_F_DLFCN_NAME_CONVERTER, DSO_R_NAME_TRANSLATION_FAILED);
return (NULL);
}
if (transform) {
if ((DSO_flags(dso) & DSO_FLAG_NAME_TRANSLATION_EXT_ONLY) == 0)
sprintf(translated, "lib%s" DSO_ext, filename);
else
sprintf(translated, "%s" DSO_ext, filename);
} else
sprintf(translated, "%s", filename);
return (translated);
}
# ifdef __sgi
# include <rld_interface.h>
# ifndef _RLD_INTERFACE_DLFCN_H_DLADDR
# define _RLD_INTERFACE_DLFCN_H_DLADDR
typedef struct Dl_info {
const char *dli_fname;
void *dli_fbase;
const char *dli_sname;
void *dli_saddr;
int dli_version;
int dli_reserved1;
long dli_reserved[4];
} Dl_info;
# else
typedef struct Dl_info Dl_info;
# endif
# define _RLD_DLADDR 14
static int dladdr(void *address, Dl_info *dl)
{
void *v;
v = _rld_new_interface(_RLD_DLADDR, address, dl);
return (int)v;
}
# endif
static int dlfcn_pathbyaddr(void *addr, char *path, int sz)
{
# ifdef HAVE_DLINFO
Dl_info dli;
int len;
if (addr == NULL) {
union {
int (*f) (void *, char *, int);
void *p;
} t = {
dlfcn_pathbyaddr
};
addr = t.p;
}
if (dladdr(addr, &dli)) {
len = (int)strlen(dli.dli_fname);
if (sz <= 0)
return len + 1;
if (len >= sz)
len = sz - 1;
memcpy(path, dli.dli_fname, len);
path[len++] = 0;
return len;
}
ERR_add_error_data(2, "dlfcn_pathbyaddr(): ", dlerror());
# endif
return -1;
}
static void *dlfcn_globallookup(const char *name)
{
void *ret = NULL, *handle = dlopen(NULL, RTLD_LAZY);
if (handle) {
ret = dlsym(handle, name);
dlclose(handle);
}
return ret;
}
#endif