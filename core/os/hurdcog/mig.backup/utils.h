#ifndef	_UTILS_H
#define	_UTILS_H
#include "routine.h"
#define MAX(a,b) \
({ __typeof__ (a) _a = (a); \
__typeof__ (b) _b = (b); \
_a > _b ? _a : _b; })
#define MIN(a,b) \
({ __typeof__ (a) _a = (a); \
__typeof__ (b) _b = (b); \
_a > _b ? _b : _a; })
#define ALIGN(x, alignment) \
((x) + (alignment)-1) & ~((alignment)-1)
typedef void	write_list_fn_t(FILE *file, const argument_t *arg);
extern void WriteImport(FILE *file, const_string_t filename);
extern void WriteRCSDecl(FILE *file, identifier_t name, const_string_t rcs);
extern void WriteBogusDefines(FILE *file);
extern void WriteList(FILE *file, const argument_t *args, write_list_fn_t *func,
u_int mask, const char *between, const char *after);
extern void WriteReverseList(FILE *file, const argument_t *args,
write_list_fn_t *func, u_int mask,
const char *between, const char *after);
extern write_list_fn_t WriteNameDecl;
extern write_list_fn_t WriteUserVarDecl;
extern write_list_fn_t WriteServerVarDecl;
extern write_list_fn_t WriteTypeDeclInServer;
extern write_list_fn_t WriteTypeDeclOutServer;
extern write_list_fn_t WriteTypeDeclInUser;
extern write_list_fn_t WriteTypeDeclOutUser;
extern write_list_fn_t WriteCheckDecl;
extern const char *ReturnTypeStr(const routine_t *rt);
extern const char *FetchUserType(const ipc_type_t *it);
extern const char *FetchServerType(const ipc_type_t *it);
extern const char *ServerVarQualifier(const argument_t *arg);
extern void WriteFieldDeclPrim(FILE *file, const argument_t *arg,
const char *(*tfunc)(const ipc_type_t *it));
extern void WriteStructDecl(FILE *file, const argument_t *args,
write_list_fn_t *func, u_int mask,
const char *name);
extern void WriteStaticDecl(FILE *file, const ipc_type_t *it,
dealloc_t dealloc, bool longform,
bool is_server, bool inname,
identifier_t name);
extern void WriteCopyType(FILE *file, const ipc_type_t *it,
const char *left, const char *right, ...);
extern void WritePackMsgType(FILE *file, const ipc_type_t *it,
dealloc_t dealloc, bool longform,
bool inname, const char *left,
const char *right, ...);
#endif