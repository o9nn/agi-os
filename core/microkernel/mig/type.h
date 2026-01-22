#ifndef	_TYPE_H
#define	_TYPE_H
#include <stdbool.h>
#include <sys/types.h>
#include "mig_string.h"
typedef u_int ipc_flags_t;
#define	flNone		(0x00)
#define	flLong		(0x01)
#define	flNotLong	(0x02)
#define	flDealloc	(0x04)
#define	flNotDealloc	(0x08)
#define	flMaybeDealloc	(0x10)
#define	flServerCopy	(0x20)
#define	flCountInOut	(0x40)
typedef enum dealloc {
d_NO,
d_YES,
d_MAYBE
} dealloc_t;
#define	strdealloc(d)	(strbool(d == d_YES))
typedef struct ipc_type
{
identifier_t itName;
struct ipc_type *itNext;
u_int itTypeSize;
u_int itPadSize;
u_int itMinTypeSize;
u_int itAlignment;
u_int itInName;
u_int itOutName;
u_int itSize;
u_int itNumber;
bool itInLine;
bool itLongForm;
dealloc_t itDeallocate;
const_string_t itInNameStr;
const_string_t itOutNameStr;
ipc_flags_t itFlags;
bool itStruct;
bool itString;
bool itVarArray;
bool itIndefinite;
bool itUserlandPort;
bool itKernelPort;
struct ipc_type *itElement;
identifier_t itUserType;
identifier_t itServerType;
identifier_t itTransType;
identifier_t itInTrans;
identifier_t itInTransPayload;
identifier_t itOutTrans;
identifier_t itDestructor;
} ipc_type_t;
#define	itNULL		((ipc_type_t *) 0)
extern ipc_type_t *itLookUp(identifier_t name);
extern void itInsert(identifier_t name, ipc_type_t *it);
extern void itTypeDecl(identifier_t name, ipc_type_t *it);
extern ipc_type_t *itShortDecl(u_int inname, const_string_t instr,
u_int outname, const_string_t outstr,
u_int defsize);
extern ipc_type_t *itLongDecl(u_int inname, const_string_t instr,
u_int outname, const_string_t outstr,
u_int defsize, u_int size, ipc_flags_t flags);
extern ipc_type_t *itPrevDecl(identifier_t name);
extern ipc_type_t *itResetType(ipc_type_t *it);
extern ipc_type_t *itVarArrayDecl(u_int number, const ipc_type_t *it);
extern ipc_type_t *itArrayDecl(u_int number, const ipc_type_t *it);
extern ipc_type_t *itPtrDecl(ipc_type_t *it);
extern ipc_type_t *itStructArrayDecl(u_int number, const ipc_type_t *it);
extern ipc_type_t *itStructDecl(u_int min_type_size_in_bytes, u_int required_alignment_in_bytes);
extern ipc_type_t *itCStringDecl(u_int number, bool varying);
extern ipc_type_t *itRetCodeType;
extern ipc_type_t *itDummyType;
extern ipc_type_t *itRequestPortType;
extern ipc_type_t *itZeroReplyPortType;
extern ipc_type_t *itRealReplyPortType;
extern ipc_type_t *itWaitTimeType;
extern ipc_type_t *itMsgOptionType;
extern ipc_type_t *itMakeCountType(void);
extern ipc_type_t *itMakePolyType(void);
extern ipc_type_t *itMakeDeallocType(void);
extern void init_type(void);
extern void itCheckReturnType(identifier_t name, const ipc_type_t *it);
extern void itCheckRequestPortType(identifier_t name, const ipc_type_t *it);
extern void itCheckReplyPortType(identifier_t name, const ipc_type_t *it);
extern void itCheckIntType(identifier_t name, const ipc_type_t *it);
extern void itCheckNaturalType(identifier_t name, ipc_type_t *it);
extern ipc_flags_t itCheckFlags(ipc_flags_t flags, identifier_t name);
extern dealloc_t itCheckDeallocate(const ipc_type_t *it, ipc_flags_t flags,
dealloc_t dfault, identifier_t name);
extern bool itCheckIsLong(const ipc_type_t *it, ipc_flags_t flags,
bool dfault, identifier_t name);
#endif