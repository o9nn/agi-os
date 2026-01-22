#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "error.h"
#include "global.h"
#include "type.h"
#include "message.h"
#include "cpu.h"
#include "utils.h"
#define int_name MACH_MSG_TYPE_INTEGER_32
#define int_name_string "MACH_MSG_TYPE_INTEGER_32"
ipc_type_t *itByteType;
ipc_type_t *itRetCodeType;
ipc_type_t *itDummyType;
ipc_type_t *itRequestPortType;
ipc_type_t *itZeroReplyPortType;
ipc_type_t *itRealReplyPortType;
ipc_type_t *itWaitTimeType;
ipc_type_t *itMsgOptionType;
ipc_type_t *itShortType;
ipc_type_t *itIntType;
ipc_type_t *itInt64Type;
ipc_type_t *itUintPtrType;
ipc_type_t *itIntPtrType;
static bool types_initialized = false;
static ipc_type_t *list = itNULL;
ipc_type_t *
itLookUp(identifier_t name)
{
if (!types_initialized)
{
error("Basic types not initialized when looking up type %s. Did you "
"forget to define the subsystem?", name);
return NULL;
}
ipc_type_t *it, **last;
for (it = *(last = &list); it != itNULL; it = *(last = &it->itNext))
if (streql(name, it->itName))
{
*last = it->itNext;
it->itNext = list;
list = it;
return it;
}
return itNULL;
}
void
itInsert(identifier_t name, ipc_type_t *it)
{
it->itName = name;
it->itNext = list;
list = it;
}
static ipc_type_t *
itAlloc(void)
{
static const ipc_type_t prototype =
{
strNULL,
0,
0,
0,
0,
0,
0,
0,
0,
1,
true,
false,
d_NO,
strNULL,
strNULL,
flNone,
true,
false,
false,
false,
false,
false,
itNULL,
strNULL,
strNULL,
strNULL,
strNULL,
strNULL,
strNULL,
strNULL,
};
ipc_type_t *new;
new = malloc(sizeof *new);
if (new == itNULL)
fatal("itAlloc(): %s", unix_error_string(errno));
*new = prototype;
return new;
}
static string_t
itNameToString(u_int name)
{
char buffer[100];
(void) sprintf(buffer, "%u", name);
return strmake(buffer);
}
static void
itCalculateSizeInfo(ipc_type_t *it)
{
assert(it->itAlignment > 0);
if (it->itInLine)
{
u_int bytes = (it->itNumber * it->itSize + 7) / 8;
u_int padding = (complex_alignof - bytes % complex_alignof) % complex_alignof;
it->itTypeSize = bytes;
it->itPadSize = padding;
if (it->itVarArray)
it->itMinTypeSize = 0;
else
it->itMinTypeSize = bytes + padding;
}
else
{
u_int bytes = sizeof_pointer;
it->itTypeSize = bytes;
it->itPadSize = 0;
it->itMinTypeSize = bytes;
assert(it->itAlignment == bytes);
}
if ((it->itNumber * it->itSize) % 8 != 0)
warn("size of C types must be multiples of 8 bits");
if ((it->itTypeSize == 0) && !it->itVarArray)
warn("sizeof(%s) == 0", it->itName);
}
static bool
itIsPortType(ipc_type_t *it)
{
return ((it->itInName == MACH_MSG_TYPE_POLYMORPHIC) &&
(it->itOutName == MACH_MSG_TYPE_POLYMORPHIC)) ||
MACH_MSG_TYPE_PORT_ANY(it->itInName) ||
MACH_MSG_TYPE_PORT_ANY(it->itOutName);
}
static void
itCalculateNameInfo(ipc_type_t *it)
{
if (it->itInNameStr == strNULL)
it->itInNameStr = strmake(itNameToString(it->itInName));
if (it->itOutNameStr == strNULL)
it->itOutNameStr = strmake(itNameToString(it->itOutName));
if (it->itUserType == strNULL)
it->itUserType = it->itName;
if (it->itServerType == strNULL)
it->itServerType = it->itName;
bool isPortType = itIsPortType(it);
bool isServerPort = isPortType && streql(it->itServerType, "mach_port_t");
bool isUserPort = isPortType && streql(it->itUserType, "mach_port_t");
if (IsKernelServer && isServerPort) {
it->itServerType = "ipc_port_t";
it->itKernelPort = true;
} else if (IsKernelUser && isUserPort) {
it->itUserType = "ipc_port_t";
it->itKernelPort = true;
} else
it->itKernelPort = false;
it->itUserlandPort = isPortType && !IsKernelUser && !IsKernelServer;
if (it->itTransType == strNULL)
it->itTransType = it->itServerType;
}
ipc_flags_t
itCheckFlags(ipc_flags_t flags, identifier_t name)
{
if ((flags&(flLong|flNotLong)) == (flLong|flNotLong)) {
warn("%s: IsLong and IsNotLong cancel out", name);
flags &= ~(flLong|flNotLong);
}
if (flags & flMaybeDealloc) {
if (flags & (flDealloc|flNotDealloc)) {
warn("%s: Dealloc and NotDealloc ignored with Dealloc[]", name);
flags &= ~(flDealloc|flNotDealloc);
}
}
if ((flags&(flDealloc|flNotDealloc)) == (flDealloc|flNotDealloc)) {
warn("%s: Dealloc and NotDealloc cancel out", name);
flags &= ~(flDealloc|flNotDealloc);
}
return flags;
}
dealloc_t
itCheckDeallocate(const ipc_type_t *it, ipc_flags_t flags, dealloc_t dfault,
identifier_t name)
{
dealloc_t dealloc = dfault;
if (flags & flMaybeDealloc)
dealloc = d_MAYBE;
if (flags & flDealloc)
dealloc = d_YES;
if (flags & flNotDealloc)
dealloc = d_NO;
if (dealloc == dfault) {
if (flags & flMaybeDealloc)
warn("%s: Dealloc[] is redundant", name);
if (flags & flDealloc)
warn("%s: Dealloc is redundant", name);
if (flags & flNotDealloc)
warn("%s: NotDealloc is redundant", name);
}
if (flags & (flMaybeDealloc|flDealloc|flNotDealloc)) {
if (dealloc != d_NO) {
if (it->itInLine && !it->itIndefinite)
warn("%s: Dealloc will cause an IPC error", name);
}
}
return dealloc;
}
static enum uselong { NotLong, CanBeLong, ShouldBeLong, MustBeLong, TooLong }
itUseLong(const ipc_type_t *it)
{
enum uselong uselong = NotLong;
if ((it->itInName == MACH_MSG_TYPE_POLYMORPHIC) ||
(it->itOutName == MACH_MSG_TYPE_POLYMORPHIC) ||
it->itVarArray)
uselong = CanBeLong;
if ((it->itVarArray && !it->itInLine) || it->itIndefinite)
uselong = ShouldBeLong;
if (((it->itInName != MACH_MSG_TYPE_POLYMORPHIC) &&
(it->itInName >= (1<<8))) ||
((it->itOutName != MACH_MSG_TYPE_POLYMORPHIC) &&
(it->itOutName >= (1<<8)))) {
error("Cannot have msgt_name greater than 255");
uselong = TooLong;
}
if ((it->itSize >= (1<<8)) ||
(it->itNumber >= (1<<12)))
uselong = MustBeLong;
if (it->itSize >= (1<<16))
uselong = TooLong;
return uselong;
}
bool
itCheckIsLong(const ipc_type_t *it, ipc_flags_t flags, bool dfault,
identifier_t name)
{
bool islong = dfault;
if (flags & flLong)
islong = true;
if (flags & flNotLong)
islong = false;
if (islong == dfault) {
if (flags & flLong)
warn("%s: IsLong is redundant", name);
if (flags & flNotLong)
warn("%s: IsNotLong is redundant", name);
}
if (flags & (flLong|flNotLong)) {
enum uselong uselong = itUseLong(it);
if (islong && ((int)uselong <= (int)NotLong))
warn("%s: doesn't need IsLong", name);
if (!islong && ((int)uselong >= (int)MustBeLong))
warn("%s: too big for IsNotLong", name);
}
return islong;
}
static void
itCheckDecl(identifier_t name, ipc_type_t *it)
{
enum uselong uselong;
it->itName = name;
itCalculateNameInfo(it);
if (it->itVarArray) {
if ((it->itInTrans != strNULL) ||
(it->itInTransPayload != strNULL) ||
(it->itOutTrans != strNULL))
error("%s: can't translate variable-sized arrays", name);
if (it->itDestructor != strNULL)
error("%s: can't destroy variable-sized array", name);
}
if (it->itVarArray && it->itInLine) {
if ((it->itElement->itUserType == strNULL) ||
(it->itElement->itServerType == strNULL))
error("%s: variable-sized in-line arrays need a named base type",
name);
}
it->itFlags = itCheckFlags(it->itFlags, name);
it->itDeallocate = itCheckDeallocate(it, it->itFlags, d_NO, name);
uselong = itUseLong(it);
if (uselong == TooLong)
error("%s: too big for mach_msg_type_long_t", name);
it->itLongForm = itCheckIsLong(it, it->itFlags,
(int)uselong >= (int)ShouldBeLong, name);
}
static void
itPrintTrans(const ipc_type_t *it)
{
if (it->itName != strNULL && it->itUserType != strNULL && !streql(it->itName, it->itUserType))
printf("\tCUserType:\t%s\n", it->itUserType);
if (it->itName != strNULL && !streql(it->itName, it->itServerType))
printf("\tCServerType:\t%s\n", it->itServerType);
if (it->itInTrans != strNULL)
printf("\tInTran:\t\t%s %s(%s)\n",
it->itTransType, it->itInTrans, it->itServerType);
if (it->itInTransPayload != strNULL)
printf("\tInTranPayload:\t\t%s %s\n",
it->itTransType, it->itInTransPayload);
if (it->itOutTrans != strNULL)
printf("\tOutTran:\t%s %s(%s)\n",
it->itServerType, it->itOutTrans, it->itTransType);
if (it->itDestructor != strNULL)
printf("\tDestructor:\t%s(%s)\n", it->itDestructor, it->itTransType);
}
static void
itPrintDecl(identifier_t name, const ipc_type_t *it)
{
printf("Type %s = ", name);
if (!it->itInLine)
printf("^ ");
if (it->itVarArray)
if (it->itNumber == 0 || it->itIndefinite)
printf("array [] of ");
else
printf("array [*:%d] of ", it->itNumber);
else if (it->itStruct && ((it->itNumber != 1) ||
(it->itInName == MACH_MSG_TYPE_STRING_C)))
printf("struct [%d] of ", it->itNumber);
else if (it->itNumber != 1)
printf("array [%d] of ", it->itNumber);
if (streql(it->itInNameStr, it->itOutNameStr))
printf("(%s,", it->itInNameStr);
else
printf("(%s|%s", it->itInNameStr, it->itOutNameStr);
printf(" %d%s%s)\n", it->itSize,	it->itLongForm ? ", IsLong" : "",
it->itDeallocate == d_YES ? ", Dealloc"
:it->itDeallocate == d_MAYBE ?", Dealloc[]"
: "");
itPrintTrans(it);
printf("\n");
}
void
itTypeDecl(identifier_t name, ipc_type_t *it)
{
itCheckDecl(name, it);
if (BeVerbose)
itPrintDecl(name, it);
}
ipc_type_t *
itShortDecl(u_int inname, const_string_t instr, u_int outname,
const_string_t outstr, u_int defsize)
{
if (defsize == 0)
error("must use full IPC type decl");
return itLongDecl(inname, instr, outname, outstr, defsize, defsize, flNone);
}
ipc_type_t *
itLongDecl(u_int inname, const_string_t instr, u_int outname,
const_string_t outstr, u_int defsize, u_int size, ipc_flags_t flags)
{
ipc_type_t *it;
if ((defsize != 0) && (defsize != size))
warn("IPC type decl has strange size (%u instead of %u)",
size, defsize);
it = itAlloc();
it->itInName = inname;
it->itInNameStr = instr;
it->itOutName = outname;
it->itOutNameStr = outstr;
it->itSize = size;
it->itAlignment = MIN(complex_alignof, size / 8);
if (inname == MACH_MSG_TYPE_STRING_C)
{
it->itStruct = false;
it->itString = true;
}
it->itFlags = flags;
itCalculateSizeInfo(it);
return it;
}
static ipc_type_t *
itCopyType(const ipc_type_t *old)
{
ipc_type_t *new = itAlloc();
*new = *old;
new->itName = strNULL;
new->itNext = itNULL;
new->itElement = (ipc_type_t *) old;
return new;
}
ipc_type_t *
itResetType(ipc_type_t *old)
{
old->itInTrans = strNULL;
old->itInTransPayload = strNULL;
old->itOutTrans = strNULL;
old->itDestructor = strNULL;
old->itUserType = strNULL;
old->itServerType = strNULL;
old->itTransType = strNULL;
return old;
}
ipc_type_t *
itPrevDecl(identifier_t name)
{
ipc_type_t *old;
old = itLookUp(name);
if (old == itNULL)
{
error("type '%s' not defined", name);
return itAlloc();
}
else
return itCopyType(old);
}
ipc_type_t *
itVarArrayDecl(u_int number, const ipc_type_t *old)
{
ipc_type_t *it = itResetType(itCopyType(old));
if (!it->itInLine || it->itVarArray)
error("IPC type decl is too complicated");
if (number != 0)
it->itNumber *= number;
else {
u_int	bytes;
bytes = (it->itNumber * it->itSize + 7) / 8;
it->itNumber = (2048 / bytes) * it->itNumber;
it->itIndefinite = true;
}
it->itVarArray = true;
it->itStruct = false;
it->itString = false;
itCalculateSizeInfo(it);
return it;
}
ipc_type_t *
itArrayDecl(u_int number, const ipc_type_t *old)
{
ipc_type_t *it = itResetType(itCopyType(old));
if (!it->itInLine || it->itVarArray)
error("IPC type decl is too complicated");
it->itNumber *= number;
it->itStruct = false;
it->itString = false;
it->itAlignment = old->itAlignment;
itCalculateSizeInfo(it);
return it;
}
ipc_type_t *
itPtrDecl(ipc_type_t *it)
{
if (!it->itInLine ||
(it->itVarArray && !it->itIndefinite && (it->itNumber > 0)))
error("IPC type decl is too complicated");
it->itNumber = 0;
it->itIndefinite = false;
it->itInLine = false;
it->itStruct = true;
it->itString = false;
it->itAlignment = sizeof_pointer;
itCalculateSizeInfo(it);
return it;
}
ipc_type_t *
itStructArrayDecl(u_int number, const ipc_type_t *old)
{
ipc_type_t *it = itResetType(itCopyType(old));
if (!it->itInLine || it->itVarArray)
error("IPC type decl is too complicated");
it->itNumber *= number;
it->itStruct = true;
it->itString = false;
it->itAlignment = old->itAlignment;
itCalculateSizeInfo(it);
return it;
}
ipc_type_t *
itStructDecl(u_int min_type_size_in_bytes, u_int required_alignment_in_bytes)
{
int final_struct_bytes = min_type_size_in_bytes;
if (final_struct_bytes % required_alignment_in_bytes) {
final_struct_bytes += required_alignment_in_bytes - (final_struct_bytes % required_alignment_in_bytes);
}
ipc_type_t *element_type = NULL;
int number_elements = 0;
switch (required_alignment_in_bytes) {
case 2:
element_type = itShortType;
assert(final_struct_bytes % 2 == 0);
number_elements = final_struct_bytes / 2;
break;
case 4:
element_type = itIntType;
assert(final_struct_bytes % 4 == 0);
number_elements = final_struct_bytes / 4;
break;
case 8:
element_type = itInt64Type;
assert(final_struct_bytes % 8 == 0);
number_elements = final_struct_bytes / 8;
break;
case 1:
default:
element_type = itByteType;
number_elements = final_struct_bytes;
break;
}
ipc_type_t *it = itResetType(itCopyType(element_type));
it->itNumber = number_elements;
it->itStruct = true;
it->itString = false;
it->itAlignment = required_alignment_in_bytes;
itCalculateSizeInfo(it);
return it;
}
ipc_type_t *
itCStringDecl(u_int count, bool varying)
{
ipc_type_t *it;
ipc_type_t *itElement;
itElement = itShortDecl(MACH_MSG_TYPE_STRING_C,
"MACH_MSG_TYPE_STRING_C",
MACH_MSG_TYPE_STRING_C,
"MACH_MSG_TYPE_STRING_C",
8);
itCheckDecl("char", itElement);
it = itResetType(itCopyType(itElement));
it->itNumber = count;
it->itVarArray = varying;
it->itStruct = false;
it->itString = true;
it->itAlignment = itElement->itAlignment;
itCalculateSizeInfo(it);
return it;
}
static ipc_type_t *
itCIntTypeDecl(const_string_t ctype, const size_t size)
{
ipc_type_t *it;
switch (size) {
case 1:
it = itShortDecl(MACH_MSG_TYPE_CHAR, "MACH_MSG_TYPE_CHAR",
MACH_MSG_TYPE_CHAR, "MACH_MSG_TYPE_CHAR", size * 8);
break;
case 2:
it = itShortDecl(MACH_MSG_TYPE_INTEGER_16,
"MACH_MSG_TYPE_INTEGER_16", MACH_MSG_TYPE_INTEGER_16,
"MACH_MSG_TYPE_INTEGER_16", size * 8);
break;
case 4:
it = itShortDecl(MACH_MSG_TYPE_INTEGER_32,
"MACH_MSG_TYPE_INTEGER_32", MACH_MSG_TYPE_INTEGER_32,
"MACH_MSG_TYPE_INTEGER_32", size * 8);
break;
case 8:
it = itShortDecl(MACH_MSG_TYPE_INTEGER_64,
"MACH_MSG_TYPE_INTEGER_64", MACH_MSG_TYPE_INTEGER_64,
"MACH_MSG_TYPE_INTEGER_64", size * 8);
break;
default:
fprintf(stderr, "Unrecognized size %zu for type %s\n", size, ctype);
exit(EXIT_FAILURE);
}
it->itName = ctype;
itCalculateNameInfo(it);
return it;
}
ipc_type_t *
itMakeCountType(void)
{
ipc_type_t *it = itAlloc();
it->itName = "mach_msg_type_number_t";
it->itInName = int_name;
it->itInNameStr = int_name_string;
it->itOutName = int_name;
it->itOutNameStr = int_name_string;
it->itSize = sizeof_int * 8;
it->itAlignment = sizeof_int;
itCalculateSizeInfo(it);
itCalculateNameInfo(it);
return it;
}
static ipc_type_t *
itMakeNaturalType(const char *name)
{
ipc_type_t *it = itAlloc();
it->itName = name;
it->itInName = int_name;
it->itInNameStr = int_name_string;
it->itOutName = int_name;
it->itOutNameStr = int_name_string;
it->itSize = sizeof_int * 8;
it->itAlignment = sizeof_int;
itCalculateSizeInfo(it);
itCalculateNameInfo(it);
return it;
}
ipc_type_t *
itMakePolyType(void)
{
ipc_type_t *it = itAlloc();
it->itName = "mach_msg_type_name_t";
it->itInName = int_name;
it->itInNameStr = int_name_string;
it->itOutName = int_name;
it->itOutNameStr = int_name_string;
it->itSize = sizeof_int * 8;
it->itAlignment = sizeof_int;
itCalculateSizeInfo(it);
itCalculateNameInfo(it);
return it;
}
ipc_type_t *
itMakeDeallocType(void)
{
ipc_type_t *it = itAlloc();
it->itName = "boolean_t";
it->itInName = MACH_MSG_TYPE_BOOLEAN;
it->itInNameStr = "MACH_MSG_TYPE_BOOLEAN";
it->itOutName = MACH_MSG_TYPE_BOOLEAN;
it->itOutNameStr = "MACH_MSG_TYPE_BOOLEAN";
it->itSize = sizeof_int * 8;
it->itAlignment = sizeof_int;
itCalculateSizeInfo(it);
itCalculateNameInfo(it);
return it;
}
void
init_type(void)
{
if (types_initialized)
{
error("Basic types were already initialized");
exit(EXIT_FAILURE);
}
types_initialized = true;
itByteType = itAlloc();
itByteType->itName = "unsigned char";
itByteType->itInName = MACH_MSG_TYPE_BYTE;
itByteType->itInNameStr = "MACH_MSG_TYPE_BYTE";
itByteType->itOutName = MACH_MSG_TYPE_BYTE;
itByteType->itOutNameStr = "MACH_MSG_TYPE_BYTE";
itByteType->itSize = sizeof_char * 8;
itByteType->itAlignment = sizeof_char;
itCalculateSizeInfo(itByteType);
itCalculateNameInfo(itByteType);
itRetCodeType = itAlloc();
itRetCodeType->itName = "kern_return_t";
itRetCodeType->itInName = int_name;
itRetCodeType->itInNameStr = int_name_string;
itRetCodeType->itOutName = int_name;
itRetCodeType->itOutNameStr = int_name_string;
itRetCodeType->itSize = sizeof_int * 8;
itRetCodeType->itAlignment = sizeof_int;
itCalculateSizeInfo(itRetCodeType);
itCalculateNameInfo(itRetCodeType);
itDummyType = itAlloc();
itDummyType->itName = "char *";
itDummyType->itInName = MACH_MSG_TYPE_UNSTRUCTURED;
itDummyType->itInNameStr = "MACH_MSG_TYPE_UNSTRUCTURED";
itDummyType->itOutName = MACH_MSG_TYPE_UNSTRUCTURED;
itDummyType->itOutNameStr = "MACH_MSG_TYPE_UNSTRUCTURED";
itDummyType->itSize = complex_alignof * 8;
itDummyType->itAlignment = complex_alignof;
itCalculateSizeInfo(itDummyType);
itCalculateNameInfo(itDummyType);
itRequestPortType = itAlloc();
itRequestPortType->itName = "mach_port_t";
itRequestPortType->itInName = MACH_MSG_TYPE_COPY_SEND;
itRequestPortType->itInNameStr = "MACH_MSG_TYPE_COPY_SEND";
itRequestPortType->itOutName = MACH_MSG_TYPE_PORT_SEND;
itRequestPortType->itOutNameStr = "MACH_MSG_TYPE_PORT_SEND";
itRequestPortType->itSize = port_size_in_bits;
itRequestPortType->itAlignment = port_size;
itCalculateSizeInfo(itRequestPortType);
itCalculateNameInfo(itRequestPortType);
itZeroReplyPortType = itAlloc();
itZeroReplyPortType->itName = "mach_port_t";
itZeroReplyPortType->itInName = 0;
itZeroReplyPortType->itInNameStr = "0";
itZeroReplyPortType->itOutName = 0;
itZeroReplyPortType->itOutNameStr = "0";
itZeroReplyPortType->itSize = port_size_in_bits;
itZeroReplyPortType->itAlignment = port_size;
itCalculateSizeInfo(itZeroReplyPortType);
itCalculateNameInfo(itZeroReplyPortType);
itRealReplyPortType = itAlloc();
itRealReplyPortType->itName = "mach_port_t";
itRealReplyPortType->itInName = MACH_MSG_TYPE_MAKE_SEND_ONCE;
itRealReplyPortType->itInNameStr = "MACH_MSG_TYPE_MAKE_SEND_ONCE";
itRealReplyPortType->itOutName = MACH_MSG_TYPE_PORT_SEND_ONCE;
itRealReplyPortType->itOutNameStr = "MACH_MSG_TYPE_PORT_SEND_ONCE";
itRealReplyPortType->itSize = port_size_in_bits;
itRealReplyPortType->itAlignment = port_size;
itCalculateSizeInfo(itRealReplyPortType);
itCalculateNameInfo(itRealReplyPortType);
itWaitTimeType = itMakeNaturalType("mach_msg_timeout_t");
itMsgOptionType = itMakeNaturalType("mach_msg_option_t");
itInsert("char", itCIntTypeDecl("char", sizeof_char));
itShortType = itCIntTypeDecl("short", sizeof_short);
itInsert("short", itShortType);
itIntType = itCIntTypeDecl("int", sizeof_int);
itInsert("int", itIntType);
itInt64Type = itCIntTypeDecl("int64", sizeof_int64_t);
itInsert("int64", itInt64Type);
itUintPtrType = itCIntTypeDecl("uintptr_t", sizeof_uintptr_t);
itInsert("uintptr_t", itUintPtrType);
itIntPtrType = itCIntTypeDecl("intptr_t", sizeof_intptr_t);
itInsert("intptr_t", itIntPtrType);
}
void
itCheckReturnType(identifier_t name, const ipc_type_t *it)
{
if (!it->itStruct)
error("type of %s is too complicated", name);
if ((it->itInName == MACH_MSG_TYPE_POLYMORPHIC) ||
(it->itOutName == MACH_MSG_TYPE_POLYMORPHIC))
error("type of %s can't be polymorphic", name);
}
void
itCheckRequestPortType(identifier_t name, const ipc_type_t *it)
{
if (((it->itOutName != MACH_MSG_TYPE_PORT_SEND) &&
(it->itOutName != MACH_MSG_TYPE_PORT_SEND_ONCE) &&
(it->itOutName != MACH_MSG_TYPE_POLYMORPHIC)) ||
(it->itNumber != 1) ||
(it->itSize != port_size_in_bits) ||
!it->itInLine ||
it->itDeallocate != d_NO ||
!it->itStruct ||
it->itVarArray)
error("argument %s isn't a proper request port", name);
}
void
itCheckReplyPortType(identifier_t name, const ipc_type_t *it)
{
if (((it->itOutName != MACH_MSG_TYPE_PORT_SEND) &&
(it->itOutName != MACH_MSG_TYPE_PORT_SEND_ONCE) &&
(it->itOutName != MACH_MSG_TYPE_POLYMORPHIC) &&
(it->itOutName != 0)) ||
(it->itNumber != 1) ||
(it->itSize != port_size_in_bits) ||
!it->itInLine ||
it->itDeallocate != d_NO ||
!it->itStruct ||
it->itVarArray)
error("argument %s isn't a proper reply port", name);
}
void
itCheckIntType(identifier_t name, const ipc_type_t *it)
{
if ((it->itInName != int_name) ||
(it->itOutName != int_name) ||
(it->itNumber != 1) ||
(it->itSize != 32) ||
!it->itInLine ||
it->itDeallocate != d_NO ||
!it->itStruct ||
it->itVarArray)
error("argument %s isn't a proper integer", name);
}
void
itCheckNaturalType(identifier_t name, ipc_type_t *it)
{
if ((it->itInName != int_name) ||
(it->itOutName != int_name) ||
(it->itNumber != 1) ||
(it->itSize != sizeof_int * 8) ||
!it->itInLine ||
it->itDeallocate != d_NO ||
!it->itStruct ||
it->itVarArray)
error("argument %s should have been a %s", name, int_name_string);
}