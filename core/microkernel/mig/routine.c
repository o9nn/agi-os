#include <stdio.h>
#include <stdlib.h>
#include "error.h"
#include "global.h"
#include "routine.h"
#include "message.h"
#include "cpu.h"
#include "utils.h"
u_int rtNumber = 0;
routine_t *
rtAlloc(void)
{
routine_t *new;
new = (routine_t *) calloc(1, sizeof *new);
if (new == rtNULL)
fatal("rtAlloc(): %s", unix_error_string(errno));
new->rtNumber = rtNumber++;
new->rtName = strNULL;
new->rtUserName = strNULL;
new->rtServerName = strNULL;
return new;
}
void
rtSkip(int n)
{
rtNumber += n;
}
argument_t *
argAlloc(void)
{
static const argument_t prototype =
{
strNULL,
argNULL,
akNone,
itNULL,
strNULL,
strNULL,
strNULL,
strNULL,
flNone,
d_NO,
false,
false,
false,
rtNULL,
argNULL,
argNULL,
argNULL,
argNULL,
argNULL,
argNULL,
1,
0,
0,
false,
false
};
argument_t *new;
new = malloc(sizeof *new);
if (new == argNULL)
fatal("argAlloc(): %s", unix_error_string(errno));
*new = prototype;
return new;
}
routine_t *
rtMakeRoutine(identifier_t name, argument_t *args)
{
routine_t *rt = rtAlloc();
rt->rtName = name;
rt->rtKind = rkRoutine;
rt->rtArgs = args;
return rt;
}
routine_t *
rtMakeSimpleRoutine(identifier_t name, argument_t *args)
{
routine_t *rt = rtAlloc();
rt->rtName = name;
rt->rtKind = rkSimpleRoutine;
rt->rtArgs = args;
return rt;
}
const char *
rtRoutineKindToStr(routine_kind_t rk)
{
switch (rk)
{
case rkRoutine:
return "Routine";
case rkSimpleRoutine:
return "SimpleRoutine";
default:
fatal("rtRoutineKindToStr(%d): not a routine_kind_t", rk);
}
}
static void
rtPrintArg(const argument_t *arg)
{
const ipc_type_t *it = arg->argType;
if (!akCheck(arg->argKind, akbUserArg|akbServerArg) ||
(akIdent(arg->argKind) == akeCount) ||
(akIdent(arg->argKind) == akePoly))
return;
printf("\n\t");
switch (akIdent(arg->argKind))
{
case akeRequestPort:
printf("RequestPort");
break;
case akeReplyPort:
printf("ReplyPort");
break;
case akeWaitTime:
printf("WaitTime");
break;
case akeMsgOption:
printf("MsgOption");
break;
case akeMsgSeqno:
printf("MsgSeqno\t");
break;
default:
if (akCheck(arg->argKind, akbRequest))
{
if (akCheck(arg->argKind, akbSend))
printf("In");
else
printf("(In)");
}
if (akCheck(arg->argKind, akbReply))
{
if (akCheck(arg->argKind, akbReturn))
printf("Out");
else
printf("(Out)");
}
printf("\t");
}
printf("\t%s: %s", arg->argName, it->itName);
if (arg->argDeallocate != it->itDeallocate)
{
if (arg->argDeallocate == d_YES)
printf(", Dealloc");
else if (arg->argDeallocate == d_MAYBE)
printf(", Dealloc[]");
else
printf(", NotDealloc");
}
if (arg->argLongForm != it->itLongForm)
{
if (arg->argLongForm)
printf(", IsLong");
else
printf(", IsNotLong");
}
if (arg->argServerCopy)
printf(", ServerCopy");
if (arg->argCountInOut)
printf(", CountInOut");
}
void
rtPrintRoutine(const routine_t *rt)
{
const argument_t *arg;
printf("%s (%d) %s(", rtRoutineKindToStr(rt->rtKind),
rt->rtNumber, rt->rtName);
for (arg = rt->rtArgs; arg != argNULL; arg = arg->argNext)
rtPrintArg(arg);
printf(")\n");
}
static void
rtCheckSimpleIn(const argument_t *args, u_int mask, bool *fixed,
bool *simple)
{
const argument_t *arg;
bool MayBeComplex = false;
bool MustBeComplex = false;
for (arg = args; arg != argNULL; arg = arg->argNext)
if (akCheck(arg->argKind, mask))
{
const ipc_type_t *it = arg->argType;
if (it->itInName == MACH_MSG_TYPE_POLYMORPHIC)
MayBeComplex = true;
if (it->itIndefinite)
MayBeComplex = true;
if (MACH_MSG_TYPE_PORT_ANY(it->itInName) ||
!it->itInLine)
MustBeComplex = true;
}
*fixed = MustBeComplex || !MayBeComplex;
*simple = !MustBeComplex;
}
static void
rtCheckSimpleOut(const argument_t *args, u_int mask, bool *fixed,
bool *simple)
{
const argument_t *arg;
bool MayBeComplex = false;
bool MustBeComplex = false;
for (arg = args; arg != argNULL; arg = arg->argNext)
if (akCheck(arg->argKind, mask))
{
const ipc_type_t *it = arg->argType;
if (it->itOutName == MACH_MSG_TYPE_POLYMORPHIC)
MayBeComplex = true;
if (it->itIndefinite)
MayBeComplex = true;
if (MACH_MSG_TYPE_PORT_ANY(it->itOutName) ||
!it->itInLine)
MustBeComplex = true;
}
*fixed = MustBeComplex || !MayBeComplex;
*simple = !MustBeComplex;
}
static u_int
rtFindSize(const argument_t *args, u_int mask)
{
const argument_t *arg;
u_int size = sizeof_mach_msg_header_t;
size = ALIGN(size, complex_alignof);
for (arg = args; arg != argNULL; arg = arg->argNext)
if (akCheck(arg->argKind, mask))
{
ipc_type_t *it = arg->argType;
if (arg->argLongForm) {
size += sizeof_mach_msg_type_long_t;
} else {
size += sizeof_mach_msg_type_t;
}
size = ALIGN(size, complex_alignof);
size += it->itMinTypeSize;
}
return size;
}
bool
rtCheckMask(const argument_t *args, u_int mask)
{
const argument_t *arg;
for (arg = args; arg != argNULL; arg = arg->argNext)
if (akCheckAll(arg->argKind, mask))
return true;
return false;
}
bool
rtCheckMaskFunction(const argument_t *args, u_int mask,
bool (*func)(const argument_t *))
{
const argument_t *arg;
for (arg = args; arg != argNULL; arg = arg->argNext)
if (akCheckAll(arg->argKind, mask))
if ((*func)(arg))
return true;
return false;
}
static void
rtDefaultArgKind(const routine_t *rt, argument_t *arg)
{
if ((arg->argKind == akNone) &&
(rt->rtRequestPort == argNULL))
arg->argKind = akRequestPort;
if (arg->argKind == akNone)
arg->argKind = akIn;
}
static void
rtProcessArgFlags(argument_t *arg)
{
const ipc_type_t *it = arg->argType;
arg->argFlags = itCheckFlags(arg->argFlags, arg->argName);
if (((IsKernelServer && akCheck(arg->argKind, akbReturn)) ||
(IsKernelUser && akCheck(arg->argKind, akbSend))) &&
(arg->argFlags & flDealloc) &&
(it->itDeallocate == d_NO)) {
arg->argDeallocate = d_YES;
} else
arg->argDeallocate = itCheckDeallocate(it, arg->argFlags,
it->itDeallocate, arg->argName);
arg->argLongForm = itCheckIsLong(it, arg->argFlags,
it->itLongForm, arg->argName);
if (arg->argFlags & flServerCopy) {
if (it->itIndefinite && akCheck(arg->argKind, akbSend))
arg->argServerCopy = true;
else
warn("%s: ServerCopy on argument is meaningless", arg->argName);
}
if (arg->argFlags & flCountInOut) {
if (it->itVarArray && it->itInLine &&
akCheck(arg->argKind, akbReply))
arg->argCountInOut = true;
else
warn("%s: CountInOut on argument is meaningless", arg->argName);
}
}
static void
rtAugmentArgKind(argument_t *arg)
{
ipc_type_t *it = arg->argType;
if (it->itVarArray && it->itInLine)
{
if (akCheckAll(arg->argKind, akbRequest|akbReply))
error("%s: Inline variable-sized arguments can't be InOut",
arg->argName);
arg->argKind = akAddFeature(arg->argKind, akbVariable);
if (it->itIndefinite)
arg->argKind = akAddFeature(arg->argKind, akbIndefinite);
}
if (akCheck(arg->argKind, akbRequest) &&
!arg->argLongForm &&
(it->itOutName != MACH_MSG_TYPE_POLYMORPHIC) &&
!it->itVarArray &&
!(IsKernelServer && (!it->itInLine ||
MACH_MSG_TYPE_PORT_ANY(it->itOutName))))
arg->argKind = akAddFeature(arg->argKind, akbRequestQC);
if (akCheck(arg->argKind, akbReply) &&
!arg->argLongForm &&
(it->itOutName != MACH_MSG_TYPE_POLYMORPHIC) &&
!it->itVarArray)
arg->argKind = akAddFeature(arg->argKind, akbReplyQC);
if (((it->itOutTrans != strNULL) &&
akCheck(arg->argKind, akbReturnSnd)) ||
((it->itInTrans != strNULL) &&
akCheckAll(arg->argKind, akbSendRcv|akbReturnSnd)) ||
((it->itDestructor != strNULL) &&
akCheck(arg->argKind, akbSendRcv) &&
!akCheck(arg->argKind, akbReturnSnd) &&
(it->itInTrans != strNULL)) ||
((akIdent(arg->argKind) == akeCount) &&
akCheck(arg->argKind, akbReturnSnd)) ||
((akIdent(arg->argKind) == akePoly) &&
akCheck(arg->argKind, akbReturnSnd)) ||
((akIdent(arg->argKind) == akeDealloc) &&
akCheck(arg->argKind, akbReturnSnd)) ||
(it->itInTransPayload != strNULL))
{
arg->argKind = akRemFeature(arg->argKind, akbReplyCopy);
arg->argKind = akAddFeature(arg->argKind, akbVarNeeded);
}
if (it->itIndefinite &&
akCheck(arg->argKind, akbReturnSnd))
{
arg->argKind = akAddFeature(arg->argKind, akbPointer);
}
if (akCheck(arg->argKind, akbSendRcv) &&
IS_64BIT_ABI &&
it->itUserlandPort &&
akCheck(arg->argKind, akbIndefinite)) {
arg->argKind = akAddFeature(arg->argKind, akbPointer);
}
}
static void
rtCheckRoutineArg(routine_t *rt, argument_t *arg)
{
switch (akIdent(arg->argKind))
{
case akeRequestPort:
if (rt->rtRequestPort != argNULL)
warn("multiple RequestPort args in %s; %s won't be used",
rt->rtName, rt->rtRequestPort->argName);
rt->rtRequestPort = arg;
break;
case akeReplyPort:
if (akCheck (arg->argKind, akbUserArg))
{
if (rt->rtUReplyPort != argNULL)
warn("multiple UserReplyPort args in %s; %s won't be used",
rt->rtName, rt->rtUReplyPort->argName);
rt->rtUReplyPort = arg;
}
if (akCheck (arg->argKind, akbServerArg))
{
if (rt->rtSReplyPort != argNULL)
warn("multiple ServerReplyPort args in %s; %s won't be used",
rt->rtName, rt->rtSReplyPort->argName);
rt->rtSReplyPort = arg;
}
break;
case akeWaitTime:
if (rt->rtWaitTime != argNULL)
warn("multiple WaitTime args in %s; %s won't be used",
rt->rtName, rt->rtWaitTime->argName);
rt->rtWaitTime = arg;
break;
case akeMsgOption:
if (rt->rtMsgOption != argNULL)
warn("multiple MsgOption args in %s; %s won't be used",
rt->rtName, rt->rtMsgOption->argName);
rt->rtMsgOption = arg;
break;
case akeMsgSeqno:
if (rt->rtMsgSeqno != argNULL)
warn("multiple MsgSeqno args in %s; %s won't be used",
rt->rtName, rt->rtMsgSeqno->argName);
rt->rtMsgSeqno = arg;
break;
case akeReturn:
if (rt->rtReturn != argNULL)
warn("multiple Return args in %s; %s won't be used",
rt->rtName, rt->rtReturn->argName);
rt->rtReturn = arg;
break;
default:
break;
}
}
static void
rtSetArgDefaults(routine_t *rt, argument_t *arg)
{
arg->argRoutine = rt;
if (arg->argVarName == strNULL)
arg->argVarName = arg->argName;
if (arg->argMsgField == strNULL)
switch(akIdent(arg->argKind))
{
case akeRequestPort:
arg->argMsgField = "Head.msgh_request_port";
break;
case akeReplyPort:
arg->argMsgField = "Head.msgh_reply_port";
break;
case akeMsgSeqno:
arg->argMsgField = "Head.msgh_seqno";
break;
default:
arg->argMsgField = arg->argName;
break;
}
if (arg->argTTName == strNULL)
arg->argTTName = strconcat(arg->argName, "Type");
if (arg->argPadName == strNULL)
arg->argPadName = strconcat(arg->argName, "Pad");
if ((rt->rtRequestPort != argNULL) &&
(rt->rtRequestPort->argPoly == arg) &&
(arg->argType != itNULL)) {
arg->argMsgField = "Head.msgh_bits";
arg->argType->itInTrans = "MACH_MSGH_BITS_REQUEST";
}
if ((rt->rtUReplyPort != argNULL) &&
(rt->rtUReplyPort->argPoly == arg) &&
(arg->argType != itNULL)) {
arg->argMsgField = "Head.msgh_bits";
arg->argType->itInTrans = "MACH_MSGH_BITS_REPLY";
}
if ((rt->rtSReplyPort != argNULL) &&
(rt->rtSReplyPort->argPoly == arg) &&
(arg->argType != itNULL)) {
arg->argMsgField = "Head.msgh_bits";
arg->argType->itInTrans = "MACH_MSGH_BITS_REPLY";
}
}
static void
rtAddCountArg(argument_t *arg)
{
argument_t *count;
count = argAlloc();
count->argName = strconcat(arg->argName, "Cnt");
count->argType = itMakeCountType();
count->argParent = arg;
count->argMultiplier = arg->argType->itElement->itNumber;
count->argNext = arg->argNext;
arg->argNext = count;
arg->argCount = count;
if (arg->argType->itString) {
count->argKind = akeCount;
count->argVarName = (char *)0;
} else
count->argKind = akAddFeature(akCount,
akCheck(arg->argKind, akbSendReturnBits));
if (arg->argLongForm)
count->argMsgField = strconcat(arg->argTTName,
".msgtl_number");
else
count->argMsgField = strconcat(arg->argTTName, ".msgt_number");
}
static void
rtAddCountInOutArg(argument_t *arg)
{
argument_t *count;
count = argAlloc();
count->argName = strconcat(arg->argName, "Cnt");
count->argType = itMakeCountType();
count->argParent = argNULL;
count->argNext = arg->argNext;
arg->argNext = count;
(count->argCInOut = arg->argCount)->argCInOut = count;
count->argKind = akCountInOut;
}
static void
rtAddPolyArg(argument_t *arg)
{
const ipc_type_t *it = arg->argType;
argument_t *poly;
arg_kind_t akbsend, akbreturn;
poly = argAlloc();
poly->argName = strconcat(arg->argName, "Poly");
poly->argType = itMakePolyType();
poly->argParent = arg;
poly->argNext = arg->argNext;
arg->argNext = poly;
arg->argPoly = poly;
akbsend = akbSend|akbSendBody;
akbreturn = akbReturn|akbReturnBody;
if (it->itInName == MACH_MSG_TYPE_POLYMORPHIC)
{
akbsend |= akbUserArg|akbSendSnd;
if (!IsKernelServer)
akbreturn |= akbServerArg|akbReturnSnd;
}
if (it->itOutName == MACH_MSG_TYPE_POLYMORPHIC)
{
akbsend |= akbServerArg|akbSendRcv;
akbreturn |= akbUserArg|akbReturnRcv;
if (IsKernelServer)
akbreturn |= akbServerArg|akbReturnSnd;
}
poly->argKind = akPoly;
if (akCheck(arg->argKind, akbSend))
poly->argKind = akAddFeature(poly->argKind,
akCheck(arg->argKind, akbsend));
if (akCheck(arg->argKind, akbReturn))
poly->argKind = akAddFeature(poly->argKind,
akCheck(arg->argKind, akbreturn));
if (arg->argLongForm)
poly->argMsgField = strconcat(arg->argTTName,
".msgtl_name");
else
poly->argMsgField = strconcat(arg->argTTName, ".msgt_name");
}
static void
rtAddDeallocArg(argument_t *arg)
{
argument_t *dealloc;
dealloc = argAlloc();
dealloc->argName = strconcat(arg->argName, "Dealloc");
dealloc->argType = itMakeDeallocType();
dealloc->argParent = arg;
dealloc->argNext = arg->argNext;
arg->argNext = dealloc;
arg->argDealloc = dealloc;
dealloc->argKind = akeDealloc;
if (akCheck(arg->argKind, akbSend))
dealloc->argKind = akAddFeature(dealloc->argKind,
akCheck(arg->argKind,
akbUserArg|akbSend|akbSendBody|
(arg->argType->itIndefinite ? 0 : akbSendSnd)));
if (akCheck(arg->argKind, akbReturn)) {
dealloc->argKind = akAddFeature(dealloc->argKind,
akCheck(arg->argKind,
akbServerArg|akbReturn|akbReturnBody|
(arg->argType->itIndefinite ? 0 : akbReturnSnd)));
if (arg->argType->itIndefinite) {
dealloc->argKind = akAddFeature(dealloc->argKind, akbVarNeeded);
dealloc->argByReferenceServer = true;
}
}
if (arg->argLongForm)
dealloc->argMsgField = strconcat(arg->argTTName,
".msgtl_header.msgt_deallocate");
else
dealloc->argMsgField = strconcat(arg->argTTName, ".msgt_deallocate");
}
static void
rtAddSCopyArg(argument_t *arg)
{
argument_t *scopy;
scopy = argAlloc();
scopy->argName = strconcat(arg->argName, "SCopy");
scopy->argType = itMakeDeallocType();
scopy->argParent = arg;
scopy->argNext = arg->argNext;
arg->argNext = scopy;
arg->argSCopy = scopy;
scopy->argKind = akServerCopy;
if (arg->argLongForm)
scopy->argMsgField = strconcat(arg->argTTName,
".msgtl_header.msgt_inline");
else
scopy->argMsgField = strconcat(arg->argTTName, ".msgt_inline");
}
static void
rtCheckRoutineArgs(routine_t *rt)
{
argument_t *arg;
for (arg = rt->rtArgs; arg != argNULL; arg = arg->argNext)
{
const ipc_type_t *it = arg->argType;
rtDefaultArgKind(rt, arg);
rtCheckRoutineArg(rt, arg);
rtSetArgDefaults(rt, arg);
if (it != itNULL)
{
rtProcessArgFlags(arg);
rtAugmentArgKind(arg);
if (arg->argServerCopy)
rtAddSCopyArg(arg);
if (arg->argDeallocate == d_MAYBE)
rtAddDeallocArg(arg);
if (it->itVarArray)
rtAddCountArg(arg);
if (arg->argCountInOut)
rtAddCountInOutArg(arg);
if ((it->itInName == MACH_MSG_TYPE_POLYMORPHIC) ||
(it->itOutName == MACH_MSG_TYPE_POLYMORPHIC))
rtAddPolyArg(arg);
}
}
}
static void
rtCheckArgTypes(routine_t *rt)
{
if (rt->rtRequestPort == argNULL)
error("%s %s doesn't have a server port argument",
rtRoutineKindToStr(rt->rtKind), rt->rtName);
if (rt->rtReturn != argNULL)
error("routine %s has a return arg", rt->rtName);
if (rt->rtReturn == argNULL)
rt->rtReturn = rt->rtRetCode;
rt->rtServerReturn = rt->rtReturn;
if ((rt->rtReturn != argNULL) &&
(rt->rtReturn->argType != itNULL))
itCheckReturnType(rt->rtReturn->argName,
rt->rtReturn->argType);
if ((rt->rtRequestPort != argNULL) &&
(rt->rtRequestPort->argType != itNULL))
itCheckRequestPortType(rt->rtRequestPort->argName,
rt->rtRequestPort->argType);
if ((rt->rtUReplyPort != argNULL) &&
(rt->rtUReplyPort->argType != itNULL))
itCheckReplyPortType(rt->rtUReplyPort->argName,
rt->rtUReplyPort->argType);
if ((rt->rtSReplyPort != argNULL) &&
(rt->rtSReplyPort->argType != itNULL))
itCheckReplyPortType(rt->rtSReplyPort->argName,
rt->rtSReplyPort->argType);
if ((rt->rtWaitTime != argNULL) &&
(rt->rtWaitTime->argType != itNULL))
itCheckNaturalType(rt->rtWaitTime->argName,
rt->rtWaitTime->argType);
if ((rt->rtMsgOption != argNULL) &&
(rt->rtMsgOption->argType != itNULL))
itCheckNaturalType(rt->rtMsgOption->argName,
rt->rtMsgOption->argType);
if ((rt->rtMsgSeqno != argNULL) &&
(rt->rtMsgSeqno->argType != itNULL))
itCheckNaturalType(rt->rtMsgSeqno->argName,
rt->rtMsgSeqno->argType);
}
static void
rtCheckArgTrans(const routine_t *rt)
{
const argument_t *arg;
for (arg = rt->rtArgs; arg != argNULL; arg = arg->argNext)
{
const ipc_type_t *it = arg->argType;
if ((it != itNULL) &&
!streql(it->itServerType, it->itTransType))
{
if (akCheck(arg->argKind, akbSendRcv) &&
(it->itInTrans == strNULL))
warn("%s: argument has no in-translation function",
arg->argName);
if (akCheck(arg->argKind, akbReturnSnd) &&
(it->itOutTrans == strNULL))
warn("%s: argument has no out-translation function",
arg->argName);
}
}
}
static void
rtAddRetCode(routine_t *rt)
{
argument_t *arg = argAlloc();
arg->argName = "RetCode";
arg->argType = itRetCodeType;
arg->argKind = akRetCode;
rt->rtRetCode = arg;
arg->argNext = rt->rtArgs;
rt->rtArgs = arg;
}
static void
rtAddWaitTime(routine_t *rt, identifier_t name)
{
argument_t *arg = argAlloc();
argument_t **loc;
arg->argName = "dummy WaitTime arg";
arg->argVarName = name;
arg->argType = itWaitTimeType;
arg->argKind = akeWaitTime;
rt->rtWaitTime = arg;
if (rt->rtMsgOption != argNULL)
loc = &rt->rtMsgOption->argNext;
else
loc = &rt->rtArgs;
arg->argNext = *loc;
*loc = arg;
rtSetArgDefaults(rt, arg);
}
static void
rtAddMsgOption(routine_t *rt, identifier_t name)
{
argument_t *arg = argAlloc();
argument_t **loc;
arg->argName = "dummy MsgOption arg";
arg->argVarName = name;
arg->argType = itMsgOptionType;
arg->argKind = akeMsgOption;
rt->rtMsgOption = arg;
if (rt->rtMsgSeqno != argNULL)
loc = &rt->rtMsgSeqno->argNext;
else
loc = &rt->rtArgs;
arg->argNext = *loc;
*loc = arg;
rtSetArgDefaults(rt, arg);
}
static void
rtAddDummyReplyPort(routine_t *rt, ipc_type_t *type, int user)
{
argument_t *arg = argAlloc();
argument_t **loc;
arg->argName = "dummy ReplyPort arg";
arg->argVarName = "dummy ReplyPort arg";
arg->argType = type;
arg->argKind = akeReplyPort;
if (user)
rt->rtUReplyPort = arg;
else
rt->rtSReplyPort = arg;
if (rt->rtRequestPort != argNULL)
loc = &rt->rtRequestPort->argNext;
else
loc = &rt->rtArgs;
arg->argNext = *loc;
*loc = arg;
rtSetArgDefaults(rt, arg);
}
static void
rtCheckVariable(routine_t *rt)
{
argument_t *arg;
int NumRequestVar = 0;
int NumReplyVar = 0;
int MaxRequestPos = 0;
int MaxReplyPos = 0;
for (arg = rt->rtArgs; arg != argNULL; arg = arg->argNext) {
argument_t *parent = arg->argParent;
if (parent == argNULL) {
if (akCheck(arg->argKind, akbRequest|akbSend)) {
arg->argRequestPos = NumRequestVar;
MaxRequestPos = NumRequestVar;
if (akCheck(arg->argKind, akbVariable))
NumRequestVar++;
} else
arg->argRequestPos = -1;
if (akCheck(arg->argKind, akbReply|akbReturn)) {
arg->argReplyPos = NumReplyVar;
MaxReplyPos = NumReplyVar;
if (akCheck(arg->argKind, akbVariable))
NumReplyVar++;
} else
arg->argReplyPos = -1;
} else {
arg->argRequestPos = parent->argRequestPos;
arg->argReplyPos = parent->argReplyPos;
}
if (akCheck(arg->argKind, akbReturnSnd) &&
!akCheck(arg->argKind, akbReplyCopy|akbVarNeeded) &&
(arg->argReplyPos > 0))
arg->argKind = akAddFeature(arg->argKind, akbVarNeeded);
}
rt->rtNumRequestVar = NumRequestVar;
rt->rtNumReplyVar = NumReplyVar;
rt->rtMaxRequestPos = MaxRequestPos;
rt->rtMaxReplyPos = MaxReplyPos;
}
static void
rtCheckDestroy(routine_t *rt)
{
argument_t *arg;
for (arg = rt->rtArgs; arg != argNULL; arg = arg->argNext) {
const ipc_type_t *it = arg->argType;
if(akCheck(arg->argKind, akbSendRcv) &&
!akCheck(arg->argKind, akbReturnSnd)) {
if ((it->itDestructor != strNULL) ||
(akCheck(arg->argKind, akbIndefinite) && !arg->argServerCopy))
arg->argKind = akAddFeature(arg->argKind, akbDestroy);
}
}
}
static void
rtAddByReference(routine_t *rt)
{
argument_t *arg;
for (arg = rt->rtArgs; arg != argNULL; arg = arg->argNext) {
const ipc_type_t *it = arg->argType;
if (akCheck(arg->argKind, akbReturnRcv) &&
(it->itStruct || it->itIndefinite)) {
arg->argByReferenceUser = true;
if (arg->argCInOut != argNULL)
arg->argCInOut->argByReferenceUser = true;
}
if (akCheck(arg->argKind, akbReturnSnd) &&
(it->itStruct || it->itIndefinite))
arg->argByReferenceServer = true;
}
}
void
rtCheckRoutine(routine_t *rt)
{
rt->rtOneWay = (rt->rtKind == rkSimpleRoutine);
rt->rtServerName = strconcat(ServerPrefix, rt->rtName);
rt->rtServerName = strconcat(RoutinePrefix, rt->rtServerName);
rt->rtUserName = strconcat(UserPrefix, rt->rtName);
rt->rtUserName = strconcat(RoutinePrefix, rt->rtUserName);
rtAddRetCode(rt);
rtCheckRoutineArgs(rt);
if (rt->rtUReplyPort == argNULL)
{
if (rt->rtOneWay)
rtAddDummyReplyPort(rt, itZeroReplyPortType, 1);
else
rtAddDummyReplyPort(rt, itRealReplyPortType, 1);
}
if (rt->rtSReplyPort == argNULL)
{
if (rt->rtOneWay)
rtAddDummyReplyPort(rt, itZeroReplyPortType, 0);
else
rtAddDummyReplyPort(rt, itRealReplyPortType, 0);
}
if (rt->rtMsgOption == argNULL)
{
if (MsgOption == strNULL)
rtAddMsgOption(rt, "MACH_MSG_OPTION_NONE");
else
rtAddMsgOption(rt, MsgOption);
}
if ((rt->rtWaitTime == argNULL) &&
(WaitTime != strNULL))
rtAddWaitTime(rt, WaitTime);
rtCheckArgTypes(rt);
rtCheckArgTrans(rt);
if (rt->rtOneWay && rtCheckMask(rt->rtArgs, akbReturn))
error("%s %s has OUT argument",
rtRoutineKindToStr(rt->rtKind), rt->rtName);
if (errors > 0)
return;
rtCheckSimpleIn(rt->rtArgs, akbRequest,
&rt->rtSimpleFixedRequest,
&rt->rtSimpleSendRequest);
rtCheckSimpleOut(rt->rtArgs, akbRequest,
&rt->rtSimpleCheckRequest,
&rt->rtSimpleReceiveRequest);
rt->rtRequestSize = rtFindSize(rt->rtArgs, akbRequest);
if (IsKernelServer)
rtCheckSimpleOut(rt->rtArgs, akbReply,
&rt->rtSimpleFixedReply,
&rt->rtSimpleSendReply);
else
rtCheckSimpleIn(rt->rtArgs, akbReply,
&rt->rtSimpleFixedReply,
&rt->rtSimpleSendReply);
rtCheckSimpleOut(rt->rtArgs, akbReply,
&rt->rtSimpleCheckReply,
&rt->rtSimpleReceiveReply);
rt->rtReplySize = rtFindSize(rt->rtArgs, akbReply);
rtCheckVariable(rt);
rtCheckDestroy(rt);
rtAddByReference(rt);
rt->rtNoReplyArgs = !rtCheckMask(rt->rtArgs, akbReturnSnd);
}