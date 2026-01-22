#include <assert.h>
#include "write.h"
#include "error.h"
#include "utils.h"
#include "global.h"
#include "mig_string.h"
#include "cpu.h"
static void
WriteIncludes(FILE *file)
{
fprintf(file, "#ifndef _GNU_SOURCE\n");
fprintf(file, "#define _GNU_SOURCE 1\n");
fprintf(file, "#endif\n\n");
if (IsKernelServer)
{
fprintf(file, "#undef\tKERNEL\n");
if (InternalHeaderFileName != strNULL)
{
char *cp;
cp = strrchr(InternalHeaderFileName, '/');
if (cp == 0)
cp = InternalHeaderFileName;
else
cp++;
fprintf(file, "#include \"%s\"\n", cp);
}
}
if (UserHeaderFileName != strNULL)
{
char *cp;
cp = strrchr(UserHeaderFileName, '/');
if (cp == 0)
cp = UserHeaderFileName;
else
cp++;
fprintf(file, "#include \"%s\"\n", cp);
}
fprintf(file, "#define EXPORT_BOOLEAN\n");
fprintf(file, "#include <mach/boolean.h>\n");
fprintf(file, "#include <mach/kern_return.h>\n");
fprintf(file, "#include <mach/message.h>\n");
fprintf(file, "#include <mach/notify.h>\n");
fprintf(file, "#include <mach/mach_types.h>\n");
fprintf(file, "#include <mach/mig_errors.h>\n");
fprintf(file, "#include <mach/mig_support.h>\n");
if (IsKernelUser)
fprintf(file, "#include <kern/ipc_mig.h>\n");
fprintf(file, "#include <stdint.h>\n");
fprintf(file, "\n");
}
static void
WriteGlobalDecls(FILE *file)
{
if (RCSId != strNULL)
WriteRCSDecl(file, strconcat(SubsystemName, "_user"), RCSId);
fprintf(file, "#define msgh_request_port\tmsgh_remote_port\n");
fprintf(file, "#define msgh_reply_port\t\tmsgh_local_port\n");
fprintf(file, "\n");
}
static void
WriteProlog(FILE *file)
{
WriteIncludes(file);
WriteBogusDefines(file);
WriteGlobalDecls(file);
}
static void
WriteEpilog(FILE *file)
{
}
static const_string_t
WriteHeaderPortType(const argument_t *arg)
{
if (arg->argType->itInName == MACH_MSG_TYPE_POLYMORPHIC)
return arg->argPoly->argVarName;
else
return arg->argType->itInNameStr;
}
static void
WriteRequestHead(FILE *file, const routine_t *rt)
{
if (rt->rtMaxRequestPos > 0)
fprintf(file, "\tInP = &Mess.In;\n");
if (rt->rtSimpleFixedRequest) {
fprintf(file, "\tInP->Head.msgh_bits =");
if (!rt->rtSimpleSendRequest)
fprintf(file, " MACH_MSGH_BITS_COMPLEX|");
fprintf(file, "\n");
fprintf(file, "\t\tMACH_MSGH_BITS(%s, %s);\n",
WriteHeaderPortType(rt->rtRequestPort),
WriteHeaderPortType(rt->rtUReplyPort));
} else {
fprintf(file, "\tInP->Head.msgh_bits = msgh_simple ?\n");
fprintf(file, "\t\tMACH_MSGH_BITS(%s, %s) :\n",
WriteHeaderPortType(rt->rtRequestPort),
WriteHeaderPortType(rt->rtUReplyPort));
fprintf(file, "\t\t(MACH_MSGH_BITS_COMPLEX|\n");
fprintf(file, "\t\t MACH_MSGH_BITS(%s, %s));\n",
WriteHeaderPortType(rt->rtRequestPort),
WriteHeaderPortType(rt->rtUReplyPort));
}
fprintf(file, "\t\n");
if (IsKernelUser)
fprintf(file, "\tInP->%s = (mach_port_t) %s;\n",
rt->rtRequestPort->argMsgField,
rt->rtRequestPort->argVarName);
else
fprintf(file, "\tInP->%s = %s;\n",
rt->rtRequestPort->argMsgField,
rt->rtRequestPort->argVarName);
if (akCheck(rt->rtUReplyPort->argKind, akbUserArg)) {
if (IsKernelUser)
fprintf(file, "\tInP->%s = (mach_port_t) %s;\n",
rt->rtUReplyPort->argMsgField,
rt->rtUReplyPort->argVarName);
else
fprintf(file, "\tInP->%s = %s;\n",
rt->rtUReplyPort->argMsgField,
rt->rtUReplyPort->argVarName);
} else if (rt->rtOneWay || IsKernelUser)
fprintf(file, "\tInP->%s = MACH_PORT_NULL;\n",
rt->rtUReplyPort->argMsgField);
else
fprintf(file, "\tInP->%s = %smig_get_reply_port();\n",
rt->rtUReplyPort->argMsgField, SubrPrefix);
fprintf(file, "\tInP->Head.msgh_seqno = 0;\n");
fprintf(file, "\tInP->Head.msgh_id = %d;\n", rt->rtNumber + SubsystemBase);
}
static void
WriteVarDecls(FILE *file, const routine_t *rt)
{
fprintf(file, "\tunion {\n");
fprintf(file, "\t\tRequest In;\n");
if (!rt->rtOneWay)
fprintf(file, "\t\tReply Out;\n");
fprintf(file, "\t} Mess;\n");
fprintf(file, "\n");
fprintf(file, "\tRequest *InP = &Mess.In;\n");
if (!rt->rtOneWay)
fprintf(file, "\tReply *OutP = &Mess.Out;\n");
fprintf(file, "\n");
if (!rt->rtOneWay)
fprintf(file, "\tmach_msg_return_t msg_result;\n");
if (!rt->rtSimpleFixedRequest)
fprintf(file, "\tboolean_t msgh_simple = %s;\n",
strbool(rt->rtSimpleSendRequest));
else if (!rt->rtOneWay &&
!(rt->rtSimpleCheckReply && rt->rtSimpleReceiveReply)) {
fprintf(file, "#if\tTypeCheck\n");
fprintf(file, "\tboolean_t msgh_simple;\n");
fprintf(file, "#endif\t\n");
}
if (rt->rtNumRequestVar > 0)
fprintf(file, "\tunsigned int msgh_size;\n");
else if (!rt->rtOneWay && !rt->rtNoReplyArgs)
{
fprintf(file, "#if\tTypeCheck\n");
fprintf(file, "\tunsigned int msgh_size;\n");
fprintf(file, "#endif\t\n");
}
if ((rt->rtMaxRequestPos > 0) ||
(rt->rtMaxReplyPos > 0))
fprintf(file, "\tunsigned int msgh_size_delta;\n");
fprintf(file, "\n");
}
static void
WriteMsgError(FILE *file, const routine_t *rt, const char *error_msg)
{
if (rt->rtReturn != rt->rtRetCode)
{
fprintf(file, "\t\t{ (%s); ", error_msg);
if (rt->rtNumReplyVar > 0)
fprintf(file, "OutP = &Mess.Out; ");
fprintf(file, "return OutP->%s; }\n", rt->rtReturn->argMsgField);
}
else
fprintf(file, "\t\treturn %s;\n", error_msg);
}
static void
WriteMsgSend(FILE *file, const routine_t *rt)
{
const char *MsgResult = "return";
char SendSize[24];
if (rt->rtNumRequestVar == 0)
{
sprintf(SendSize, "%d", rt->rtRequestSize);
fprintf(file, "\t_Static_assert(sizeof(Request) == %s, \"Request expected to be %s bytes\");\n", SendSize, SendSize);
}
else
strcpy(SendSize, "msgh_size");
fprintf(file, "\tInP->Head.msgh_size = %s;\n\n", SendSize);
if (IsKernelUser)
{
fprintf(file, "\t%s %smach_msg_send_from_kernel(",
MsgResult, SubrPrefix);
fprintf(file, "&InP->Head, %s);\n", SendSize);
}
else
{
fprintf(file, "\t%s %smach_msg(&InP->Head, MACH_SEND_MSG|%s, %s, 0,",
MsgResult,
SubrPrefix,
rt->rtMsgOption->argVarName,
SendSize);
fprintf(file,
" MACH_PORT_NULL, MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);\n"
);
}
}
static void
WriteMsgCheckReceive(FILE *file, const routine_t *rt, const char *success)
{
fprintf(file, "\tif (msg_result != %s) {\n", success);
if (!akCheck(rt->rtUReplyPort->argKind, akbUserArg) && !IsKernelUser)
{
fprintf(file, "\t\t%smig_dealloc_reply_port(%s);\n",
SubrPrefix, "InP->Head.msgh_reply_port");
}
WriteMsgError(file, rt, "msg_result");
fprintf(file, "\t}\n");
if (!akCheck(rt->rtUReplyPort->argKind, akbUserArg) && !IsKernelUser)
{
fprintf(file, "\t%smig_put_reply_port(InP->Head.msgh_reply_port);\n",
SubrPrefix);
}
}
static void
WriteMsgRPC(FILE *file, const routine_t *rt)
{
char SendSize[24];
if (rt->rtNumRequestVar == 0)
{
sprintf(SendSize, "%d", rt->rtRequestSize);
fprintf(file, "\t_Static_assert(sizeof(Request) == %s, \"Request expected to be %s bytes\");\n", SendSize, SendSize);
} else
strcpy(SendSize, "msgh_size");
fprintf(file, "\tInP->Head.msgh_size = %s;\n\n", SendSize);
if (IsKernelUser)
fprintf(file, "\tmsg_result = %smach_msg_rpc_from_kernel(&InP->Head, %s, sizeof(Reply));\n",
SubrPrefix,
SendSize);
else
fprintf(file, "\tmsg_result = %smach_msg(&InP->Head, MACH_SEND_MSG|MACH_RCV_MSG|%s%s, %s, sizeof(Reply), InP->Head.msgh_reply_port, %s, MACH_PORT_NULL);\n",
SubrPrefix,
rt->rtMsgOption->argVarName,
rt->rtWaitTime != argNULL ? "|MACH_RCV_TIMEOUT" : "",
SendSize,
rt->rtWaitTime != argNULL? rt->rtWaitTime->argVarName : "MACH_MSG_TIMEOUT_NONE");
WriteMsgCheckReceive(file, rt, "MACH_MSG_SUCCESS");
fprintf(file, "\n");
}
static void
WritePackArgType(FILE *file, const argument_t *arg)
{
WritePackMsgType(file, arg->argType,
arg->argType->itIndefinite ? d_NO : arg->argDeallocate,
arg->argLongForm, true,
"InP->%s", "%s", arg->argTTName);
fprintf(file, "\n");
}
static void
WritePackArgValue(FILE *file, const argument_t *arg)
{
const ipc_type_t *it = arg->argType;
const char *ref = arg->argByReferenceUser ? "*" : "";
if (it->itInLine && it->itVarArray) {
if (it->itString) {
fprintf(file,
"\tInP->%s = %smig_strncpy(InP->%s, %s, %d);\n",
arg->argCount->argMsgField,
SubrPrefix,
arg->argMsgField,
arg->argVarName,
it->itNumber);
fprintf(file,
"\tif (InP->%s < %d) InP->%s += 1;\n",
arg->argCount->argMsgField,
it->itNumber,
arg->argCount->argMsgField);
}
else {
const argument_t *count = arg->argCount;
const char *countRef = count->argByReferenceUser ? "*" :"";
const ipc_type_t *btype = it->itElement;
const bool is_64bit_port = IS_64BIT_ABI && btype->itUserlandPort;
fprintf(file, "\tif (%s%s > %d) {\n",
countRef, count->argVarName,
it->itNumber/btype->itNumber);
if (it->itIndefinite) {
fprintf(file, "\t\tInP->%s%s.msgt_inline = FALSE;\n",
arg->argTTName,
arg->argLongForm ? ".msgtl_header" : "");
if (is_64bit_port) {
fprintf(file, "\t\t\n");
fprintf(file, "\t\tInP->%s%s.msgt_size = %d;\n",
arg->argTTName,
arg->argLongForm ? ".msgtl_header" : "",
port_name_size_in_bits);
}
if (arg->argDeallocate == d_YES)
fprintf(file, "\t\tInP->%s%s.msgt_deallocate = TRUE;\n",
arg->argTTName,
arg->argLongForm ? ".msgtl_header" : "");
else if (arg->argDeallocate == d_MAYBE)
fprintf(file, "\t\tInP->%s%s.msgt_deallocate = %s%s;\n",
arg->argTTName,
arg->argLongForm ? ".msgtl_header" : "",
arg->argDealloc->argByReferenceUser ? "*" : "",
arg->argDealloc->argVarName);
fprintf(file, "\t\tInP->%s%s = %s%s;\n",
arg->argMsgField,
OOLPostfix,
ref, arg->argVarName);
if (!arg->argRoutine->rtSimpleFixedRequest)
fprintf(file, "\t\tmsgh_simple = FALSE;\n");
}
else
WriteMsgError(file, arg->argRoutine, "MIG_ARRAY_TOO_LARGE");
fprintf(file, "\t}\n\telse if (%s%s) {\n", countRef, count->argVarName);
if (is_64bit_port) {
fprintf(file, "\t\t\n");
fprintf(file, "\t\tmach_port_name_inlined_t *inlined_%s = (mach_port_name_inlined_t *)InP->%s;\n",
arg->argMsgField, arg->argMsgField);
fprintf(file, "\t\tmach_msg_type_number_t i;\n");
fprintf(file, "\t\tfor (i = 0; i < %s%s; i++) {\n", countRef, count->argVarName);
fprintf(file, "\t\t\t\n");
fprintf(file, "\t\t\tinlined_%s[i].kernel_port_do_not_use = 0;\n",
arg->argMsgField);
fprintf(file, "\t\t\tinlined_%s[i].name = (%s%s)[i];\n", arg->argMsgField, ref, arg->argMsgField);
fprintf(file, "\t\t}\n");
} else {
fprintf(file, "\t\tmemcpy(InP->%s, %s%s, ", arg->argMsgField, ref, arg->argVarName);
if (btype->itTypeSize > 1)
fprintf(file, "%d * ", btype->itTypeSize);
fprintf(file, "%s%s);\n", countRef, count->argVarName);
}
fprintf(file, "\t}\n");
}
}
else if (arg->argMultiplier > 1) {
WriteCopyType(file, it, "InP->%s", " %d * %s%s",
arg->argMsgField, arg->argMultiplier,
ref, arg->argVarName);
} else {
bool is_inlined_port = it->itUserlandPort && it->itInLine;
WriteCopyType(file, it, "InP->%s%s", " %s%s",
arg->argMsgField, is_inlined_port ? ".name" : "", ref, arg->argVarName);
}
fprintf(file, "\n");
}
static void
WriteAdjustMsgSimple(FILE *file, const argument_t *arg)
{
if (!arg->argRoutine->rtSimpleFixedRequest)
{
const char *ref = arg->argByReferenceUser ? "*" : "";
fprintf(file, "\tif (MACH_MSG_TYPE_PORT_ANY(%s%s))\n",
ref, arg->argVarName);
fprintf(file, "\t\tmsgh_simple = FALSE;\n");
fprintf(file, "\n");
}
}
static void
WriteArgSize(FILE *file, const argument_t *arg)
{
const ipc_type_t *ptype = arg->argType;
int bsize = ptype->itElement->itTypeSize;
const argument_t *count = arg->argCount;
if (ptype->itIndefinite) {
fprintf(file, "(InP->%s%s.msgt_inline) ? ",
arg->argTTName, arg->argLongForm ? ".msgtl_header" : "");
}
if (bsize % complex_alignof != 0)
fprintf(file, "(");
if (bsize > 1)
fprintf(file, "%d * ", bsize);
if (ptype->itString)
fprintf(file, "InP->%s", count->argMsgField);
else
fprintf(file, "%s%s",
count->argByReferenceUser ? "*" : "",
count->argVarName);
if (bsize % complex_alignof != 0)
fprintf(file, " + %zd) & ~%zdU", complex_alignof - 1, complex_alignof - 1);
if (ptype->itIndefinite) {
fprintf(file, " : sizeof(%s *)",
FetchUserType(ptype->itElement));
}
}
static void
WriteAdjustMsgSize(FILE *file, const argument_t *arg)
{
const ipc_type_t *ptype = arg->argType;
fprintf(file, "\tmsgh_size_delta = ");
WriteArgSize(file, arg);
fprintf(file, ";\n");
if (arg->argRequestPos == 0)
fprintf(file, "\tmsgh_size = %d + msgh_size_delta;\n",
arg->argRoutine->rtRequestSize);
else
fprintf(file, "\tmsgh_size += msgh_size_delta;\n");
fprintf(file,
"\tInP = (Request *) ((char *) InP + msgh_size_delta - %d);\n",
ptype->itTypeSize + ptype->itPadSize);
}
static void
WriteFinishMsgSize(FILE *file, const argument_t *arg)
{
if (arg->argRequestPos == 0) {
fprintf(file, "\tmsgh_size = %d + (",
arg->argRoutine->rtRequestSize);
WriteArgSize(file, arg);
fprintf(file, ");\n");
}
else {
fprintf(file, "\tmsgh_size += ");
WriteArgSize(file, arg);
fprintf(file, ";\n");
}
}
static void
WriteInitializeCount(FILE *file, const argument_t *arg)
{
const ipc_type_t *ptype = arg->argCInOut->argParent->argType;
const ipc_type_t *btype = ptype->itElement;
fprintf(file, "\tif (%s%s < %d)\n",
arg->argByReferenceUser ? "*" : "",
arg->argVarName,
ptype->itNumber/btype->itNumber);
fprintf(file, "\t\tInP->%s = %s%s;\n",
arg->argMsgField,
arg->argByReferenceUser ? "*" : "",
arg->argVarName);
fprintf(file, "\telse\n");
fprintf(file, "\t\tInP->%s = %d;\n",
arg->argMsgField, ptype->itNumber/btype->itNumber);
fprintf(file, "\n");
}
static void
WritePackArg(FILE *file, const argument_t *arg)
{
if (akCheck(arg->argKind, akbRequest))
WritePackArgType(file, arg);
if ((akIdent(arg->argKind) == akePoly) &&
akCheckAll(arg->argKind, akbSendSnd|akbUserArg))
WriteAdjustMsgSimple(file, arg);
if ((akIdent(arg->argKind) == akeCountInOut) &&
akCheck(arg->argKind, akbSendSnd))
WriteInitializeCount(file, arg);
else if (akCheckAll(arg->argKind, akbSendSnd|akbSendBody))
WritePackArgValue(file, arg);
}
static void
WriteRequestArgs(FILE *file, const routine_t *rt)
{
const argument_t *arg;
const argument_t *lastVarArg;
lastVarArg = argNULL;
for (arg = rt->rtArgs; arg != argNULL; arg = arg->argNext)
{
if (lastVarArg != argNULL &&
lastVarArg->argRequestPos < arg->argRequestPos)
{
WriteAdjustMsgSize(file, lastVarArg);
lastVarArg = argNULL;
}
WritePackArg(file, arg);
if (akCheckAll(arg->argKind, akbSendSnd|akbSendBody|akbVariable))
lastVarArg = arg;
}
if (lastVarArg != argNULL)
WriteFinishMsgSize(file, lastVarArg);
}
static void
WriteCheckIdentity(FILE *file, const routine_t *rt)
{
fprintf(file, "\tif (mig_unlikely (OutP->Head.msgh_id != %d)) {\n",
rt->rtNumber + SubsystemBase + 100);
fprintf(file, "\t\tif (OutP->Head.msgh_id == MACH_NOTIFY_SEND_ONCE)\n\t");
WriteMsgError(file, rt, "MIG_SERVER_DIED");
fprintf(file, "\t\telse {\n");
fprintf(file, "\t\t\t%smig_dealloc_reply_port(%s);\n\t",
SubrPrefix,"InP->Head.msgh_reply_port");
WriteMsgError(file, rt, "MIG_REPLY_MISMATCH");
fprintf(file, "\t\t}\n\t}\n");
fprintf(file, "\n");
fprintf(file, "#if\tTypeCheck\n");
if (rt->rtSimpleCheckReply && rt->rtSimpleReceiveReply)
{
if (!rt->rtNoReplyArgs)
fprintf(file, "\tmsgh_size = OutP->Head.msgh_size;\n\n");
fprintf(file,
"\tif (mig_unlikely ("
"(OutP->Head.msgh_bits & MACH_MSGH_BITS_COMPLEX) ||\n");
if (rt->rtNoReplyArgs)
fprintf(file, "\t    (OutP->Head.msgh_size != %d)))\n",
rt->rtReplySize);
else {
fprintf(file, "\t    ((msgh_size %s %d) &&\n",
(rt->rtNumReplyVar > 0) ? "<" : "!=",
rt->rtReplySize);
fprintf(file, "\t     ((msgh_size != sizeof(mig_reply_header_t)) ||\n");
fprintf(file, "\t      (OutP->RetCode == KERN_SUCCESS)))))\n");
}
}
else {
fprintf(file, "\tmsgh_size = OutP->Head.msgh_size;\n");
fprintf(file, "\tmsgh_simple = !(OutP->Head.msgh_bits & MACH_MSGH_BITS_COMPLEX);\n");
fprintf(file, "\n");
fprintf(file, "\tif (mig_unlikely (((msgh_size %s %d)",
(rt->rtNumReplyVar > 0) ? "<" : "!=",
rt->rtReplySize);
if (rt->rtSimpleCheckReply)
fprintf(file, " || msgh_simple");
fprintf(file, ") &&\n");
fprintf(file, "\t    ((msgh_size != sizeof(mig_reply_header_t)) ||\n");
fprintf(file, "\t     !msgh_simple ||\n");
fprintf(file, "\t     (OutP->RetCode == KERN_SUCCESS))))\n");
}
WriteMsgError(file, rt, "MIG_TYPE_ERROR");
fprintf(file, "#endif\t\n");
fprintf(file, "\n");
}
static void
WriteRetCodeCheck(FILE *file, const routine_t *rt)
{
fprintf(file, "\tif (OutP->RetCode != KERN_SUCCESS)\n");
WriteMsgError(file, rt, "OutP->RetCode");
fprintf(file, "\n");
}
static void
WriteTypeCheck(FILE *file, const argument_t *arg)
{
const ipc_type_t *it = arg->argType;
const routine_t *rt = arg->argRoutine;
fprintf(file, "#if\tTypeCheck\n");
if (akCheck(arg->argKind, akbReplyQC))
{
fprintf(file, "\tif (BAD_TYPECHECK (&OutP->%s, &%sCheck))\n",
arg->argTTName, arg->argVarName);
}
else
{
fprintf(file, "\tif (mig_unlikely (");
if (!it->itIndefinite) {
fprintf(file, "(OutP->%s%s.msgt_inline != %s) ||\n\t    ",
arg->argTTName,
arg->argLongForm ? ".msgtl_header" : "",
strbool(it->itInLine));
}
fprintf(file, "(OutP->%s%s.msgt_longform != %s) ||\n",
arg->argTTName,
arg->argLongForm ? ".msgtl_header" : "",
strbool(arg->argLongForm));
if (it->itOutName == MACH_MSG_TYPE_POLYMORPHIC)
{
if (!rt->rtSimpleCheckReply)
fprintf(file, "\t    (MACH_MSG_TYPE_PORT_ANY(OutP->%s.msgt%s_name) && msgh_simple) ||\n",
arg->argTTName,
arg->argLongForm ? "l" : "");
}
else
fprintf(file, "\t    (OutP->%s.msgt%s_name != %s) ||\n",
arg->argTTName,
arg->argLongForm ? "l" : "",
it->itOutNameStr);
if (!it->itVarArray)
fprintf(file, "\t    (OutP->%s.msgt%s_number != %d) ||\n",
arg->argTTName,
arg->argLongForm ? "l" : "",
it->itNumber);
if (IS_64BIT_ABI && it->itUserlandPort && arg->argLongForm) {
fprintf(file, "\t    (OutP->%s.msgtl_size != %d && OutP->%s.msgtl_header.msgt_inline == TRUE) || \n",
arg->argTTName,
it->itSize,
arg->argTTName);
fprintf(file, "\t    (OutP->%s.msgtl_size != %d && OutP->%s.msgtl_header.msgt_inline == FALSE)",
arg->argTTName,
port_name_size_in_bits,
arg->argTTName);
} else {
fprintf(file, "\t    (OutP->%s.msgt%s_size != %d)",
arg->argTTName,
arg->argLongForm ? "l" : "",
it->itSize);
}
fprintf(file, "))\n");
}
WriteMsgError(file, rt, "MIG_TYPE_ERROR");
fprintf(file, "#endif\t\n");
fprintf(file, "\n");
}
static void
WriteCheckArgSize(FILE *file, const argument_t *arg)
{
const ipc_type_t *ptype = arg->argType;
const ipc_type_t *btype = ptype->itElement;
const argument_t *count = arg->argCount;
int multiplier = btype->itTypeSize / btype->itNumber;
if (ptype->itIndefinite) {
fprintf(file, "(OutP->%s%s.msgt_inline) ? ",
arg->argTTName, arg->argLongForm ? ".msgtl_header" : "");
}
if (btype->itTypeSize % complex_alignof != 0)
fprintf(file, "(");
if (multiplier > 1)
fprintf(file, "%d * ", multiplier);
fprintf(file, "OutP->%s", count->argMsgField);
if (btype->itTypeSize % complex_alignof != 0)
fprintf(file, " + %zd) & ~%zdU", complex_alignof - 1, complex_alignof - 1);
if (ptype->itIndefinite)
fprintf(file, " : sizeof(%s *)", FetchUserType(btype));
}
static void
WriteCheckMsgSize(FILE *file, const argument_t *arg)
{
const routine_t *rt = arg->argRoutine;
if (arg->argReplyPos == rt->rtMaxReplyPos)
{
fprintf(file, "#if\tTypeCheck\n");
fprintf(file, "\tif (mig_unlikely (msgh_size != %d + (",
rt->rtReplySize);
WriteCheckArgSize(file, arg);
fprintf(file, ")))\n");
WriteMsgError(file, rt, "MIG_TYPE_ERROR");
fprintf(file, "#endif\t\n");
}
else
{
bool LastVarArg = arg->argReplyPos+1 == rt->rtNumReplyVar;
fprintf(file, "\tmsgh_size_delta = ");
WriteCheckArgSize(file, arg);
fprintf(file, ";\n");
fprintf(file, "#if\tTypeCheck\n");
if (LastVarArg)
fprintf(file,
"\tif (mig_unlikely (msgh_size != %d + msgh_size_delta))\n",
rt->rtReplySize);
else
fprintf(file,
"\tif (mig_unlikely (msgh_size < %d + msgh_size_delta))\n",
rt->rtReplySize);
WriteMsgError(file, rt, "MIG_TYPE_ERROR");
if (!LastVarArg)
fprintf(file, "\tmsgh_size -= msgh_size_delta;\n");
fprintf(file, "#endif\t\n");
}
fprintf(file, "\n");
}
static void
WriteExtractArgValueThroughCopy(FILE *file, const argument_t *arg, const argument_t *count,
const ipc_type_t *btype, const char *ref, const bool is_64bit_port)
{
if (is_64bit_port) {
fprintf(file, "\t\t\n");
fprintf(file, "\t\tmach_port_name_inlined_t *inlined_%s = (mach_port_name_inlined_t *)OutP->%s;\n",
arg->argMsgField, arg->argMsgField);
fprintf(file, "\t\tmach_msg_type_number_t i;\n");
fprintf(file, "\t\tfor (i = 0; i < OutP->%s; i++) {\n", count->argMsgField);
fprintf(file, "\t\t\t(%s%s)[i] = inlined_%s[i].name;\n",
ref, arg->argVarName, arg->argMsgField);
fprintf(file, "\t\t}\n");
} else {
fprintf(file, "\t\tmemcpy(%s%s, OutP->%s, ", ref, arg->argVarName,
arg->argMsgField);
if (btype->itTypeSize != btype->itNumber)
fprintf(file, "%d * ", btype->itTypeSize/btype->itNumber);
fprintf(file, "OutP->%s);\n", count->argMsgField);
}
}
static void
WriteExtractArgValue(FILE *file, const argument_t *arg)
{
const ipc_type_t	*argType = arg->argType;
const char *ref = arg->argByReferenceUser ? "*" : "";
if (argType->itInLine && argType->itVarArray) {
if (argType->itString) {
fprintf(file, "\t(void) %smig_strncpy(%s%s, OutP->%s, %d);\n",
SubrPrefix,
ref,
arg->argVarName,
arg->argMsgField,
argType->itNumber);
}
else if (argType->itIndefinite) {
const argument_t *count = arg->argCount;
const char *countRef = count->argByReferenceUser ? "*" : "";
const ipc_type_t *btype = argType->itElement;
const bool is_64bit_port = IS_64BIT_ABI && btype->itUserlandPort;
fprintf(file, "\tif (!OutP->%s%s.msgt_inline)\n",
arg->argTTName,
arg->argLongForm ? ".msgtl_header" : "");
fprintf(file, "\t\t%s%s = OutP->%s%s;\n",
ref, arg->argVarName,
arg->argMsgField,
OOLPostfix);
fprintf(file, "\telse if (OutP->%s", count->argMsgField);
if (btype->itNumber > 1)
fprintf(file, " / %d", btype->itNumber);
fprintf(file, " > %s%s) {\n", countRef, count->argVarName);
fprintf(file, "\t\t%smig_allocate((vm_offset_t *)%s, ",
SubrPrefix, arg->argVarName);
if (is_64bit_port)
fprintf(file, "%d * ", port_name_size);
else if (btype->itTypeSize != btype->itNumber)
fprintf(file, "%d * ", btype->itTypeSize/btype->itNumber);
fprintf(file, "OutP->%s);\n", count->argMsgField);
WriteExtractArgValueThroughCopy(file, arg, count, btype, ref, is_64bit_port);
fprintf(file, "\t}\n");
fprintf(file, "\telse if (OutP->%s) {\n", count->argMsgField);
WriteExtractArgValueThroughCopy(file, arg, count, btype, ref, is_64bit_port);
fprintf(file, "\t}\n");
}
else {
const argument_t *count = arg->argCount;
const char *countRef = count->argByReferenceUser ? "*" :"";
const ipc_type_t *btype = argType->itElement;
fprintf(file, "\tif (OutP->%s", count->argMsgField);
if (btype->itNumber > 1)
fprintf(file, " / %d", btype->itNumber);
fprintf(file, " > %s%s) {\n",
countRef, count->argVarName);
fprintf(file, "\t\tif (%s%s)\n", countRef, count->argVarName);
fprintf(file, "\t\t\tmemcpy(%s%s, OutP->%s, ", ref, arg->argVarName,
arg->argMsgField);
if (btype->itTypeSize > 1)
fprintf(file, "%d * ", btype->itTypeSize);
fprintf(file, "%s%s);\n",
countRef, count->argVarName);
fprintf(file, "\t\t%s%s = OutP->%s",
countRef, count->argVarName, count->argMsgField);
if (btype->itNumber > 1)
fprintf(file, " / %d", btype->itNumber);
fprintf(file, ";\n");
WriteMsgError(file,arg->argRoutine, "MIG_ARRAY_TOO_LARGE");
fprintf(file, "\t}\n\telse if (OutP->%s) {\n", count->argMsgField);
fprintf(file, "\t\tmemcpy(%s%s, OutP->%s, ", ref, arg->argVarName,
arg->argMsgField);
if (btype->itTypeSize != btype->itNumber)
fprintf(file, "%d * ", btype->itTypeSize/btype->itNumber);
fprintf(file, "OutP->%s);\n", count->argMsgField);
fprintf(file, "\t}\n");
}
}
else if (arg->argMultiplier > 1) {
WriteCopyType(file, argType,
"%s%s", " OutP->%s / %d",
ref, arg->argVarName, arg->argMsgField,
arg->argMultiplier);
} else {
bool is_inlined_port = argType->itUserlandPort && argType->itInLine;
WriteCopyType(file, argType,
"%s%s", " OutP->%s%s",
ref, arg->argVarName, arg->argMsgField, is_inlined_port ? ".name" : "");
}
fprintf(file, "\n");
}
static void
WriteExtractArg(FILE *file, const argument_t *arg)
{
const routine_t *rt = arg->argRoutine;
if (akCheck(arg->argKind, akbReply))
WriteTypeCheck(file, arg);
if (akCheckAll(arg->argKind, akbVariable|akbReply))
WriteCheckMsgSize(file, arg);
if (arg == rt->rtRetCode)
WriteRetCodeCheck(file, rt);
if (akCheckAll(arg->argKind, akbReturnRcv))
WriteExtractArgValue(file, arg);
}
static void
WriteAdjustReplyMsgPtr(FILE *file, const argument_t *arg)
{
const ipc_type_t *ptype = arg->argType;
fprintf(file,
"\tOutP = (Reply *) ((char *) OutP + msgh_size_delta - %d);\n\n",
ptype->itTypeSize + ptype->itPadSize);
}
static void
WriteReplyArgs(FILE *file, const routine_t *rt)
{
const argument_t *arg;
const argument_t *lastVarArg;
lastVarArg = argNULL;
for (arg = rt->rtArgs; arg != argNULL; arg = arg->argNext) {
if (lastVarArg != argNULL &&
lastVarArg->argReplyPos < arg->argReplyPos)
{
WriteAdjustReplyMsgPtr(file, lastVarArg);
lastVarArg = argNULL;
}
WriteExtractArg(file, arg);
if (akCheckAll(arg->argKind, akbReturnRcv|akbVariable))
lastVarArg = arg;
}
}
static void
WriteReturnValue(FILE *file, const routine_t *rt)
{
if (rt->rtReturn == rt->rtRetCode)
fprintf(file, "\treturn KERN_SUCCESS;\n");
else
{
if (rt->rtNumReplyVar > 0)
fprintf(file, "\tOutP = &Mess.Out;\n");
fprintf(file, "\treturn OutP->%s;\n", rt->rtReturn->argMsgField);
}
}
static void
WriteFieldDecl(FILE *file, const argument_t *arg)
{
WriteFieldDeclPrim(file, arg, FetchUserType);
}
static void
WriteStubDecl(FILE *file, const routine_t *rt)
{
fprintf(file, "\n");
fprintf(file, "\n", rtRoutineKindToStr(rt->rtKind), rt->rtName);
fprintf(file, "mig_external %s %s\n", ReturnTypeStr(rt), rt->rtUserName);
fprintf(file, "(\n");
WriteList(file, rt->rtArgs, WriteUserVarDecl, akbUserArg, ",\n", "\n");
fprintf(file, ")\n");
fprintf(file, "{\n");
}
static void
WriteRoutine(FILE *file, const routine_t *rt)
{
WriteStubDecl(file, rt);
WriteStructDecl(file, rt->rtArgs, WriteFieldDecl, akbRequest, "Request");
if (!rt->rtOneWay)
WriteStructDecl(file, rt->rtArgs, WriteFieldDecl, akbReply, "Reply");
WriteVarDecls(file, rt);
WriteList(file, rt->rtArgs, WriteTypeDeclInUser, akbRequest, "\n", "\n");
if (!rt->rtOneWay)
WriteList(file, rt->rtArgs, WriteCheckDecl, akbReplyQC, "\n", "\n");
WriteRequestArgs(file, rt);
WriteRequestHead(file, rt);
fprintf(file, "\n");
if (rt->rtOneWay)
WriteMsgSend(file, rt);
else
{
WriteMsgRPC(file, rt);
WriteCheckIdentity(file, rt);
if (rt->rtNoReplyArgs)
{
WriteTypeCheck(file, rt->rtRetCode);
fprintf(file, "\treturn OutP->RetCode;\n");
}
else {
WriteReplyArgs(file, rt);
WriteReturnValue(file, rt);
}
}
fprintf(file, "}\n");
}
void
WriteUser(FILE *file, const statement_t *stats)
{
const statement_t *stat;
WriteProlog(file);
for (stat = stats; stat != stNULL; stat = stat->stNext)
switch (stat->stKind)
{
case skRoutine:
WriteRoutine(file, stat->stRoutine);
break;
case skImport:
case skUImport:
WriteImport(file, stat->stFileName);
break;
case skSImport:
break;
default:
fatal("WriteUser(): bad statement_kind_t (%d)",
(int) stat->stKind);
}
WriteEpilog(file);
}
void
WriteUserIndividual(const statement_t *stats)
{
const statement_t *stat;
for (stat = stats; stat != stNULL; stat = stat->stNext)
switch (stat->stKind)
{
case skRoutine:
{
FILE *file;
char *filename;
filename = strconcat(UserFilePrefix,
strconcat(stat->stRoutine->rtName, ".c"));
file = fopen(filename, "w");
if (file == NULL)
fatal("fopen(%s): %s", filename,
unix_error_string(errno));
WriteProlog(file);
{
const statement_t *s;
for (s = stats; s != stNULL; s = s->stNext)
switch (s->stKind)
{
case skImport:
case skUImport:
WriteImport(file, s->stFileName);
break;
default:
break;
}
}
WriteRoutine(file, stat->stRoutine);
WriteEpilog(file);
if (ferror(file) || fclose(file))
fatal("fclose(%s): %s", filename,
unix_error_string(errno));
strfree(filename);
}
break;
case skImport:
case skUImport:
break;
case skSImport:
break;
default:
fatal("WriteUserIndividual(): bad statement_kind_t (%d)",
(int) stat->stKind);
}
}