#ifndef	_ROUTINE_H
#define	_ROUTINE_H
#include <stdbool.h>
#include <sys/types.h>
#include "type.h"
#define akeNone		(0)
#define akeNormal	(1)
#define akeRequestPort	(2)
#define akeWaitTime	(3)
#define akeReplyPort	(4)
#define akeMsgOption	(5)
#define akeMsgSeqno	(6)
#define akeRetCode	(7)
#define akeReturn	(8)
#define akeCount	(9)
#define akePoly		(10)
#define	akeDealloc	(11)
#define	akeServerCopy	(12)
#define akeCountInOut	(13)
#define	akeBITS		(0x0000003f)
#define	akbRequest	(0x00000040)
#define	akbReply	(0x00000080)
#define	akbUserArg	(0x00000100)
#define	akbServerArg	(0x00000200)
#define akbSend		(0x00000400)
#define akbSendBody	(0x00000800)
#define akbSendSnd	(0x00001000)
#define akbSendRcv	(0x00002000)
#define akbReturn	(0x00004000)
#define akbReturnBody	(0x00008000)
#define akbReturnSnd	(0x00010000)
#define akbReturnRcv	(0x00020000)
#define akbReplyInit	(0x00040000)
#define akbRequestQC	(0x00080000)
#define akbReplyQC	(0x00100000)
#define akbReplyCopy	(0x00200000)
#define akbVarNeeded	(0x00400000)
#define akbDestroy	(0x00800000)
#define akbVariable	(0x01000000)
#define	akbIndefinite	(0x02000000)
#define	akbPointer	(0x04000000)
typedef u_int  arg_kind_t;
#define akbNone		(0)
#define akbAll		(~akbNone)
#define akbAllBits	(~akeBITS)
#define akbSendBits	(akbSend|akbSendBody|akbSendSnd|akbSendRcv)
#define akbReturnBits	(akbReturn|akbReturnBody|akbReturnSnd|akbReturnRcv)
#define akbSendReturnBits	(akbSendBits|akbReturnBits)
#define akNone		akeNone
#define akIn		akAddFeature(akeNormal,				\
akbUserArg|akbServerArg|akbRequest|akbSendBits)
#define akOut		akAddFeature(akeNormal,				\
akbUserArg|akbServerArg|akbReply|akbReturnBits|akbReplyInit)
#define akInOut		akAddFeature(akeNormal,				\
akbUserArg|akbServerArg|akbRequest|akbReply|			\
akbSendBits|akbReturnBits|akbReplyInit|akbReplyCopy)
#define akRequestPort	akAddFeature(akeRequestPort,			\
akbUserArg|akbServerArg|akbSend|akbSendSnd|akbSendRcv)
#define akWaitTime	akAddFeature(akeWaitTime, akbUserArg)
#define akMsgOption	akAddFeature(akeMsgOption, akbUserArg)
#define akMsgSeqno	akAddFeature(akeMsgSeqno,			\
akbServerArg|akbSend|akbSendRcv)
#define akReplyPort	akAddFeature(akeReplyPort,			\
akbUserArg|akbServerArg|akbSend|akbSendSnd|akbSendRcv)
#define akUReplyPort	akAddFeature(akeReplyPort,			\
akbUserArg|akbSend|akbSendSnd|akbSendRcv)
#define akSReplyPort	akAddFeature(akeReplyPort,			\
akbServerArg|akbSend|akbSendSnd|akbSendRcv)
#define akRetCode	akAddFeature(akeRetCode, akbReply)
#define akReturn	akAddFeature(akeReturn,				\
akbReply|akbReplyInit)
#define akCount		akAddFeature(akeCount,				\
akbUserArg|akbServerArg)
#define akPoly		akePoly
#define	akDealloc	akAddFeature(akeDealloc, akbUserArg)
#define	akServerCopy	akAddFeature(akeServerCopy, akbServerArg|akbSendRcv)
#define akCountInOut	akAddFeature(akeCountInOut, akbRequest|akbSendBits)
#define	akCheck(ak, bits)	((ak) & (bits))
#define akCheckAll(ak, bits)	(akCheck(ak, bits) == (bits))
#define akAddFeature(ak, bits)	((ak)|(bits))
#define akRemFeature(ak, bits)	((ak)&~(bits))
#define akIdent(ak)		((ak) & akeBITS)
typedef struct argument
{
identifier_t argName;
struct argument *argNext;
arg_kind_t argKind;
ipc_type_t *argType;
const_string_t argVarName;
const_string_t argMsgField;
const_string_t argTTName;
const_string_t argPadName;
ipc_flags_t argFlags;
dealloc_t argDeallocate;
bool argLongForm;
bool argServerCopy;
bool argCountInOut;
struct routine *argRoutine;
struct argument *argCount;
struct argument *argCInOut;
struct argument *argPoly;
struct argument *argDealloc;
struct argument *argSCopy;
struct argument *argParent;
int argMultiplier;
int argRequestPos;
int argReplyPos;
bool	argByReferenceUser;
bool	argByReferenceServer;
} argument_t;
typedef enum
{
rkRoutine,
rkSimpleRoutine,
} routine_kind_t;
typedef struct routine
{
identifier_t rtName;
routine_kind_t rtKind;
argument_t *rtArgs;
u_int rtNumber;
identifier_t rtUserName;
identifier_t rtServerName;
bool rtOneWay;
bool rtSimpleFixedRequest;
bool rtSimpleSendRequest;
bool rtSimpleCheckRequest;
bool rtSimpleReceiveRequest;
bool rtSimpleFixedReply;
bool rtSimpleSendReply;
bool rtSimpleCheckReply;
bool rtSimpleReceiveReply;
u_int rtRequestSize;
u_int rtReplySize;
int rtNumRequestVar;
int rtNumReplyVar;
int rtMaxRequestPos;
int rtMaxReplyPos;
bool rtNoReplyArgs;
argument_t *rtRequestPort;
argument_t *rtUReplyPort;
argument_t *rtSReplyPort;
argument_t *rtReturn;
argument_t *rtServerReturn;
argument_t *rtRetCode;
argument_t *rtWaitTime;
argument_t *rtMsgOption;
argument_t *rtMsgSeqno;
} routine_t;
#define rtNULL		((routine_t *) 0)
#define argNULL		((argument_t *) 0)
extern u_int rtNumber;
extern routine_t *rtAlloc(void);
extern void rtSkip(int);
extern argument_t *argAlloc(void);
extern bool rtCheckMask(const argument_t *args, u_int mask);
extern bool rtCheckMaskFunction(const argument_t *args, u_int mask,
bool (*func)(const argument_t *arg));
extern routine_t *rtMakeRoutine(identifier_t name, argument_t *args);
extern routine_t *rtMakeSimpleRoutine(identifier_t name, argument_t *args);
extern void rtPrintRoutine(const routine_t *rt);
extern void rtCheckRoutine(routine_t *rt);
extern const char *rtRoutineKindToStr(routine_kind_t rk);
#endif