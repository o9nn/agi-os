typedef struct Box Box;
typedef struct Header Header;
typedef struct MAddr MAddr;
typedef struct MbLock MbLock;
typedef struct MimeHdr MimeHdr;
typedef struct Msg Msg;
typedef struct NamedInt NamedInt;
typedef struct Pair Pair;
enum
{
StrAlloc = 32,
BufSize = 8*1024,
NDigest = 40,
NUid = 10,
NFlags = 8,
LockSecs = 5 * 60,
MboxNameLen = 256,
MsgNameLen = 32,
UserNameLen = 64,
MUtf7Max = 6,
MSeen = 1 << 0,
MAnswered = 1 << 1,
MFlagged = 1 << 2,
MDeleted = 1 << 3,
MDraft = 1 << 4,
MRecent = 1 << 5,
NotBogus = 0,
BogusHeader = 1,
BogusBody = 2,
BogusTried = 4,
};
struct Box
{
char *name;
char *fs;
char *fsDir;
char *imp;
uchar writable;
uchar dirtyImp;
uchar sendFlags;
Qid qid;
Qid impQid;
long mtime;
ulong max;
ulong toldMax;
ulong recent;
ulong toldRecent;
ulong uidnext;
ulong uidvalidity;
Msg *msgs;
};
enum
{
IFrom,
ITo,
ICc,
IReplyTo,
IUnixDate,
ISubject,
IType,
IDisposition,
IFilename,
IDigest,
IBcc,
IInReplyTo,
IDate,
ISender,
IMessageId,
ILines,
IMax
};
struct Header
{
char *buf;
ulong size;
ulong lines;
MimeHdr *type;
MimeHdr *id;
MimeHdr *description;
MimeHdr *encoding;
MimeHdr *md5;
MimeHdr *disposition;
MimeHdr *language;
};
struct Msg
{
Msg *next;
Msg *prev;
Msg *kids;
Msg *parent;
char *fsDir;
Header head;
Header mime;
int flags;
uchar sendFlags;
uchar expunged;
uchar matched;
uchar bogus;
ulong uid;
ulong seq;
ulong id;
char *fs;
char *efs;
ulong size;
ulong lines;
char *iBuf;
char *info[IMax];
char *unixDate;
MAddr *unixFrom;
MAddr *to;
MAddr *from;
MAddr *replyTo;
MAddr *sender;
MAddr *cc;
MAddr *bcc;
};
struct MAddr
{
char *personal;
char *box;
char *host;
MAddr *next;
};
struct MimeHdr
{
char *s;
char *t;
MimeHdr *next;
};
struct NamedInt
{
char *name;
int v;
};
struct MbLock
{
int fd;
};
typedef struct Fetch Fetch;
typedef struct NList NList;
typedef struct SList SList;
typedef struct MsgSet MsgSet;
typedef struct Store Store;
typedef struct Search Search;
enum
{
FEnvelope,
FFlags,
FInternalDate,
FRfc822,
FRfc822Head,
FRfc822Size,
FRfc822Text,
FBodyStruct,
FUid,
FBody,
FBodySect,
FBodyPeek,
FMax
};
enum
{
FPAll,
FPHead,
FPHeadFields,
FPHeadFieldsNot,
FPMime,
FPText,
FPMax
};
struct Fetch
{
uchar op;
uchar part;
uchar partial;
long start;
long size;
NList *sect;
SList *hdrs;
Fetch *next;
};
enum{
SMessages = 1 << 0,
SRecent = 1 << 1,
SUidNext = 1 << 2,
SUidValidity = 1 << 3,
SUnseen = 1 << 4,
};
enum
{
STFlags,
STFlagsSilent,
STMax
};
struct Store
{
uchar sign;
uchar op;
int flags;
};
enum
{
SKNone,
SKCharset,
SKAll,
SKAnswered,
SKBcc,
SKBefore,
SKBody,
SKCc,
SKDeleted,
SKDraft,
SKFlagged,
SKFrom,
SKHeader,
SKKeyword,
SKLarger,
SKNew,
SKNot,
SKOld,
SKOn,
SKOr,
SKRecent,
SKSeen,
SKSentBefore,
SKSentOn,
SKSentSince,
SKSet,
SKSince,
SKSmaller,
SKSubject,
SKText,
SKTo,
SKUid,
SKUnanswered,
SKUndeleted,
SKUndraft,
SKUnflagged,
SKUnkeyword,
SKUnseen,
SKMax
};
struct Search
{
int key;
char *s;
char *hdr;
ulong num;
int year;
int mon;
int mday;
MsgSet *set;
Search *left;
Search *right;
Search *next;
};
struct NList
{
ulong n;
NList *next;
};
struct SList
{
char *s;
SList *next;
};
struct MsgSet
{
ulong from;
ulong to;
MsgSet *next;
};
struct Pair
{
ulong start;
ulong stop;
};
#include "bin.h"
extern Bin *parseBin;
extern Biobuf bout;
extern Biobuf bin;
extern char username[UserNameLen];
extern char mboxDir[MboxNameLen];
extern char *fetchPartNames[FPMax];
extern char *site;
extern char *remote;
extern int debug;
#include "fns.h"