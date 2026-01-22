#ifndef SLANG_COM_HELPER_H
#define SLANG_COM_HELPER_H
#include "slang.h"
#include <atomic>
#ifndef SLANG_HANDLE_RESULT_FAIL
#define SLANG_HANDLE_RESULT_FAIL(x)
#endif
#define SLANG_RETURN_ON_FAIL(x)             \
{                                       \
SlangResult _res = (x);             \
if (SLANG_FAILED(_res))             \
{                                   \
SLANG_HANDLE_RESULT_FAIL(_res); \
return _res;                    \
}                                   \
}
#define SLANG_RETURN_VOID_ON_FAIL(x)        \
{                                       \
SlangResult _res = (x);             \
if (SLANG_FAILED(_res))             \
{                                   \
SLANG_HANDLE_RESULT_FAIL(_res); \
return;                         \
}                                   \
}
#define SLANG_RETURN_FALSE_ON_FAIL(x)       \
{                                       \
SlangResult _res = (x);             \
if (SLANG_FAILED(_res))             \
{                                   \
SLANG_HANDLE_RESULT_FAIL(_res); \
return false;                   \
}                                   \
}
#define SLANG_RETURN_NULL_ON_FAIL(x)        \
{                                       \
SlangResult _res = (x);             \
if (SLANG_FAILED(_res))             \
{                                   \
SLANG_HANDLE_RESULT_FAIL(_res); \
return nullptr;                 \
}                                   \
}
#define SLANG_ASSERT_ON_FAIL(x) \
{                           \
SlangResult _res = (x); \
if (SLANG_FAILED(_res)) \
{                       \
assert(false);      \
return _res;        \
}                       \
}
#define SLANG_ASSERT_VOID_ON_FAIL(x) \
{                                \
SlangResult _res = (x);      \
if (SLANG_FAILED(_res))      \
{                            \
assert(false);           \
return;                  \
}                            \
}
#if defined(__cplusplus)
namespace Slang
{
typedef SlangResult Result;
typedef SlangUUID Guid;
}
SLANG_FORCE_INLINE bool operator==(const Slang::Guid& aIn, const Slang::Guid& bIn)
{
using namespace Slang;
typedef uint32_t CmpType;
union GuidCompare
{
Guid guid;
CmpType data[sizeof(Guid) / sizeof(CmpType)];
};
const CmpType* a = reinterpret_cast<const GuidCompare&>(aIn).data;
const CmpType* b = reinterpret_cast<const GuidCompare&>(bIn).data;
return ((a[0] ^ b[0]) | (a[1] ^ b[1]) | (a[2] ^ b[2]) | (a[3] ^ b[3])) == 0;
}
SLANG_FORCE_INLINE bool operator!=(const Slang::Guid& a, const Slang::Guid& b)
{
return !(a == b);
}
#define SLANG_IUNKNOWN_QUERY_INTERFACE                     \
SLANG_NO_THROW SlangResult SLANG_MCALL queryInterface( \
SlangUUID const& uuid,                             \
void** outObject) SLANG_OVERRIDE                   \
{                                                      \
ISlangUnknown* intf = getInterface(uuid);          \
if (intf)                                          \
{                                                  \
addRef();                                      \
*outObject = intf;                             \
return SLANG_OK;                               \
}                                                  \
return SLANG_E_NO_INTERFACE;                       \
}
#define SLANG_IUNKNOWN_ADD_REF                   \
SLANG_NO_THROW uint32_t SLANG_MCALL addRef() \
{                                            \
return ++m_refCount;                     \
}
#define SLANG_IUNKNOWN_RELEASE                    \
SLANG_NO_THROW uint32_t SLANG_MCALL release() \
{                                             \
--m_refCount;                             \
if (m_refCount == 0)                      \
{                                         \
delete this;                          \
return 0;                             \
}                                         \
return m_refCount;                        \
}
#define SLANG_IUNKNOWN_ALL         \
SLANG_IUNKNOWN_QUERY_INTERFACE \
SLANG_IUNKNOWN_ADD_REF         \
SLANG_IUNKNOWN_RELEASE
#define SLANG_REF_OBJECT_IUNKNOWN_QUERY_INTERFACE          \
SLANG_NO_THROW SlangResult SLANG_MCALL queryInterface( \
SlangUUID const& uuid,                             \
void** outObject) SLANG_OVERRIDE                   \
{                                                      \
void* intf = getInterface(uuid);                   \
if (intf)                                          \
{                                                  \
addReference();                                \
*outObject = intf;                             \
return SLANG_OK;                               \
}                                                  \
return SLANG_E_NO_INTERFACE;                       \
}
#define SLANG_REF_OBJECT_IUNKNOWN_ADD_REF                       \
SLANG_NO_THROW uint32_t SLANG_MCALL addRef() SLANG_OVERRIDE \
{                                                           \
return (uint32_t)addReference();                        \
}
#define SLANG_REF_OBJECT_IUNKNOWN_RELEASE                        \
SLANG_NO_THROW uint32_t SLANG_MCALL release() SLANG_OVERRIDE \
{                                                            \
return (uint32_t)releaseReference();                     \
}
#define SLANG_REF_OBJECT_IUNKNOWN_ALL         \
SLANG_REF_OBJECT_IUNKNOWN_QUERY_INTERFACE \
SLANG_REF_OBJECT_IUNKNOWN_ADD_REF         \
SLANG_REF_OBJECT_IUNKNOWN_RELEASE
#endif
#endif