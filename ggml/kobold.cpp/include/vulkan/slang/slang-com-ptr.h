#ifndef SLANG_COM_PTR_H
#define SLANG_COM_PTR_H
#include "slang-com-helper.h"
#include <assert.h>
#include <cstddef>
namespace Slang
{
enum InitAttach
{
INIT_ATTACH
};
template<class T>
class ComPtr
{
public:
typedef T Type;
typedef ComPtr ThisType;
typedef ISlangUnknown* Ptr;
SLANG_FORCE_INLINE ComPtr()
: m_ptr(nullptr)
{
}
SLANG_FORCE_INLINE ComPtr(std::nullptr_t)
: m_ptr(nullptr)
{
}
SLANG_FORCE_INLINE explicit ComPtr(T* ptr)
: m_ptr(ptr)
{
if (ptr)
((Ptr)ptr)->addRef();
}
SLANG_FORCE_INLINE ComPtr(const ThisType& rhs)
: m_ptr(rhs.m_ptr)
{
if (m_ptr)
((Ptr)m_ptr)->addRef();
}
SLANG_FORCE_INLINE explicit ComPtr(InitAttach, T* ptr)
: m_ptr(ptr)
{
}
SLANG_FORCE_INLINE ComPtr(InitAttach, const ThisType& rhs)
: m_ptr(rhs.m_ptr)
{
}
#ifdef SLANG_HAS_MOVE_SEMANTICS
SLANG_FORCE_INLINE ComPtr(ThisType&& rhs)
: m_ptr(rhs.m_ptr)
{
rhs.m_ptr = nullptr;
}
SLANG_FORCE_INLINE ComPtr& operator=(ThisType&& rhs)
{
T* swap = m_ptr;
m_ptr = rhs.m_ptr;
rhs.m_ptr = swap;
return *this;
}
#endif
SLANG_FORCE_INLINE ~ComPtr()
{
if (m_ptr)
((Ptr)m_ptr)->release();
}
SLANG_FORCE_INLINE operator T*() const { return m_ptr; }
SLANG_FORCE_INLINE T& operator*() { return *m_ptr; }
SLANG_FORCE_INLINE T* operator->() const { return m_ptr; }
SLANG_FORCE_INLINE const ThisType& operator=(const ThisType& rhs);
SLANG_FORCE_INLINE T* operator=(T* in);
SLANG_FORCE_INLINE T* get() const { return m_ptr; }
SLANG_FORCE_INLINE void setNull();
SLANG_FORCE_INLINE T* detach()
{
T* ptr = m_ptr;
m_ptr = nullptr;
return ptr;
}
SLANG_FORCE_INLINE void attach(T* in) { m_ptr = in; }
SLANG_FORCE_INLINE T** writeRef()
{
setNull();
return &m_ptr;
}
SLANG_FORCE_INLINE T* const* readRef() const { return &m_ptr; }
void swap(ThisType& rhs);
protected:
#ifndef SLANG_COM_PTR_ENABLE_REF_OPERATOR
SLANG_FORCE_INLINE T** operator&() = delete;
#endif
T* m_ptr;
};
template<typename T>
void ComPtr<T>::setNull()
{
if (m_ptr)
{
((Ptr)m_ptr)->release();
m_ptr = nullptr;
}
}
template<typename T>
const ComPtr<T>& ComPtr<T>::operator=(const ThisType& rhs)
{
if (rhs.m_ptr)
((Ptr)rhs.m_ptr)->addRef();
if (m_ptr)
((Ptr)m_ptr)->release();
m_ptr = rhs.m_ptr;
return *this;
}
template<typename T>
T* ComPtr<T>::operator=(T* ptr)
{
if (ptr)
((Ptr)ptr)->addRef();
if (m_ptr)
((Ptr)m_ptr)->release();
m_ptr = ptr;
return m_ptr;
}
template<typename T>
void ComPtr<T>::swap(ThisType& rhs)
{
T* tmp = m_ptr;
m_ptr = rhs.m_ptr;
rhs.m_ptr = tmp;
}
}
#endif