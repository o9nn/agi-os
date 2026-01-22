#ifndef _CXXABI_FORCED_H
#define _CXXABI_FORCED_H 1
#pragma GCC system_header
#pragma GCC visibility push(default)
#ifdef __cplusplus
namespace __cxxabiv1
{
class __forced_unwind
{
virtual ~__forced_unwind() throw();
virtual void __pure_dummy() = 0;
};
}
#endif
#pragma GCC visibility pop
#endif