#ifndef _ENABLE_SPECIAL_MEMBERS_H
#define _ENABLE_SPECIAL_MEMBERS_H 1
#pragma GCC system_header
#include <bits/c++config.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
struct _Enable_default_constructor_tag
{
explicit constexpr _Enable_default_constructor_tag() = default;
};
template<bool _Switch, typename _Tag = void>
struct _Enable_default_constructor
{
constexpr _Enable_default_constructor() noexcept = default;
constexpr _Enable_default_constructor(_Enable_default_constructor const&)
noexcept = default;
constexpr _Enable_default_constructor(_Enable_default_constructor&&)
noexcept = default;
_Enable_default_constructor&
operator=(_Enable_default_constructor const&) noexcept = default;
_Enable_default_constructor&
operator=(_Enable_default_constructor&&) noexcept = default;
constexpr explicit
_Enable_default_constructor(_Enable_default_constructor_tag) { }
};
template<bool _Switch, typename _Tag = void>
struct _Enable_destructor { };
template<bool _Copy, bool _CopyAssignment,
bool _Move, bool _MoveAssignment,
typename _Tag = void>
struct _Enable_copy_move { };
template<bool _Default, bool _Destructor,
bool _Copy, bool _CopyAssignment,
bool _Move, bool _MoveAssignment,
typename _Tag = void>
struct _Enable_special_members
: private _Enable_default_constructor<_Default, _Tag>,
private _Enable_destructor<_Destructor, _Tag>,
private _Enable_copy_move<_Copy, _CopyAssignment,
_Move, _MoveAssignment,
_Tag>
{ };
template<typename _Tag>
struct _Enable_default_constructor<false, _Tag>
{
constexpr _Enable_default_constructor() noexcept = delete;
constexpr _Enable_default_constructor(_Enable_default_constructor const&)
noexcept = default;
constexpr _Enable_default_constructor(_Enable_default_constructor&&)
noexcept = default;
_Enable_default_constructor&
operator=(_Enable_default_constructor const&) noexcept = default;
_Enable_default_constructor&
operator=(_Enable_default_constructor&&) noexcept = default;
constexpr explicit
_Enable_default_constructor(_Enable_default_constructor_tag) { }
};
template<typename _Tag>
struct _Enable_destructor<false, _Tag>
{ ~_Enable_destructor() noexcept = delete; };
template<typename _Tag>
struct _Enable_copy_move<false, true, true, true, _Tag>
{
constexpr _Enable_copy_move() noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move const&) noexcept = delete;
constexpr _Enable_copy_move(_Enable_copy_move&&) noexcept = default;
_Enable_copy_move&
operator=(_Enable_copy_move const&) noexcept = default;
_Enable_copy_move&
operator=(_Enable_copy_move&&) noexcept = default;
};
template<typename _Tag>
struct _Enable_copy_move<true, false, true, true, _Tag>
{
constexpr _Enable_copy_move() noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move const&) noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move&&) noexcept = default;
_Enable_copy_move&
operator=(_Enable_copy_move const&) noexcept = delete;
_Enable_copy_move&
operator=(_Enable_copy_move&&) noexcept = default;
};
template<typename _Tag>
struct _Enable_copy_move<false, false, true, true, _Tag>
{
constexpr _Enable_copy_move() noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move const&) noexcept = delete;
constexpr _Enable_copy_move(_Enable_copy_move&&) noexcept = default;
_Enable_copy_move&
operator=(_Enable_copy_move const&) noexcept = delete;
_Enable_copy_move&
operator=(_Enable_copy_move&&) noexcept = default;
};
template<typename _Tag>
struct _Enable_copy_move<true, true, false, true, _Tag>
{
constexpr _Enable_copy_move() noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move const&) noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move&&) noexcept = delete;
_Enable_copy_move&
operator=(_Enable_copy_move const&) noexcept = default;
_Enable_copy_move&
operator=(_Enable_copy_move&&) noexcept = default;
};
template<typename _Tag>
struct _Enable_copy_move<false, true, false, true, _Tag>
{
constexpr _Enable_copy_move() noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move const&) noexcept = delete;
constexpr _Enable_copy_move(_Enable_copy_move&&) noexcept = delete;
_Enable_copy_move&
operator=(_Enable_copy_move const&) noexcept = default;
_Enable_copy_move&
operator=(_Enable_copy_move&&) noexcept = default;
};
template<typename _Tag>
struct _Enable_copy_move<true, false, false, true, _Tag>
{
constexpr _Enable_copy_move() noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move const&) noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move&&) noexcept = delete;
_Enable_copy_move&
operator=(_Enable_copy_move const&) noexcept = delete;
_Enable_copy_move&
operator=(_Enable_copy_move&&) noexcept = default;
};
template<typename _Tag>
struct _Enable_copy_move<false, false, false, true, _Tag>
{
constexpr _Enable_copy_move() noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move const&) noexcept = delete;
constexpr _Enable_copy_move(_Enable_copy_move&&) noexcept = delete;
_Enable_copy_move&
operator=(_Enable_copy_move const&) noexcept = delete;
_Enable_copy_move&
operator=(_Enable_copy_move&&) noexcept = default;
};
template<typename _Tag>
struct _Enable_copy_move<true, true, true, false, _Tag>
{
constexpr _Enable_copy_move() noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move const&) noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move&&) noexcept = default;
_Enable_copy_move&
operator=(_Enable_copy_move const&) noexcept = default;
_Enable_copy_move&
operator=(_Enable_copy_move&&) noexcept = delete;
};
template<typename _Tag>
struct _Enable_copy_move<false, true, true, false, _Tag>
{
constexpr _Enable_copy_move() noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move const&) noexcept = delete;
constexpr _Enable_copy_move(_Enable_copy_move&&) noexcept = default;
_Enable_copy_move&
operator=(_Enable_copy_move const&) noexcept = default;
_Enable_copy_move&
operator=(_Enable_copy_move&&) noexcept = delete;
};
template<typename _Tag>
struct _Enable_copy_move<true, false, true, false, _Tag>
{
constexpr _Enable_copy_move() noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move const&) noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move&&) noexcept = default;
_Enable_copy_move&
operator=(_Enable_copy_move const&) noexcept = delete;
_Enable_copy_move&
operator=(_Enable_copy_move&&) noexcept = delete;
};
template<typename _Tag>
struct _Enable_copy_move<false, false, true, false, _Tag>
{
constexpr _Enable_copy_move() noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move const&) noexcept = delete;
constexpr _Enable_copy_move(_Enable_copy_move&&) noexcept = default;
_Enable_copy_move&
operator=(_Enable_copy_move const&) noexcept = delete;
_Enable_copy_move&
operator=(_Enable_copy_move&&) noexcept = delete;
};
template<typename _Tag>
struct _Enable_copy_move<true, true, false, false, _Tag>
{
constexpr _Enable_copy_move() noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move const&) noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move&&) noexcept = delete;
_Enable_copy_move&
operator=(_Enable_copy_move const&) noexcept = default;
_Enable_copy_move&
operator=(_Enable_copy_move&&) noexcept = delete;
};
template<typename _Tag>
struct _Enable_copy_move<false, true, false, false, _Tag>
{
constexpr _Enable_copy_move() noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move const&) noexcept = delete;
constexpr _Enable_copy_move(_Enable_copy_move&&) noexcept = delete;
_Enable_copy_move&
operator=(_Enable_copy_move const&) noexcept = default;
_Enable_copy_move&
operator=(_Enable_copy_move&&) noexcept = delete;
};
template<typename _Tag>
struct _Enable_copy_move<true, false, false, false, _Tag>
{
constexpr _Enable_copy_move() noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move const&) noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move&&) noexcept = delete;
_Enable_copy_move&
operator=(_Enable_copy_move const&) noexcept = delete;
_Enable_copy_move&
operator=(_Enable_copy_move&&) noexcept = delete;
};
template<typename _Tag>
struct _Enable_copy_move<false, false, false, false, _Tag>
{
constexpr _Enable_copy_move() noexcept = default;
constexpr _Enable_copy_move(_Enable_copy_move const&) noexcept = delete;
constexpr _Enable_copy_move(_Enable_copy_move&&) noexcept = delete;
_Enable_copy_move&
operator=(_Enable_copy_move const&) noexcept = delete;
_Enable_copy_move&
operator=(_Enable_copy_move&&) noexcept = delete;
};
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif