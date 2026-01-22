#ifndef _UNORDERED_SET_H
#define _UNORDERED_SET_H
#include <bits/hashtable.h>
#include <bits/allocator.h>
#include <bits/functional_hash.h>
#include <bits/stl_function.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
_GLIBCXX_BEGIN_NAMESPACE_CONTAINER
template<bool _Cache>
using __uset_traits = __detail::_Hashtable_traits<_Cache, true, true>;
template<typename _Value,
typename _Hash = hash<_Value>,
typename _Pred = std::equal_to<_Value>,
typename _Alloc = std::allocator<_Value>,
typename _Tr = __uset_traits<__cache_default<_Value, _Hash>::value>>
using __uset_hashtable = _Hashtable<_Value, _Value, _Alloc,
__detail::_Identity, _Pred, _Hash,
__detail::_Mod_range_hashing,
__detail::_Default_ranged_hash,
__detail::_Prime_rehash_policy, _Tr>;
template<bool _Cache>
using __umset_traits = __detail::_Hashtable_traits<_Cache, true, false>;
template<typename _Value,
typename _Hash = hash<_Value>,
typename _Pred = std::equal_to<_Value>,
typename _Alloc = std::allocator<_Value>,
typename _Tr = __umset_traits<__cache_default<_Value, _Hash>::value>>
using __umset_hashtable = _Hashtable<_Value, _Value, _Alloc,
__detail::_Identity,
_Pred, _Hash,
__detail::_Mod_range_hashing,
__detail::_Default_ranged_hash,
__detail::_Prime_rehash_policy, _Tr>;
template<class _Value, class _Hash, class _Pred, class _Alloc>
class unordered_multiset;
template<typename _Value,
typename _Hash = hash<_Value>,
typename _Pred = equal_to<_Value>,
typename _Alloc = allocator<_Value>>
class unordered_set
{
typedef __uset_hashtable<_Value, _Hash, _Pred, _Alloc>  _Hashtable;
_Hashtable _M_h;
public:
typedef typename _Hashtable::key_type	key_type;
typedef typename _Hashtable::value_type	value_type;
typedef typename _Hashtable::hasher	hasher;
typedef typename _Hashtable::key_equal	key_equal;
typedef typename _Hashtable::allocator_type allocator_type;
typedef typename _Hashtable::pointer		pointer;
typedef typename _Hashtable::const_pointer	const_pointer;
typedef typename _Hashtable::reference		reference;
typedef typename _Hashtable::const_reference	const_reference;
typedef typename _Hashtable::iterator		iterator;
typedef typename _Hashtable::const_iterator	const_iterator;
typedef typename _Hashtable::local_iterator	local_iterator;
typedef typename _Hashtable::const_local_iterator	const_local_iterator;
typedef typename _Hashtable::size_type		size_type;
typedef typename _Hashtable::difference_type	difference_type;
#if __cplusplus > 201402L
using node_type = typename _Hashtable::node_type;
using insert_return_type = typename _Hashtable::insert_return_type;
#endif
unordered_set() = default;
explicit
unordered_set(size_type __n,
const hasher& __hf = hasher(),
const key_equal& __eql = key_equal(),
const allocator_type& __a = allocator_type())
: _M_h(__n, __hf, __eql, __a)
{ }
template<typename _InputIterator>
unordered_set(_InputIterator __first, _InputIterator __last,
size_type __n = 0,
const hasher& __hf = hasher(),
const key_equal& __eql = key_equal(),
const allocator_type& __a = allocator_type())
: _M_h(__first, __last, __n, __hf, __eql, __a)
{ }
unordered_set(const unordered_set&) = default;
unordered_set(unordered_set&&) = default;
explicit
unordered_set(const allocator_type& __a)
: _M_h(__a)
{ }
unordered_set(const unordered_set& __uset,
const allocator_type& __a)
: _M_h(__uset._M_h, __a)
{ }
unordered_set(unordered_set&& __uset,
const allocator_type& __a)
noexcept( noexcept(_Hashtable(std::move(__uset._M_h), __a)) )
: _M_h(std::move(__uset._M_h), __a)
{ }
unordered_set(initializer_list<value_type> __l,
size_type __n = 0,
const hasher& __hf = hasher(),
const key_equal& __eql = key_equal(),
const allocator_type& __a = allocator_type())
: _M_h(__l, __n, __hf, __eql, __a)
{ }
unordered_set(size_type __n, const allocator_type& __a)
: unordered_set(__n, hasher(), key_equal(), __a)
{ }
unordered_set(size_type __n, const hasher& __hf,
const allocator_type& __a)
: unordered_set(__n, __hf, key_equal(), __a)
{ }
template<typename _InputIterator>
unordered_set(_InputIterator __first, _InputIterator __last,
size_type __n,
const allocator_type& __a)
: unordered_set(__first, __last, __n, hasher(), key_equal(), __a)
{ }
template<typename _InputIterator>
unordered_set(_InputIterator __first, _InputIterator __last,
size_type __n, const hasher& __hf,
const allocator_type& __a)
: unordered_set(__first, __last, __n, __hf, key_equal(), __a)
{ }
unordered_set(initializer_list<value_type> __l,
size_type __n,
const allocator_type& __a)
: unordered_set(__l, __n, hasher(), key_equal(), __a)
{ }
unordered_set(initializer_list<value_type> __l,
size_type __n, const hasher& __hf,
const allocator_type& __a)
: unordered_set(__l, __n, __hf, key_equal(), __a)
{ }
unordered_set&
operator=(const unordered_set&) = default;
unordered_set&
operator=(unordered_set&&) = default;
unordered_set&
operator=(initializer_list<value_type> __l)
{
_M_h = __l;
return *this;
}
allocator_type
get_allocator() const noexcept
{ return _M_h.get_allocator(); }
_GLIBCXX_NODISCARD bool
empty() const noexcept
{ return _M_h.empty(); }
size_type
size() const noexcept
{ return _M_h.size(); }
size_type
max_size() const noexcept
{ return _M_h.max_size(); }
iterator
begin() noexcept
{ return _M_h.begin(); }
const_iterator
begin() const noexcept
{ return _M_h.begin(); }
iterator
end() noexcept
{ return _M_h.end(); }
const_iterator
end() const noexcept
{ return _M_h.end(); }
const_iterator
cbegin() const noexcept
{ return _M_h.begin(); }
const_iterator
cend() const noexcept
{ return _M_h.end(); }
template<typename... _Args>
std::pair<iterator, bool>
emplace(_Args&&... __args)
{ return _M_h.emplace(std::forward<_Args>(__args)...); }
template<typename... _Args>
iterator
emplace_hint(const_iterator __pos, _Args&&... __args)
{ return _M_h.emplace_hint(__pos, std::forward<_Args>(__args)...); }
std::pair<iterator, bool>
insert(const value_type& __x)
{ return _M_h.insert(__x); }
std::pair<iterator, bool>
insert(value_type&& __x)
{ return _M_h.insert(std::move(__x)); }
iterator
insert(const_iterator __hint, const value_type& __x)
{ return _M_h.insert(__hint, __x); }
iterator
insert(const_iterator __hint, value_type&& __x)
{ return _M_h.insert(__hint, std::move(__x)); }
template<typename _InputIterator>
void
insert(_InputIterator __first, _InputIterator __last)
{ _M_h.insert(__first, __last); }
void
insert(initializer_list<value_type> __l)
{ _M_h.insert(__l); }
#if __cplusplus > 201402L
node_type
extract(const_iterator __pos)
{
__glibcxx_assert(__pos != end());
return _M_h.extract(__pos);
}
node_type
extract(const key_type& __key)
{ return _M_h.extract(__key); }
insert_return_type
insert(node_type&& __nh)
{ return _M_h._M_reinsert_node(std::move(__nh)); }
iterator
insert(const_iterator, node_type&& __nh)
{ return _M_h._M_reinsert_node(std::move(__nh)).position; }
#endif
iterator
erase(const_iterator __position)
{ return _M_h.erase(__position); }
iterator
erase(iterator __position)
{ return _M_h.erase(__position); }
size_type
erase(const key_type& __x)
{ return _M_h.erase(__x); }
iterator
erase(const_iterator __first, const_iterator __last)
{ return _M_h.erase(__first, __last); }
void
clear() noexcept
{ _M_h.clear(); }
void
swap(unordered_set& __x)
noexcept( noexcept(_M_h.swap(__x._M_h)) )
{ _M_h.swap(__x._M_h); }
#if __cplusplus > 201402L
template<typename, typename, typename>
friend class std::_Hash_merge_helper;
template<typename _H2, typename _P2>
void
merge(unordered_set<_Value, _H2, _P2, _Alloc>& __source)
{
using _Merge_helper = _Hash_merge_helper<unordered_set, _H2, _P2>;
_M_h._M_merge_unique(_Merge_helper::_S_get_table(__source));
}
template<typename _H2, typename _P2>
void
merge(unordered_set<_Value, _H2, _P2, _Alloc>&& __source)
{ merge(__source); }
template<typename _H2, typename _P2>
void
merge(unordered_multiset<_Value, _H2, _P2, _Alloc>& __source)
{
using _Merge_helper = _Hash_merge_helper<unordered_set, _H2, _P2>;
_M_h._M_merge_unique(_Merge_helper::_S_get_table(__source));
}
template<typename _H2, typename _P2>
void
merge(unordered_multiset<_Value, _H2, _P2, _Alloc>&& __source)
{ merge(__source); }
#endif
hasher
hash_function() const
{ return _M_h.hash_function(); }
key_equal
key_eq() const
{ return _M_h.key_eq(); }
iterator
find(const key_type& __x)
{ return _M_h.find(__x); }
#if __cplusplus > 201703L
template<typename _Kt>
auto
find(const _Kt& __k)
-> decltype(_M_h._M_find_tr(__k))
{ return _M_h._M_find_tr(__k); }
#endif
const_iterator
find(const key_type& __x) const
{ return _M_h.find(__x); }
#if __cplusplus > 201703L
template<typename _Kt>
auto
find(const _Kt& __k) const
-> decltype(_M_h._M_find_tr(__k))
{ return _M_h._M_find_tr(__k); }
#endif
size_type
count(const key_type& __x) const
{ return _M_h.count(__x); }
#if __cplusplus > 201703L
template<typename _Kt>
auto
count(const _Kt& __k) const
-> decltype(_M_h._M_count_tr(__k))
{ return _M_h._M_count_tr(__k); }
#endif
#if __cplusplus > 201703L
bool
contains(const key_type& __x) const
{ return _M_h.find(__x) != _M_h.end(); }
template<typename _Kt>
auto
contains(const _Kt& __k) const
-> decltype(_M_h._M_find_tr(__k), void(), true)
{ return _M_h._M_find_tr(__k) != _M_h.end(); }
#endif
std::pair<iterator, iterator>
equal_range(const key_type& __x)
{ return _M_h.equal_range(__x); }
#if __cplusplus > 201703L
template<typename _Kt>
auto
equal_range(const _Kt& __k)
-> decltype(_M_h._M_equal_range_tr(__k))
{ return _M_h._M_equal_range_tr(__k); }
#endif
std::pair<const_iterator, const_iterator>
equal_range(const key_type& __x) const
{ return _M_h.equal_range(__x); }
#if __cplusplus > 201703L
template<typename _Kt>
auto
equal_range(const _Kt& __k) const
-> decltype(_M_h._M_equal_range_tr(__k))
{ return _M_h._M_equal_range_tr(__k); }
#endif
size_type
bucket_count() const noexcept
{ return _M_h.bucket_count(); }
size_type
max_bucket_count() const noexcept
{ return _M_h.max_bucket_count(); }
size_type
bucket_size(size_type __n) const
{ return _M_h.bucket_size(__n); }
size_type
bucket(const key_type& __key) const
{ return _M_h.bucket(__key); }
local_iterator
begin(size_type __n)
{ return _M_h.begin(__n); }
const_local_iterator
begin(size_type __n) const
{ return _M_h.begin(__n); }
const_local_iterator
cbegin(size_type __n) const
{ return _M_h.cbegin(__n); }
local_iterator
end(size_type __n)
{ return _M_h.end(__n); }
const_local_iterator
end(size_type __n) const
{ return _M_h.end(__n); }
const_local_iterator
cend(size_type __n) const
{ return _M_h.cend(__n); }
float
load_factor() const noexcept
{ return _M_h.load_factor(); }
float
max_load_factor() const noexcept
{ return _M_h.max_load_factor(); }
void
max_load_factor(float __z)
{ _M_h.max_load_factor(__z); }
void
rehash(size_type __n)
{ _M_h.rehash(__n); }
void
reserve(size_type __n)
{ _M_h.reserve(__n); }
template<typename _Value1, typename _Hash1, typename _Pred1,
typename _Alloc1>
friend bool
operator==(const unordered_set<_Value1, _Hash1, _Pred1, _Alloc1>&,
const unordered_set<_Value1, _Hash1, _Pred1, _Alloc1>&);
};
#if __cpp_deduction_guides >= 201606
template<typename _InputIterator,
typename _Hash =
hash<typename iterator_traits<_InputIterator>::value_type>,
typename _Pred =
equal_to<typename iterator_traits<_InputIterator>::value_type>,
typename _Allocator =
allocator<typename iterator_traits<_InputIterator>::value_type>,
typename = _RequireInputIter<_InputIterator>,
typename = _RequireNotAllocatorOrIntegral<_Hash>,
typename = _RequireNotAllocator<_Pred>,
typename = _RequireAllocator<_Allocator>>
unordered_set(_InputIterator, _InputIterator,
unordered_set<int>::size_type = {},
_Hash = _Hash(), _Pred = _Pred(), _Allocator = _Allocator())
-> unordered_set<typename iterator_traits<_InputIterator>::value_type,
_Hash, _Pred, _Allocator>;
template<typename _Tp, typename _Hash = hash<_Tp>,
typename _Pred = equal_to<_Tp>,
typename _Allocator = allocator<_Tp>,
typename = _RequireNotAllocatorOrIntegral<_Hash>,
typename = _RequireNotAllocator<_Pred>,
typename = _RequireAllocator<_Allocator>>
unordered_set(initializer_list<_Tp>,
unordered_set<int>::size_type = {},
_Hash = _Hash(), _Pred = _Pred(), _Allocator = _Allocator())
-> unordered_set<_Tp, _Hash, _Pred, _Allocator>;
template<typename _InputIterator, typename _Allocator,
typename = _RequireInputIter<_InputIterator>,
typename = _RequireAllocator<_Allocator>>
unordered_set(_InputIterator, _InputIterator,
unordered_set<int>::size_type, _Allocator)
-> unordered_set<typename iterator_traits<_InputIterator>::value_type,
hash<
typename iterator_traits<_InputIterator>::value_type>,
equal_to<
typename iterator_traits<_InputIterator>::value_type>,
_Allocator>;
template<typename _InputIterator, typename _Hash, typename _Allocator,
typename = _RequireInputIter<_InputIterator>,
typename = _RequireNotAllocatorOrIntegral<_Hash>,
typename = _RequireAllocator<_Allocator>>
unordered_set(_InputIterator, _InputIterator,
unordered_set<int>::size_type,
_Hash, _Allocator)
-> unordered_set<typename iterator_traits<_InputIterator>::value_type,
_Hash,
equal_to<
typename iterator_traits<_InputIterator>::value_type>,
_Allocator>;
template<typename _Tp, typename _Allocator,
typename = _RequireAllocator<_Allocator>>
unordered_set(initializer_list<_Tp>,
unordered_set<int>::size_type, _Allocator)
-> unordered_set<_Tp, hash<_Tp>, equal_to<_Tp>, _Allocator>;
template<typename _Tp, typename _Hash, typename _Allocator,
typename = _RequireNotAllocatorOrIntegral<_Hash>,
typename = _RequireAllocator<_Allocator>>
unordered_set(initializer_list<_Tp>,
unordered_set<int>::size_type, _Hash, _Allocator)
-> unordered_set<_Tp, _Hash, equal_to<_Tp>, _Allocator>;
#endif
template<typename _Value,
typename _Hash = hash<_Value>,
typename _Pred = equal_to<_Value>,
typename _Alloc = allocator<_Value>>
class unordered_multiset
{
typedef __umset_hashtable<_Value, _Hash, _Pred, _Alloc>  _Hashtable;
_Hashtable _M_h;
public:
typedef typename _Hashtable::key_type	key_type;
typedef typename _Hashtable::value_type	value_type;
typedef typename _Hashtable::hasher	hasher;
typedef typename _Hashtable::key_equal	key_equal;
typedef typename _Hashtable::allocator_type allocator_type;
typedef typename _Hashtable::pointer		pointer;
typedef typename _Hashtable::const_pointer	const_pointer;
typedef typename _Hashtable::reference		reference;
typedef typename _Hashtable::const_reference	const_reference;
typedef typename _Hashtable::iterator		iterator;
typedef typename _Hashtable::const_iterator	const_iterator;
typedef typename _Hashtable::local_iterator	local_iterator;
typedef typename _Hashtable::const_local_iterator	const_local_iterator;
typedef typename _Hashtable::size_type		size_type;
typedef typename _Hashtable::difference_type	difference_type;
#if __cplusplus > 201402L
using node_type = typename _Hashtable::node_type;
#endif
unordered_multiset() = default;
explicit
unordered_multiset(size_type __n,
const hasher& __hf = hasher(),
const key_equal& __eql = key_equal(),
const allocator_type& __a = allocator_type())
: _M_h(__n, __hf, __eql, __a)
{ }
template<typename _InputIterator>
unordered_multiset(_InputIterator __first, _InputIterator __last,
size_type __n = 0,
const hasher& __hf = hasher(),
const key_equal& __eql = key_equal(),
const allocator_type& __a = allocator_type())
: _M_h(__first, __last, __n, __hf, __eql, __a)
{ }
unordered_multiset(const unordered_multiset&) = default;
unordered_multiset(unordered_multiset&&) = default;
unordered_multiset(initializer_list<value_type> __l,
size_type __n = 0,
const hasher& __hf = hasher(),
const key_equal& __eql = key_equal(),
const allocator_type& __a = allocator_type())
: _M_h(__l, __n, __hf, __eql, __a)
{ }
unordered_multiset&
operator=(const unordered_multiset&) = default;
unordered_multiset&
operator=(unordered_multiset&&) = default;
explicit
unordered_multiset(const allocator_type& __a)
: _M_h(__a)
{ }
unordered_multiset(const unordered_multiset& __umset,
const allocator_type& __a)
: _M_h(__umset._M_h, __a)
{ }
unordered_multiset(unordered_multiset&& __umset,
const allocator_type& __a)
noexcept( noexcept(_Hashtable(std::move(__umset._M_h), __a)) )
: _M_h(std::move(__umset._M_h), __a)
{ }
unordered_multiset(size_type __n, const allocator_type& __a)
: unordered_multiset(__n, hasher(), key_equal(), __a)
{ }
unordered_multiset(size_type __n, const hasher& __hf,
const allocator_type& __a)
: unordered_multiset(__n, __hf, key_equal(), __a)
{ }
template<typename _InputIterator>
unordered_multiset(_InputIterator __first, _InputIterator __last,
size_type __n,
const allocator_type& __a)
: unordered_multiset(__first, __last, __n, hasher(), key_equal(), __a)
{ }
template<typename _InputIterator>
unordered_multiset(_InputIterator __first, _InputIterator __last,
size_type __n, const hasher& __hf,
const allocator_type& __a)
: unordered_multiset(__first, __last, __n, __hf, key_equal(), __a)
{ }
unordered_multiset(initializer_list<value_type> __l,
size_type __n,
const allocator_type& __a)
: unordered_multiset(__l, __n, hasher(), key_equal(), __a)
{ }
unordered_multiset(initializer_list<value_type> __l,
size_type __n, const hasher& __hf,
const allocator_type& __a)
: unordered_multiset(__l, __n, __hf, key_equal(), __a)
{ }
unordered_multiset&
operator=(initializer_list<value_type> __l)
{
_M_h = __l;
return *this;
}
allocator_type
get_allocator() const noexcept
{ return _M_h.get_allocator(); }
_GLIBCXX_NODISCARD bool
empty() const noexcept
{ return _M_h.empty(); }
size_type
size() const noexcept
{ return _M_h.size(); }
size_type
max_size() const noexcept
{ return _M_h.max_size(); }
iterator
begin() noexcept
{ return _M_h.begin(); }
const_iterator
begin() const noexcept
{ return _M_h.begin(); }
iterator
end() noexcept
{ return _M_h.end(); }
const_iterator
end() const noexcept
{ return _M_h.end(); }
const_iterator
cbegin() const noexcept
{ return _M_h.begin(); }
const_iterator
cend() const noexcept
{ return _M_h.end(); }
template<typename... _Args>
iterator
emplace(_Args&&... __args)
{ return _M_h.emplace(std::forward<_Args>(__args)...); }
template<typename... _Args>
iterator
emplace_hint(const_iterator __pos, _Args&&... __args)
{ return _M_h.emplace_hint(__pos, std::forward<_Args>(__args)...); }
iterator
insert(const value_type& __x)
{ return _M_h.insert(__x); }
iterator
insert(value_type&& __x)
{ return _M_h.insert(std::move(__x)); }
iterator
insert(const_iterator __hint, const value_type& __x)
{ return _M_h.insert(__hint, __x); }
iterator
insert(const_iterator __hint, value_type&& __x)
{ return _M_h.insert(__hint, std::move(__x)); }
template<typename _InputIterator>
void
insert(_InputIterator __first, _InputIterator __last)
{ _M_h.insert(__first, __last); }
void
insert(initializer_list<value_type> __l)
{ _M_h.insert(__l); }
#if __cplusplus > 201402L
node_type
extract(const_iterator __pos)
{
__glibcxx_assert(__pos != end());
return _M_h.extract(__pos);
}
node_type
extract(const key_type& __key)
{ return _M_h.extract(__key); }
iterator
insert(node_type&& __nh)
{ return _M_h._M_reinsert_node_multi(cend(), std::move(__nh)); }
iterator
insert(const_iterator __hint, node_type&& __nh)
{ return _M_h._M_reinsert_node_multi(__hint, std::move(__nh)); }
#endif
iterator
erase(const_iterator __position)
{ return _M_h.erase(__position); }
iterator
erase(iterator __position)
{ return _M_h.erase(__position); }
size_type
erase(const key_type& __x)
{ return _M_h.erase(__x); }
iterator
erase(const_iterator __first, const_iterator __last)
{ return _M_h.erase(__first, __last); }
void
clear() noexcept
{ _M_h.clear(); }
void
swap(unordered_multiset& __x)
noexcept( noexcept(_M_h.swap(__x._M_h)) )
{ _M_h.swap(__x._M_h); }
#if __cplusplus > 201402L
template<typename, typename, typename>
friend class std::_Hash_merge_helper;
template<typename _H2, typename _P2>
void
merge(unordered_multiset<_Value, _H2, _P2, _Alloc>& __source)
{
using _Merge_helper
= _Hash_merge_helper<unordered_multiset, _H2, _P2>;
_M_h._M_merge_multi(_Merge_helper::_S_get_table(__source));
}
template<typename _H2, typename _P2>
void
merge(unordered_multiset<_Value, _H2, _P2, _Alloc>&& __source)
{ merge(__source); }
template<typename _H2, typename _P2>
void
merge(unordered_set<_Value, _H2, _P2, _Alloc>& __source)
{
using _Merge_helper
= _Hash_merge_helper<unordered_multiset, _H2, _P2>;
_M_h._M_merge_multi(_Merge_helper::_S_get_table(__source));
}
template<typename _H2, typename _P2>
void
merge(unordered_set<_Value, _H2, _P2, _Alloc>&& __source)
{ merge(__source); }
#endif
hasher
hash_function() const
{ return _M_h.hash_function(); }
key_equal
key_eq() const
{ return _M_h.key_eq(); }
iterator
find(const key_type& __x)
{ return _M_h.find(__x); }
#if __cplusplus > 201703L
template<typename _Kt>
auto
find(const _Kt& __x)
-> decltype(_M_h._M_find_tr(__x))
{ return _M_h._M_find_tr(__x); }
#endif
const_iterator
find(const key_type& __x) const
{ return _M_h.find(__x); }
#if __cplusplus > 201703L
template<typename _Kt>
auto
find(const _Kt& __x) const
-> decltype(_M_h._M_find_tr(__x))
{ return _M_h._M_find_tr(__x); }
#endif
size_type
count(const key_type& __x) const
{ return _M_h.count(__x); }
#if __cplusplus > 201703L
template<typename _Kt>
auto
count(const _Kt& __x) const -> decltype(_M_h._M_count_tr(__x))
{ return _M_h._M_count_tr(__x); }
#endif
#if __cplusplus > 201703L
bool
contains(const key_type& __x) const
{ return _M_h.find(__x) != _M_h.end(); }
template<typename _Kt>
auto
contains(const _Kt& __x) const
-> decltype(_M_h._M_find_tr(__x), void(), true)
{ return _M_h._M_find_tr(__x) != _M_h.end(); }
#endif
std::pair<iterator, iterator>
equal_range(const key_type& __x)
{ return _M_h.equal_range(__x); }
#if __cplusplus > 201703L
template<typename _Kt>
auto
equal_range(const _Kt& __x)
-> decltype(_M_h._M_equal_range_tr(__x))
{ return _M_h._M_equal_range_tr(__x); }
#endif
std::pair<const_iterator, const_iterator>
equal_range(const key_type& __x) const
{ return _M_h.equal_range(__x); }
#if __cplusplus > 201703L
template<typename _Kt>
auto
equal_range(const _Kt& __x) const
-> decltype(_M_h._M_equal_range_tr(__x))
{ return _M_h._M_equal_range_tr(__x); }
#endif
size_type
bucket_count() const noexcept
{ return _M_h.bucket_count(); }
size_type
max_bucket_count() const noexcept
{ return _M_h.max_bucket_count(); }
size_type
bucket_size(size_type __n) const
{ return _M_h.bucket_size(__n); }
size_type
bucket(const key_type& __key) const
{ return _M_h.bucket(__key); }
local_iterator
begin(size_type __n)
{ return _M_h.begin(__n); }
const_local_iterator
begin(size_type __n) const
{ return _M_h.begin(__n); }
const_local_iterator
cbegin(size_type __n) const
{ return _M_h.cbegin(__n); }
local_iterator
end(size_type __n)
{ return _M_h.end(__n); }
const_local_iterator
end(size_type __n) const
{ return _M_h.end(__n); }
const_local_iterator
cend(size_type __n) const
{ return _M_h.cend(__n); }
float
load_factor() const noexcept
{ return _M_h.load_factor(); }
float
max_load_factor() const noexcept
{ return _M_h.max_load_factor(); }
void
max_load_factor(float __z)
{ _M_h.max_load_factor(__z); }
void
rehash(size_type __n)
{ _M_h.rehash(__n); }
void
reserve(size_type __n)
{ _M_h.reserve(__n); }
template<typename _Value1, typename _Hash1, typename _Pred1,
typename _Alloc1>
friend bool
operator==(const unordered_multiset<_Value1, _Hash1, _Pred1, _Alloc1>&,
const unordered_multiset<_Value1, _Hash1, _Pred1, _Alloc1>&);
};
#if __cpp_deduction_guides >= 201606
template<typename _InputIterator,
typename _Hash =
hash<typename iterator_traits<_InputIterator>::value_type>,
typename _Pred =
equal_to<typename iterator_traits<_InputIterator>::value_type>,
typename _Allocator =
allocator<typename iterator_traits<_InputIterator>::value_type>,
typename = _RequireInputIter<_InputIterator>,
typename = _RequireNotAllocatorOrIntegral<_Hash>,
typename = _RequireNotAllocator<_Pred>,
typename = _RequireAllocator<_Allocator>>
unordered_multiset(_InputIterator, _InputIterator,
unordered_multiset<int>::size_type = {},
_Hash = _Hash(), _Pred = _Pred(),
_Allocator = _Allocator())
-> unordered_multiset<typename iterator_traits<_InputIterator>::value_type,
_Hash, _Pred, _Allocator>;
template<typename _Tp, typename _Hash = hash<_Tp>,
typename _Pred = equal_to<_Tp>,
typename _Allocator = allocator<_Tp>,
typename = _RequireNotAllocatorOrIntegral<_Hash>,
typename = _RequireNotAllocator<_Pred>,
typename = _RequireAllocator<_Allocator>>
unordered_multiset(initializer_list<_Tp>,
unordered_multiset<int>::size_type = {},
_Hash = _Hash(), _Pred = _Pred(),
_Allocator = _Allocator())
-> unordered_multiset<_Tp, _Hash, _Pred, _Allocator>;
template<typename _InputIterator, typename _Allocator,
typename = _RequireInputIter<_InputIterator>,
typename = _RequireAllocator<_Allocator>>
unordered_multiset(_InputIterator, _InputIterator,
unordered_multiset<int>::size_type, _Allocator)
-> unordered_multiset<typename iterator_traits<_InputIterator>::value_type,
hash<typename
iterator_traits<_InputIterator>::value_type>,
equal_to<typename
iterator_traits<_InputIterator>::value_type>,
_Allocator>;
template<typename _InputIterator, typename _Hash, typename _Allocator,
typename = _RequireInputIter<_InputIterator>,
typename = _RequireNotAllocatorOrIntegral<_Hash>,
typename = _RequireAllocator<_Allocator>>
unordered_multiset(_InputIterator, _InputIterator,
unordered_multiset<int>::size_type,
_Hash, _Allocator)
-> unordered_multiset<typename
iterator_traits<_InputIterator>::value_type,
_Hash,
equal_to<
typename
iterator_traits<_InputIterator>::value_type>,
_Allocator>;
template<typename _Tp, typename _Allocator,
typename = _RequireAllocator<_Allocator>>
unordered_multiset(initializer_list<_Tp>,
unordered_multiset<int>::size_type, _Allocator)
-> unordered_multiset<_Tp, hash<_Tp>, equal_to<_Tp>, _Allocator>;
template<typename _Tp, typename _Hash, typename _Allocator,
typename = _RequireNotAllocatorOrIntegral<_Hash>,
typename = _RequireAllocator<_Allocator>>
unordered_multiset(initializer_list<_Tp>,
unordered_multiset<int>::size_type, _Hash, _Allocator)
-> unordered_multiset<_Tp, _Hash, equal_to<_Tp>, _Allocator>;
#endif
template<class _Value, class _Hash, class _Pred, class _Alloc>
inline void
swap(unordered_set<_Value, _Hash, _Pred, _Alloc>& __x,
unordered_set<_Value, _Hash, _Pred, _Alloc>& __y)
noexcept(noexcept(__x.swap(__y)))
{ __x.swap(__y); }
template<class _Value, class _Hash, class _Pred, class _Alloc>
inline void
swap(unordered_multiset<_Value, _Hash, _Pred, _Alloc>& __x,
unordered_multiset<_Value, _Hash, _Pred, _Alloc>& __y)
noexcept(noexcept(__x.swap(__y)))
{ __x.swap(__y); }
template<class _Value, class _Hash, class _Pred, class _Alloc>
inline bool
operator==(const unordered_set<_Value, _Hash, _Pred, _Alloc>& __x,
const unordered_set<_Value, _Hash, _Pred, _Alloc>& __y)
{ return __x._M_h._M_equal(__y._M_h); }
#if __cpp_impl_three_way_comparison < 201907L
template<class _Value, class _Hash, class _Pred, class _Alloc>
inline bool
operator!=(const unordered_set<_Value, _Hash, _Pred, _Alloc>& __x,
const unordered_set<_Value, _Hash, _Pred, _Alloc>& __y)
{ return !(__x == __y); }
#endif
template<class _Value, class _Hash, class _Pred, class _Alloc>
inline bool
operator==(const unordered_multiset<_Value, _Hash, _Pred, _Alloc>& __x,
const unordered_multiset<_Value, _Hash, _Pred, _Alloc>& __y)
{ return __x._M_h._M_equal(__y._M_h); }
#if __cpp_impl_three_way_comparison < 201907L
template<class _Value, class _Hash, class _Pred, class _Alloc>
inline bool
operator!=(const unordered_multiset<_Value, _Hash, _Pred, _Alloc>& __x,
const unordered_multiset<_Value, _Hash, _Pred, _Alloc>& __y)
{ return !(__x == __y); }
#endif
_GLIBCXX_END_NAMESPACE_CONTAINER
#if __cplusplus > 201402L
template<typename _Val, typename _Hash1, typename _Eq1, typename _Alloc,
typename _Hash2, typename _Eq2>
struct _Hash_merge_helper<
_GLIBCXX_STD_C::unordered_set<_Val, _Hash1, _Eq1, _Alloc>, _Hash2, _Eq2>
{
private:
template<typename... _Tp>
using unordered_set = _GLIBCXX_STD_C::unordered_set<_Tp...>;
template<typename... _Tp>
using unordered_multiset = _GLIBCXX_STD_C::unordered_multiset<_Tp...>;
friend unordered_set<_Val, _Hash1, _Eq1, _Alloc>;
static auto&
_S_get_table(unordered_set<_Val, _Hash2, _Eq2, _Alloc>& __set)
{ return __set._M_h; }
static auto&
_S_get_table(unordered_multiset<_Val, _Hash2, _Eq2, _Alloc>& __set)
{ return __set._M_h; }
};
template<typename _Val, typename _Hash1, typename _Eq1, typename _Alloc,
typename _Hash2, typename _Eq2>
struct _Hash_merge_helper<
_GLIBCXX_STD_C::unordered_multiset<_Val, _Hash1, _Eq1, _Alloc>,
_Hash2, _Eq2>
{
private:
template<typename... _Tp>
using unordered_set = _GLIBCXX_STD_C::unordered_set<_Tp...>;
template<typename... _Tp>
using unordered_multiset = _GLIBCXX_STD_C::unordered_multiset<_Tp...>;
friend unordered_multiset<_Val, _Hash1, _Eq1, _Alloc>;
static auto&
_S_get_table(unordered_set<_Val, _Hash2, _Eq2, _Alloc>& __set)
{ return __set._M_h; }
static auto&
_S_get_table(unordered_multiset<_Val, _Hash2, _Eq2, _Alloc>& __set)
{ return __set._M_h; }
};
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif