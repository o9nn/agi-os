#ifndef _UNORDERED_MAP_H
#define _UNORDERED_MAP_H
#include <bits/hashtable.h>
#include <bits/allocator.h>
#include <bits/functional_hash.h>
#include <bits/stl_function.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
_GLIBCXX_BEGIN_NAMESPACE_CONTAINER
template<bool _Cache>
using __umap_traits = __detail::_Hashtable_traits<_Cache, false, true>;
template<typename _Key,
typename _Tp,
typename _Hash = hash<_Key>,
typename _Pred = std::equal_to<_Key>,
typename _Alloc = std::allocator<std::pair<const _Key, _Tp> >,
typename _Tr = __umap_traits<__cache_default<_Key, _Hash>::value>>
using __umap_hashtable = _Hashtable<_Key, std::pair<const _Key, _Tp>,
_Alloc, __detail::_Select1st,
_Pred, _Hash,
__detail::_Mod_range_hashing,
__detail::_Default_ranged_hash,
__detail::_Prime_rehash_policy, _Tr>;
template<bool _Cache>
using __ummap_traits = __detail::_Hashtable_traits<_Cache, false, false>;
template<typename _Key,
typename _Tp,
typename _Hash = hash<_Key>,
typename _Pred = std::equal_to<_Key>,
typename _Alloc = std::allocator<std::pair<const _Key, _Tp> >,
typename _Tr = __ummap_traits<__cache_default<_Key, _Hash>::value>>
using __ummap_hashtable = _Hashtable<_Key, std::pair<const _Key, _Tp>,
_Alloc, __detail::_Select1st,
_Pred, _Hash,
__detail::_Mod_range_hashing,
__detail::_Default_ranged_hash,
__detail::_Prime_rehash_policy, _Tr>;
template<class _Key, class _Tp, class _Hash, class _Pred, class _Alloc>
class unordered_multimap;
template<typename _Key, typename _Tp,
typename _Hash = hash<_Key>,
typename _Pred = equal_to<_Key>,
typename _Alloc = allocator<std::pair<const _Key, _Tp>>>
class unordered_map
{
typedef __umap_hashtable<_Key, _Tp, _Hash, _Pred, _Alloc> _Hashtable;
_Hashtable _M_h;
public:
typedef typename _Hashtable::key_type key_type;
typedef typename _Hashtable::value_type value_type;
typedef typename _Hashtable::mapped_type mapped_type;
typedef typename _Hashtable::hasher hasher;
typedef typename _Hashtable::key_equal key_equal;
typedef typename _Hashtable::allocator_type allocator_type;
typedef typename _Hashtable::pointer pointer;
typedef typename _Hashtable::const_pointer const_pointer;
typedef typename _Hashtable::reference reference;
typedef typename _Hashtable::const_reference const_reference;
typedef typename _Hashtable::iterator iterator;
typedef typename _Hashtable::const_iterator const_iterator;
typedef typename _Hashtable::local_iterator local_iterator;
typedef typename _Hashtable::const_local_iterator const_local_iterator;
typedef typename _Hashtable::size_type size_type;
typedef typename _Hashtable::difference_type difference_type;
#if __cplusplus > 201402L
using node_type = typename _Hashtable::node_type;
using insert_return_type = typename _Hashtable::insert_return_type;
#endif
unordered_map() = default;
explicit
unordered_map(size_type __n,
const hasher& __hf = hasher(),
const key_equal& __eql = key_equal(),
const allocator_type& __a = allocator_type())
: _M_h(__n, __hf, __eql, __a)
{ }
template<typename _InputIterator>
unordered_map(_InputIterator __first, _InputIterator __last,
size_type __n = 0,
const hasher& __hf = hasher(),
const key_equal& __eql = key_equal(),
const allocator_type& __a = allocator_type())
: _M_h(__first, __last, __n, __hf, __eql, __a)
{ }
unordered_map(const unordered_map&) = default;
unordered_map(unordered_map&&) = default;
explicit
unordered_map(const allocator_type& __a)
: _M_h(__a)
{ }
unordered_map(const unordered_map& __umap,
const allocator_type& __a)
: _M_h(__umap._M_h, __a)
{ }
unordered_map(unordered_map&& __umap,
const allocator_type& __a)
noexcept( noexcept(_Hashtable(std::move(__umap._M_h), __a)) )
: _M_h(std::move(__umap._M_h), __a)
{ }
unordered_map(initializer_list<value_type> __l,
size_type __n = 0,
const hasher& __hf = hasher(),
const key_equal& __eql = key_equal(),
const allocator_type& __a = allocator_type())
: _M_h(__l, __n, __hf, __eql, __a)
{ }
unordered_map(size_type __n, const allocator_type& __a)
: unordered_map(__n, hasher(), key_equal(), __a)
{ }
unordered_map(size_type __n, const hasher& __hf,
const allocator_type& __a)
: unordered_map(__n, __hf, key_equal(), __a)
{ }
template<typename _InputIterator>
unordered_map(_InputIterator __first, _InputIterator __last,
size_type __n,
const allocator_type& __a)
: unordered_map(__first, __last, __n, hasher(), key_equal(), __a)
{ }
template<typename _InputIterator>
unordered_map(_InputIterator __first, _InputIterator __last,
size_type __n, const hasher& __hf,
const allocator_type& __a)
: unordered_map(__first, __last, __n, __hf, key_equal(), __a)
{ }
unordered_map(initializer_list<value_type> __l,
size_type __n,
const allocator_type& __a)
: unordered_map(__l, __n, hasher(), key_equal(), __a)
{ }
unordered_map(initializer_list<value_type> __l,
size_type __n, const hasher& __hf,
const allocator_type& __a)
: unordered_map(__l, __n, __hf, key_equal(), __a)
{ }
unordered_map&
operator=(const unordered_map&) = default;
unordered_map&
operator=(unordered_map&&) = default;
unordered_map&
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
const_iterator
cbegin() const noexcept
{ return _M_h.begin(); }
iterator
end() noexcept
{ return _M_h.end(); }
const_iterator
end() const noexcept
{ return _M_h.end(); }
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
#define __cpp_lib_unordered_map_try_emplace 201411L
template <typename... _Args>
pair<iterator, bool>
try_emplace(const key_type& __k, _Args&&... __args)
{
return _M_h.try_emplace(cend(), __k, std::forward<_Args>(__args)...);
}
template <typename... _Args>
pair<iterator, bool>
try_emplace(key_type&& __k, _Args&&... __args)
{
return _M_h.try_emplace(cend(), std::move(__k),
std::forward<_Args>(__args)...);
}
template <typename... _Args>
iterator
try_emplace(const_iterator __hint, const key_type& __k,
_Args&&... __args)
{
return _M_h.try_emplace(__hint, __k,
std::forward<_Args>(__args)...).first;
}
template <typename... _Args>
iterator
try_emplace(const_iterator __hint, key_type&& __k, _Args&&... __args)
{
return _M_h.try_emplace(__hint, std::move(__k),
std::forward<_Args>(__args)...).first;
}
#endif
std::pair<iterator, bool>
insert(const value_type& __x)
{ return _M_h.insert(__x); }
std::pair<iterator, bool>
insert(value_type&& __x)
{ return _M_h.insert(std::move(__x)); }
template<typename _Pair>
__enable_if_t<is_constructible<value_type, _Pair&&>::value,
pair<iterator, bool>>
insert(_Pair&& __x)
{ return _M_h.emplace(std::forward<_Pair>(__x)); }
iterator
insert(const_iterator __hint, const value_type& __x)
{ return _M_h.insert(__hint, __x); }
iterator
insert(const_iterator __hint, value_type&& __x)
{ return _M_h.insert(__hint, std::move(__x)); }
template<typename _Pair>
__enable_if_t<is_constructible<value_type, _Pair&&>::value, iterator>
insert(const_iterator __hint, _Pair&& __x)
{ return _M_h.emplace_hint(__hint, std::forward<_Pair>(__x)); }
template<typename _InputIterator>
void
insert(_InputIterator __first, _InputIterator __last)
{ _M_h.insert(__first, __last); }
void
insert(initializer_list<value_type> __l)
{ _M_h.insert(__l); }
#if __cplusplus > 201402L
template <typename _Obj>
pair<iterator, bool>
insert_or_assign(const key_type& __k, _Obj&& __obj)
{
auto __ret = _M_h.try_emplace(cend(), __k,
std::forward<_Obj>(__obj));
if (!__ret.second)
__ret.first->second = std::forward<_Obj>(__obj);
return __ret;
}
template <typename _Obj>
pair<iterator, bool>
insert_or_assign(key_type&& __k, _Obj&& __obj)
{
auto __ret = _M_h.try_emplace(cend(), std::move(__k),
std::forward<_Obj>(__obj));
if (!__ret.second)
__ret.first->second = std::forward<_Obj>(__obj);
return __ret;
}
template <typename _Obj>
iterator
insert_or_assign(const_iterator __hint, const key_type& __k,
_Obj&& __obj)
{
auto __ret = _M_h.try_emplace(__hint, __k, std::forward<_Obj>(__obj));
if (!__ret.second)
__ret.first->second = std::forward<_Obj>(__obj);
return __ret.first;
}
template <typename _Obj>
iterator
insert_or_assign(const_iterator __hint, key_type&& __k, _Obj&& __obj)
{
auto __ret = _M_h.try_emplace(__hint, std::move(__k),
std::forward<_Obj>(__obj));
if (!__ret.second)
__ret.first->second = std::forward<_Obj>(__obj);
return __ret.first;
}
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
swap(unordered_map& __x)
noexcept( noexcept(_M_h.swap(__x._M_h)) )
{ _M_h.swap(__x._M_h); }
#if __cplusplus > 201402L
template<typename, typename, typename>
friend class std::_Hash_merge_helper;
template<typename _H2, typename _P2>
void
merge(unordered_map<_Key, _Tp, _H2, _P2, _Alloc>& __source)
{
using _Merge_helper = _Hash_merge_helper<unordered_map, _H2, _P2>;
_M_h._M_merge_unique(_Merge_helper::_S_get_table(__source));
}
template<typename _H2, typename _P2>
void
merge(unordered_map<_Key, _Tp, _H2, _P2, _Alloc>&& __source)
{ merge(__source); }
template<typename _H2, typename _P2>
void
merge(unordered_multimap<_Key, _Tp, _H2, _P2, _Alloc>& __source)
{
using _Merge_helper = _Hash_merge_helper<unordered_map, _H2, _P2>;
_M_h._M_merge_unique(_Merge_helper::_S_get_table(__source));
}
template<typename _H2, typename _P2>
void
merge(unordered_multimap<_Key, _Tp, _H2, _P2, _Alloc>&& __source)
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
find(const _Kt& __x) -> decltype(_M_h._M_find_tr(__x))
{ return _M_h._M_find_tr(__x); }
#endif
const_iterator
find(const key_type& __x) const
{ return _M_h.find(__x); }
#if __cplusplus > 201703L
template<typename _Kt>
auto
find(const _Kt& __x) const -> decltype(_M_h._M_find_tr(__x))
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
mapped_type&
operator[](const key_type& __k)
{ return _M_h[__k]; }
mapped_type&
operator[](key_type&& __k)
{ return _M_h[std::move(__k)]; }
mapped_type&
at(const key_type& __k)
{ return _M_h.at(__k); }
const mapped_type&
at(const key_type& __k) const
{ return _M_h.at(__k); }
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
template<typename _Key1, typename _Tp1, typename _Hash1, typename _Pred1,
typename _Alloc1>
friend bool
operator==(const unordered_map<_Key1, _Tp1, _Hash1, _Pred1, _Alloc1>&,
const unordered_map<_Key1, _Tp1, _Hash1, _Pred1, _Alloc1>&);
};
#if __cpp_deduction_guides >= 201606
template<typename _InputIterator,
typename _Hash = hash<__iter_key_t<_InputIterator>>,
typename _Pred = equal_to<__iter_key_t<_InputIterator>>,
typename _Allocator = allocator<__iter_to_alloc_t<_InputIterator>>,
typename = _RequireInputIter<_InputIterator>,
typename = _RequireNotAllocatorOrIntegral<_Hash>,
typename = _RequireNotAllocator<_Pred>,
typename = _RequireAllocator<_Allocator>>
unordered_map(_InputIterator, _InputIterator,
typename unordered_map<int, int>::size_type = {},
_Hash = _Hash(), _Pred = _Pred(), _Allocator = _Allocator())
-> unordered_map<__iter_key_t<_InputIterator>,
__iter_val_t<_InputIterator>,
_Hash, _Pred, _Allocator>;
template<typename _Key, typename _Tp, typename _Hash = hash<_Key>,
typename _Pred = equal_to<_Key>,
typename _Allocator = allocator<pair<const _Key, _Tp>>,
typename = _RequireNotAllocatorOrIntegral<_Hash>,
typename = _RequireNotAllocator<_Pred>,
typename = _RequireAllocator<_Allocator>>
unordered_map(initializer_list<pair<_Key, _Tp>>,
typename unordered_map<int, int>::size_type = {},
_Hash = _Hash(), _Pred = _Pred(), _Allocator = _Allocator())
-> unordered_map<_Key, _Tp, _Hash, _Pred, _Allocator>;
template<typename _InputIterator, typename _Allocator,
typename = _RequireInputIter<_InputIterator>,
typename = _RequireAllocator<_Allocator>>
unordered_map(_InputIterator, _InputIterator,
typename unordered_map<int, int>::size_type, _Allocator)
-> unordered_map<__iter_key_t<_InputIterator>,
__iter_val_t<_InputIterator>,
hash<__iter_key_t<_InputIterator>>,
equal_to<__iter_key_t<_InputIterator>>,
_Allocator>;
template<typename _InputIterator, typename _Allocator,
typename = _RequireInputIter<_InputIterator>,
typename = _RequireAllocator<_Allocator>>
unordered_map(_InputIterator, _InputIterator, _Allocator)
-> unordered_map<__iter_key_t<_InputIterator>,
__iter_val_t<_InputIterator>,
hash<__iter_key_t<_InputIterator>>,
equal_to<__iter_key_t<_InputIterator>>,
_Allocator>;
template<typename _InputIterator, typename _Hash, typename _Allocator,
typename = _RequireInputIter<_InputIterator>,
typename = _RequireNotAllocatorOrIntegral<_Hash>,
typename = _RequireAllocator<_Allocator>>
unordered_map(_InputIterator, _InputIterator,
typename unordered_map<int, int>::size_type,
_Hash, _Allocator)
-> unordered_map<__iter_key_t<_InputIterator>,
__iter_val_t<_InputIterator>, _Hash,
equal_to<__iter_key_t<_InputIterator>>, _Allocator>;
template<typename _Key, typename _Tp, typename _Allocator,
typename = _RequireAllocator<_Allocator>>
unordered_map(initializer_list<pair<_Key, _Tp>>,
typename unordered_map<int, int>::size_type,
_Allocator)
-> unordered_map<_Key, _Tp, hash<_Key>, equal_to<_Key>, _Allocator>;
template<typename _Key, typename _Tp, typename _Allocator,
typename = _RequireAllocator<_Allocator>>
unordered_map(initializer_list<pair<_Key, _Tp>>, _Allocator)
-> unordered_map<_Key, _Tp, hash<_Key>, equal_to<_Key>, _Allocator>;
template<typename _Key, typename _Tp, typename _Hash, typename _Allocator,
typename = _RequireNotAllocatorOrIntegral<_Hash>,
typename = _RequireAllocator<_Allocator>>
unordered_map(initializer_list<pair<_Key, _Tp>>,
typename unordered_map<int, int>::size_type,
_Hash, _Allocator)
-> unordered_map<_Key, _Tp, _Hash, equal_to<_Key>, _Allocator>;
#endif
template<typename _Key, typename _Tp,
typename _Hash = hash<_Key>,
typename _Pred = equal_to<_Key>,
typename _Alloc = allocator<std::pair<const _Key, _Tp>>>
class unordered_multimap
{
typedef __ummap_hashtable<_Key, _Tp, _Hash, _Pred, _Alloc> _Hashtable;
_Hashtable _M_h;
public:
typedef typename _Hashtable::key_type key_type;
typedef typename _Hashtable::value_type value_type;
typedef typename _Hashtable::mapped_type mapped_type;
typedef typename _Hashtable::hasher hasher;
typedef typename _Hashtable::key_equal key_equal;
typedef typename _Hashtable::allocator_type allocator_type;
typedef typename _Hashtable::pointer pointer;
typedef typename _Hashtable::const_pointer const_pointer;
typedef typename _Hashtable::reference reference;
typedef typename _Hashtable::const_reference const_reference;
typedef typename _Hashtable::iterator iterator;
typedef typename _Hashtable::const_iterator const_iterator;
typedef typename _Hashtable::local_iterator local_iterator;
typedef typename _Hashtable::const_local_iterator const_local_iterator;
typedef typename _Hashtable::size_type size_type;
typedef typename _Hashtable::difference_type difference_type;
#if __cplusplus > 201402L
using node_type = typename _Hashtable::node_type;
#endif
unordered_multimap() = default;
explicit
unordered_multimap(size_type __n,
const hasher& __hf = hasher(),
const key_equal& __eql = key_equal(),
const allocator_type& __a = allocator_type())
: _M_h(__n, __hf, __eql, __a)
{ }
template<typename _InputIterator>
unordered_multimap(_InputIterator __first, _InputIterator __last,
size_type __n = 0,
const hasher& __hf = hasher(),
const key_equal& __eql = key_equal(),
const allocator_type& __a = allocator_type())
: _M_h(__first, __last, __n, __hf, __eql, __a)
{ }
unordered_multimap(const unordered_multimap&) = default;
unordered_multimap(unordered_multimap&&) = default;
explicit
unordered_multimap(const allocator_type& __a)
: _M_h(__a)
{ }
unordered_multimap(const unordered_multimap& __ummap,
const allocator_type& __a)
: _M_h(__ummap._M_h, __a)
{ }
unordered_multimap(unordered_multimap&& __ummap,
const allocator_type& __a)
noexcept( noexcept(_Hashtable(std::move(__ummap._M_h), __a)) )
: _M_h(std::move(__ummap._M_h), __a)
{ }
unordered_multimap(initializer_list<value_type> __l,
size_type __n = 0,
const hasher& __hf = hasher(),
const key_equal& __eql = key_equal(),
const allocator_type& __a = allocator_type())
: _M_h(__l, __n, __hf, __eql, __a)
{ }
unordered_multimap(size_type __n, const allocator_type& __a)
: unordered_multimap(__n, hasher(), key_equal(), __a)
{ }
unordered_multimap(size_type __n, const hasher& __hf,
const allocator_type& __a)
: unordered_multimap(__n, __hf, key_equal(), __a)
{ }
template<typename _InputIterator>
unordered_multimap(_InputIterator __first, _InputIterator __last,
size_type __n,
const allocator_type& __a)
: unordered_multimap(__first, __last, __n, hasher(), key_equal(), __a)
{ }
template<typename _InputIterator>
unordered_multimap(_InputIterator __first, _InputIterator __last,
size_type __n, const hasher& __hf,
const allocator_type& __a)
: unordered_multimap(__first, __last, __n, __hf, key_equal(), __a)
{ }
unordered_multimap(initializer_list<value_type> __l,
size_type __n,
const allocator_type& __a)
: unordered_multimap(__l, __n, hasher(), key_equal(), __a)
{ }
unordered_multimap(initializer_list<value_type> __l,
size_type __n, const hasher& __hf,
const allocator_type& __a)
: unordered_multimap(__l, __n, __hf, key_equal(), __a)
{ }
unordered_multimap&
operator=(const unordered_multimap&) = default;
unordered_multimap&
operator=(unordered_multimap&&) = default;
unordered_multimap&
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
const_iterator
cbegin() const noexcept
{ return _M_h.begin(); }
iterator
end() noexcept
{ return _M_h.end(); }
const_iterator
end() const noexcept
{ return _M_h.end(); }
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
template<typename _Pair>
__enable_if_t<is_constructible<value_type, _Pair&&>::value, iterator>
insert(_Pair&& __x)
{ return _M_h.emplace(std::forward<_Pair>(__x)); }
iterator
insert(const_iterator __hint, const value_type& __x)
{ return _M_h.insert(__hint, __x); }
iterator
insert(const_iterator __hint, value_type&& __x)
{ return _M_h.insert(__hint, std::move(__x)); }
template<typename _Pair>
__enable_if_t<is_constructible<value_type, _Pair&&>::value, iterator>
insert(const_iterator __hint, _Pair&& __x)
{ return _M_h.emplace_hint(__hint, std::forward<_Pair>(__x)); }
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
swap(unordered_multimap& __x)
noexcept( noexcept(_M_h.swap(__x._M_h)) )
{ _M_h.swap(__x._M_h); }
#if __cplusplus > 201402L
template<typename, typename, typename>
friend class std::_Hash_merge_helper;
template<typename _H2, typename _P2>
void
merge(unordered_multimap<_Key, _Tp, _H2, _P2, _Alloc>& __source)
{
using _Merge_helper
= _Hash_merge_helper<unordered_multimap, _H2, _P2>;
_M_h._M_merge_multi(_Merge_helper::_S_get_table(__source));
}
template<typename _H2, typename _P2>
void
merge(unordered_multimap<_Key, _Tp, _H2, _P2, _Alloc>&& __source)
{ merge(__source); }
template<typename _H2, typename _P2>
void
merge(unordered_map<_Key, _Tp, _H2, _P2, _Alloc>& __source)
{
using _Merge_helper
= _Hash_merge_helper<unordered_multimap, _H2, _P2>;
_M_h._M_merge_multi(_Merge_helper::_S_get_table(__source));
}
template<typename _H2, typename _P2>
void
merge(unordered_map<_Key, _Tp, _H2, _P2, _Alloc>&& __source)
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
find(const _Kt& __x) -> decltype(_M_h._M_find_tr(__x))
{ return _M_h._M_find_tr(__x); }
#endif
const_iterator
find(const key_type& __x) const
{ return _M_h.find(__x); }
#if __cplusplus > 201703L
template<typename _Kt>
auto
find(const _Kt& __x) const -> decltype(_M_h._M_find_tr(__x))
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
template<typename _Key1, typename _Tp1, typename _Hash1, typename _Pred1,
typename _Alloc1>
friend bool
operator==(const unordered_multimap<_Key1, _Tp1,
_Hash1, _Pred1, _Alloc1>&,
const unordered_multimap<_Key1, _Tp1,
_Hash1, _Pred1, _Alloc1>&);
};
#if __cpp_deduction_guides >= 201606
template<typename _InputIterator,
typename _Hash = hash<__iter_key_t<_InputIterator>>,
typename _Pred = equal_to<__iter_key_t<_InputIterator>>,
typename _Allocator = allocator<__iter_to_alloc_t<_InputIterator>>,
typename = _RequireInputIter<_InputIterator>,
typename = _RequireNotAllocatorOrIntegral<_Hash>,
typename = _RequireNotAllocator<_Pred>,
typename = _RequireAllocator<_Allocator>>
unordered_multimap(_InputIterator, _InputIterator,
unordered_multimap<int, int>::size_type = {},
_Hash = _Hash(), _Pred = _Pred(),
_Allocator = _Allocator())
-> unordered_multimap<__iter_key_t<_InputIterator>,
__iter_val_t<_InputIterator>, _Hash, _Pred,
_Allocator>;
template<typename _Key, typename _Tp, typename _Hash = hash<_Key>,
typename _Pred = equal_to<_Key>,
typename _Allocator = allocator<pair<const _Key, _Tp>>,
typename = _RequireNotAllocatorOrIntegral<_Hash>,
typename = _RequireNotAllocator<_Pred>,
typename = _RequireAllocator<_Allocator>>
unordered_multimap(initializer_list<pair<_Key, _Tp>>,
unordered_multimap<int, int>::size_type = {},
_Hash = _Hash(), _Pred = _Pred(),
_Allocator = _Allocator())
-> unordered_multimap<_Key, _Tp, _Hash, _Pred, _Allocator>;
template<typename _InputIterator, typename _Allocator,
typename = _RequireInputIter<_InputIterator>,
typename = _RequireAllocator<_Allocator>>
unordered_multimap(_InputIterator, _InputIterator,
unordered_multimap<int, int>::size_type, _Allocator)
-> unordered_multimap<__iter_key_t<_InputIterator>,
__iter_val_t<_InputIterator>,
hash<__iter_key_t<_InputIterator>>,
equal_to<__iter_key_t<_InputIterator>>, _Allocator>;
template<typename _InputIterator, typename _Allocator,
typename = _RequireInputIter<_InputIterator>,
typename = _RequireAllocator<_Allocator>>
unordered_multimap(_InputIterator, _InputIterator, _Allocator)
-> unordered_multimap<__iter_key_t<_InputIterator>,
__iter_val_t<_InputIterator>,
hash<__iter_key_t<_InputIterator>>,
equal_to<__iter_key_t<_InputIterator>>, _Allocator>;
template<typename _InputIterator, typename _Hash, typename _Allocator,
typename = _RequireInputIter<_InputIterator>,
typename = _RequireNotAllocatorOrIntegral<_Hash>,
typename = _RequireAllocator<_Allocator>>
unordered_multimap(_InputIterator, _InputIterator,
unordered_multimap<int, int>::size_type, _Hash,
_Allocator)
-> unordered_multimap<__iter_key_t<_InputIterator>,
__iter_val_t<_InputIterator>, _Hash,
equal_to<__iter_key_t<_InputIterator>>, _Allocator>;
template<typename _Key, typename _Tp, typename _Allocator,
typename = _RequireAllocator<_Allocator>>
unordered_multimap(initializer_list<pair<_Key, _Tp>>,
unordered_multimap<int, int>::size_type,
_Allocator)
-> unordered_multimap<_Key, _Tp, hash<_Key>, equal_to<_Key>, _Allocator>;
template<typename _Key, typename _Tp, typename _Allocator,
typename = _RequireAllocator<_Allocator>>
unordered_multimap(initializer_list<pair<_Key, _Tp>>, _Allocator)
-> unordered_multimap<_Key, _Tp, hash<_Key>, equal_to<_Key>, _Allocator>;
template<typename _Key, typename _Tp, typename _Hash, typename _Allocator,
typename = _RequireNotAllocatorOrIntegral<_Hash>,
typename = _RequireAllocator<_Allocator>>
unordered_multimap(initializer_list<pair<_Key, _Tp>>,
unordered_multimap<int, int>::size_type,
_Hash, _Allocator)
-> unordered_multimap<_Key, _Tp, _Hash, equal_to<_Key>, _Allocator>;
#endif
template<class _Key, class _Tp, class _Hash, class _Pred, class _Alloc>
inline void
swap(unordered_map<_Key, _Tp, _Hash, _Pred, _Alloc>& __x,
unordered_map<_Key, _Tp, _Hash, _Pred, _Alloc>& __y)
noexcept(noexcept(__x.swap(__y)))
{ __x.swap(__y); }
template<class _Key, class _Tp, class _Hash, class _Pred, class _Alloc>
inline void
swap(unordered_multimap<_Key, _Tp, _Hash, _Pred, _Alloc>& __x,
unordered_multimap<_Key, _Tp, _Hash, _Pred, _Alloc>& __y)
noexcept(noexcept(__x.swap(__y)))
{ __x.swap(__y); }
template<class _Key, class _Tp, class _Hash, class _Pred, class _Alloc>
inline bool
operator==(const unordered_map<_Key, _Tp, _Hash, _Pred, _Alloc>& __x,
const unordered_map<_Key, _Tp, _Hash, _Pred, _Alloc>& __y)
{ return __x._M_h._M_equal(__y._M_h); }
#if __cpp_impl_three_way_comparison < 201907L
template<class _Key, class _Tp, class _Hash, class _Pred, class _Alloc>
inline bool
operator!=(const unordered_map<_Key, _Tp, _Hash, _Pred, _Alloc>& __x,
const unordered_map<_Key, _Tp, _Hash, _Pred, _Alloc>& __y)
{ return !(__x == __y); }
#endif
template<class _Key, class _Tp, class _Hash, class _Pred, class _Alloc>
inline bool
operator==(const unordered_multimap<_Key, _Tp, _Hash, _Pred, _Alloc>& __x,
const unordered_multimap<_Key, _Tp, _Hash, _Pred, _Alloc>& __y)
{ return __x._M_h._M_equal(__y._M_h); }
#if __cpp_impl_three_way_comparison < 201907L
template<class _Key, class _Tp, class _Hash, class _Pred, class _Alloc>
inline bool
operator!=(const unordered_multimap<_Key, _Tp, _Hash, _Pred, _Alloc>& __x,
const unordered_multimap<_Key, _Tp, _Hash, _Pred, _Alloc>& __y)
{ return !(__x == __y); }
#endif
_GLIBCXX_END_NAMESPACE_CONTAINER
#if __cplusplus > 201402L
template<typename _Key, typename _Val, typename _Hash1, typename _Eq1,
typename _Alloc, typename _Hash2, typename _Eq2>
struct _Hash_merge_helper<
_GLIBCXX_STD_C::unordered_map<_Key, _Val, _Hash1, _Eq1, _Alloc>,
_Hash2, _Eq2>
{
private:
template<typename... _Tp>
using unordered_map = _GLIBCXX_STD_C::unordered_map<_Tp...>;
template<typename... _Tp>
using unordered_multimap = _GLIBCXX_STD_C::unordered_multimap<_Tp...>;
friend unordered_map<_Key, _Val, _Hash1, _Eq1, _Alloc>;
static auto&
_S_get_table(unordered_map<_Key, _Val, _Hash2, _Eq2, _Alloc>& __map)
{ return __map._M_h; }
static auto&
_S_get_table(unordered_multimap<_Key, _Val, _Hash2, _Eq2, _Alloc>& __map)
{ return __map._M_h; }
};
template<typename _Key, typename _Val, typename _Hash1, typename _Eq1,
typename _Alloc, typename _Hash2, typename _Eq2>
struct _Hash_merge_helper<
_GLIBCXX_STD_C::unordered_multimap<_Key, _Val, _Hash1, _Eq1, _Alloc>,
_Hash2, _Eq2>
{
private:
template<typename... _Tp>
using unordered_map = _GLIBCXX_STD_C::unordered_map<_Tp...>;
template<typename... _Tp>
using unordered_multimap = _GLIBCXX_STD_C::unordered_multimap<_Tp...>;
friend unordered_multimap<_Key, _Val, _Hash1, _Eq1, _Alloc>;
static auto&
_S_get_table(unordered_map<_Key, _Val, _Hash2, _Eq2, _Alloc>& __map)
{ return __map._M_h; }
static auto&
_S_get_table(unordered_multimap<_Key, _Val, _Hash2, _Eq2, _Alloc>& __map)
{ return __map._M_h; }
};
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif