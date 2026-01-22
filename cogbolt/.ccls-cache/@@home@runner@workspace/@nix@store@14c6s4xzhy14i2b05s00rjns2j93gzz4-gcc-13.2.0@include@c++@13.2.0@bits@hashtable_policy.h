#ifndef _HASHTABLE_POLICY_H
#define _HASHTABLE_POLICY_H 1
#include <tuple>
#include <bits/functional_hash.h>
#include <bits/stl_algobase.h>
#include <bits/stl_pair.h>
#include <ext/aligned_buffer.h>
#include <ext/alloc_traits.h>
#include <ext/numeric_traits.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
template<typename _Key, typename _Value, typename _Alloc,
typename _ExtractKey, typename _Equal,
typename _Hash, typename _RangeHash, typename _Unused,
typename _RehashPolicy, typename _Traits>
class _Hashtable;
namespace __detail
{
template<typename _Key, typename _Value, typename _ExtractKey,
typename _Equal, typename _Hash, typename _RangeHash,
typename _Unused, typename _Traits>
struct _Hashtable_base;
template<typename _Iterator>
inline typename std::iterator_traits<_Iterator>::difference_type
__distance_fw(_Iterator __first, _Iterator __last,
std::input_iterator_tag)
{ return __first != __last ? 1 : 0; }
template<typename _Iterator>
inline typename std::iterator_traits<_Iterator>::difference_type
__distance_fw(_Iterator __first, _Iterator __last,
std::forward_iterator_tag)
{ return std::distance(__first, __last); }
template<typename _Iterator>
inline typename std::iterator_traits<_Iterator>::difference_type
__distance_fw(_Iterator __first, _Iterator __last)
{ return __distance_fw(__first, __last,
std::__iterator_category(__first)); }
struct _Identity
{
template<typename _Tp>
_Tp&&
operator()(_Tp&& __x) const noexcept
{ return std::forward<_Tp>(__x); }
};
struct _Select1st
{
template<typename _Pair>
struct __1st_type;
template<typename _Tp, typename _Up>
struct __1st_type<pair<_Tp, _Up>>
{ using type = _Tp; };
template<typename _Tp, typename _Up>
struct __1st_type<const pair<_Tp, _Up>>
{ using type = const _Tp; };
template<typename _Pair>
struct __1st_type<_Pair&>
{ using type = typename __1st_type<_Pair>::type&; };
template<typename _Tp>
typename __1st_type<_Tp>::type&&
operator()(_Tp&& __x) const noexcept
{ return std::forward<_Tp>(__x).first; }
};
template<typename _ExKey, typename _Value>
struct _ConvertToValueType;
template<typename _Value>
struct _ConvertToValueType<_Identity, _Value>
{
template<typename _Kt>
constexpr _Kt&&
operator()(_Kt&& __k) const noexcept
{ return std::forward<_Kt>(__k); }
};
template<typename _Value>
struct _ConvertToValueType<_Select1st, _Value>
{
constexpr _Value&&
operator()(_Value&& __x) const noexcept
{ return std::move(__x); }
constexpr const _Value&
operator()(const _Value& __x) const noexcept
{ return __x; }
template<typename _Kt, typename _Val>
constexpr std::pair<_Kt, _Val>&&
operator()(std::pair<_Kt, _Val>&& __x) const noexcept
{ return std::move(__x); }
template<typename _Kt, typename _Val>
constexpr const std::pair<_Kt, _Val>&
operator()(const std::pair<_Kt, _Val>& __x) const noexcept
{ return __x; }
};
template<typename _ExKey>
struct _NodeBuilder;
template<>
struct _NodeBuilder<_Select1st>
{
template<typename _Kt, typename _Arg, typename _NodeGenerator>
static auto
_S_build(_Kt&& __k, _Arg&& __arg, const _NodeGenerator& __node_gen)
-> typename _NodeGenerator::__node_type*
{
return __node_gen(std::forward<_Kt>(__k),
std::forward<_Arg>(__arg).second);
}
};
template<>
struct _NodeBuilder<_Identity>
{
template<typename _Kt, typename _Arg, typename _NodeGenerator>
static auto
_S_build(_Kt&& __k, _Arg&&, const _NodeGenerator& __node_gen)
-> typename _NodeGenerator::__node_type*
{ return __node_gen(std::forward<_Kt>(__k)); }
};
template<typename _NodeAlloc>
struct _Hashtable_alloc;
template<typename _NodeAlloc>
struct _ReuseOrAllocNode
{
private:
using __node_alloc_type = _NodeAlloc;
using __hashtable_alloc = _Hashtable_alloc<__node_alloc_type>;
using __node_alloc_traits =
typename __hashtable_alloc::__node_alloc_traits;
public:
using __node_type = typename __hashtable_alloc::__node_type;
_ReuseOrAllocNode(__node_type* __nodes, __hashtable_alloc& __h)
: _M_nodes(__nodes), _M_h(__h) { }
_ReuseOrAllocNode(const _ReuseOrAllocNode&) = delete;
~_ReuseOrAllocNode()
{ _M_h._M_deallocate_nodes(_M_nodes); }
template<typename... _Args>
__node_type*
operator()(_Args&&... __args) const
{
if (_M_nodes)
{
__node_type* __node = _M_nodes;
_M_nodes = _M_nodes->_M_next();
__node->_M_nxt = nullptr;
auto& __a = _M_h._M_node_allocator();
__node_alloc_traits::destroy(__a, __node->_M_valptr());
__try
{
__node_alloc_traits::construct(__a, __node->_M_valptr(),
std::forward<_Args>(__args)...);
}
__catch(...)
{
_M_h._M_deallocate_node_ptr(__node);
__throw_exception_again;
}
return __node;
}
return _M_h._M_allocate_node(std::forward<_Args>(__args)...);
}
private:
mutable __node_type* _M_nodes;
__hashtable_alloc& _M_h;
};
template<typename _NodeAlloc>
struct _AllocNode
{
private:
using __hashtable_alloc = _Hashtable_alloc<_NodeAlloc>;
public:
using __node_type = typename __hashtable_alloc::__node_type;
_AllocNode(__hashtable_alloc& __h)
: _M_h(__h) { }
template<typename... _Args>
__node_type*
operator()(_Args&&... __args) const
{ return _M_h._M_allocate_node(std::forward<_Args>(__args)...); }
private:
__hashtable_alloc& _M_h;
};
template<bool _Cache_hash_code, bool _Constant_iterators, bool _Unique_keys>
struct _Hashtable_traits
{
using __hash_cached = __bool_constant<_Cache_hash_code>;
using __constant_iterators = __bool_constant<_Constant_iterators>;
using __unique_keys = __bool_constant<_Unique_keys>;
};
template<typename _Hash>
struct _Hashtable_hash_traits
{
static constexpr std::size_t
__small_size_threshold() noexcept
{ return std::__is_fast_hash<_Hash>::value ? 0 : 20; }
};
struct _Hash_node_base
{
_Hash_node_base* _M_nxt;
_Hash_node_base() noexcept : _M_nxt() { }
_Hash_node_base(_Hash_node_base* __next) noexcept : _M_nxt(__next) { }
};
template<typename _Value>
struct _Hash_node_value_base
{
typedef _Value value_type;
__gnu_cxx::__aligned_buffer<_Value> _M_storage;
_Value*
_M_valptr() noexcept
{ return _M_storage._M_ptr(); }
const _Value*
_M_valptr() const noexcept
{ return _M_storage._M_ptr(); }
_Value&
_M_v() noexcept
{ return *_M_valptr(); }
const _Value&
_M_v() const noexcept
{ return *_M_valptr(); }
};
template<bool _Cache_hash_code>
struct _Hash_node_code_cache
{ };
template<>
struct _Hash_node_code_cache<true>
{ std::size_t  _M_hash_code; };
template<typename _Value, bool _Cache_hash_code>
struct _Hash_node_value
: _Hash_node_value_base<_Value>
, _Hash_node_code_cache<_Cache_hash_code>
{ };
template<typename _Value, bool _Cache_hash_code>
struct _Hash_node
: _Hash_node_base
, _Hash_node_value<_Value, _Cache_hash_code>
{
_Hash_node*
_M_next() const noexcept
{ return static_cast<_Hash_node*>(this->_M_nxt); }
};
template<typename _Value, bool _Cache_hash_code>
struct _Node_iterator_base
{
using __node_type = _Hash_node<_Value, _Cache_hash_code>;
__node_type* _M_cur;
_Node_iterator_base() : _M_cur(nullptr) { }
_Node_iterator_base(__node_type* __p) noexcept
: _M_cur(__p) { }
void
_M_incr() noexcept
{ _M_cur = _M_cur->_M_next(); }
friend bool
operator==(const _Node_iterator_base& __x, const _Node_iterator_base& __y)
noexcept
{ return __x._M_cur == __y._M_cur; }
#if __cpp_impl_three_way_comparison < 201907L
friend bool
operator!=(const _Node_iterator_base& __x, const _Node_iterator_base& __y)
noexcept
{ return __x._M_cur != __y._M_cur; }
#endif
};
template<typename _Value, bool __constant_iterators, bool __cache>
struct _Node_iterator
: public _Node_iterator_base<_Value, __cache>
{
private:
using __base_type = _Node_iterator_base<_Value, __cache>;
using __node_type = typename __base_type::__node_type;
public:
using value_type = _Value;
using difference_type = std::ptrdiff_t;
using iterator_category = std::forward_iterator_tag;
using pointer = __conditional_t<__constant_iterators,
const value_type*, value_type*>;
using reference = __conditional_t<__constant_iterators,
const value_type&, value_type&>;
_Node_iterator() = default;
explicit
_Node_iterator(__node_type* __p) noexcept
: __base_type(__p) { }
reference
operator*() const noexcept
{ return this->_M_cur->_M_v(); }
pointer
operator->() const noexcept
{ return this->_M_cur->_M_valptr(); }
_Node_iterator&
operator++() noexcept
{
this->_M_incr();
return *this;
}
_Node_iterator
operator++(int) noexcept
{
_Node_iterator __tmp(*this);
this->_M_incr();
return __tmp;
}
};
template<typename _Value, bool __constant_iterators, bool __cache>
struct _Node_const_iterator
: public _Node_iterator_base<_Value, __cache>
{
private:
using __base_type = _Node_iterator_base<_Value, __cache>;
using __node_type = typename __base_type::__node_type;
public:
typedef _Value					value_type;
typedef std::ptrdiff_t				difference_type;
typedef std::forward_iterator_tag			iterator_category;
typedef const value_type*				pointer;
typedef const value_type&				reference;
_Node_const_iterator() = default;
explicit
_Node_const_iterator(__node_type* __p) noexcept
: __base_type(__p) { }
_Node_const_iterator(const _Node_iterator<_Value, __constant_iterators,
__cache>& __x) noexcept
: __base_type(__x._M_cur) { }
reference
operator*() const noexcept
{ return this->_M_cur->_M_v(); }
pointer
operator->() const noexcept
{ return this->_M_cur->_M_valptr(); }
_Node_const_iterator&
operator++() noexcept
{
this->_M_incr();
return *this;
}
_Node_const_iterator
operator++(int) noexcept
{
_Node_const_iterator __tmp(*this);
this->_M_incr();
return __tmp;
}
};
struct _Mod_range_hashing
{
typedef std::size_t first_argument_type;
typedef std::size_t second_argument_type;
typedef std::size_t result_type;
result_type
operator()(first_argument_type __num,
second_argument_type __den) const noexcept
{ return __num % __den; }
};
struct _Default_ranged_hash { };
struct _Prime_rehash_policy
{
using __has_load_factor = true_type;
_Prime_rehash_policy(float __z = 1.0) noexcept
: _M_max_load_factor(__z), _M_next_resize(0) { }
float
max_load_factor() const noexcept
{ return _M_max_load_factor; }
std::size_t
_M_next_bkt(std::size_t __n) const;
std::size_t
_M_bkt_for_elements(std::size_t __n) const
{ return __builtin_ceil(__n / (double)_M_max_load_factor); }
std::pair<bool, std::size_t>
_M_need_rehash(std::size_t __n_bkt, std::size_t __n_elt,
std::size_t __n_ins) const;
typedef std::size_t _State;
_State
_M_state() const
{ return _M_next_resize; }
void
_M_reset() noexcept
{ _M_next_resize = 0; }
void
_M_reset(_State __state)
{ _M_next_resize = __state; }
static const std::size_t _S_growth_factor = 2;
float		_M_max_load_factor;
mutable std::size_t	_M_next_resize;
};
struct _Mask_range_hashing
{
typedef std::size_t first_argument_type;
typedef std::size_t second_argument_type;
typedef std::size_t result_type;
result_type
operator()(first_argument_type __num,
second_argument_type __den) const noexcept
{ return __num & (__den - 1); }
};
inline std::size_t
__clp2(std::size_t __n) noexcept
{
using __gnu_cxx::__int_traits;
if (__n < 2)
return __n;
const unsigned __lz = sizeof(size_t) > sizeof(long)
? __builtin_clzll(__n - 1ull)
: __builtin_clzl(__n - 1ul);
return (size_t(1) << (__int_traits<size_t>::__digits - __lz - 1)) << 1;
}
struct _Power2_rehash_policy
{
using __has_load_factor = true_type;
_Power2_rehash_policy(float __z = 1.0) noexcept
: _M_max_load_factor(__z), _M_next_resize(0) { }
float
max_load_factor() const noexcept
{ return _M_max_load_factor; }
std::size_t
_M_next_bkt(std::size_t __n) noexcept
{
if (__n == 0)
return 1;
const auto __max_width = std::min<size_t>(sizeof(size_t), 8);
const auto __max_bkt = size_t(1) << (__max_width * __CHAR_BIT__ - 1);
std::size_t __res = __clp2(__n);
if (__res == 0)
__res = __max_bkt;
else if (__res == 1)
__res = 2;
if (__res == __max_bkt)
_M_next_resize = size_t(-1);
else
_M_next_resize
= __builtin_floor(__res * (double)_M_max_load_factor);
return __res;
}
std::size_t
_M_bkt_for_elements(std::size_t __n) const noexcept
{ return __builtin_ceil(__n / (double)_M_max_load_factor); }
std::pair<bool, std::size_t>
_M_need_rehash(std::size_t __n_bkt, std::size_t __n_elt,
std::size_t __n_ins) noexcept
{
if (__n_elt + __n_ins > _M_next_resize)
{
double __min_bkts
= std::max<std::size_t>(__n_elt + __n_ins, _M_next_resize ? 0 : 11)
/ (double)_M_max_load_factor;
if (__min_bkts >= __n_bkt)
return { true,
_M_next_bkt(std::max<std::size_t>(__builtin_floor(__min_bkts) + 1,
__n_bkt * _S_growth_factor)) };
_M_next_resize
= __builtin_floor(__n_bkt * (double)_M_max_load_factor);
return { false, 0 };
}
else
return { false, 0 };
}
typedef std::size_t _State;
_State
_M_state() const noexcept
{ return _M_next_resize; }
void
_M_reset() noexcept
{ _M_next_resize = 0; }
void
_M_reset(_State __state) noexcept
{ _M_next_resize = __state; }
static const std::size_t _S_growth_factor = 2;
float	_M_max_load_factor;
std::size_t	_M_next_resize;
};
template<typename _Key, typename _Value, typename _Alloc,
typename _ExtractKey, typename _Equal,
typename _Hash, typename _RangeHash, typename _Unused,
typename _RehashPolicy, typename _Traits,
bool _Unique_keys = _Traits::__unique_keys::value>
struct _Map_base { };
template<typename _Key, typename _Val, typename _Alloc, typename _Equal,
typename _Hash, typename _RangeHash, typename _Unused,
typename _RehashPolicy, typename _Traits>
struct _Map_base<_Key, pair<const _Key, _Val>, _Alloc, _Select1st, _Equal,
_Hash, _RangeHash, _Unused, _RehashPolicy, _Traits, false>
{
using mapped_type = _Val;
};
template<typename _Key, typename _Val, typename _Alloc, typename _Equal,
typename _Hash, typename _RangeHash, typename _Unused,
typename _RehashPolicy, typename _Traits>
struct _Map_base<_Key, pair<const _Key, _Val>, _Alloc, _Select1st, _Equal,
_Hash, _RangeHash, _Unused, _RehashPolicy, _Traits, true>
{
private:
using __hashtable_base = _Hashtable_base<_Key, pair<const _Key, _Val>,
_Select1st, _Equal, _Hash,
_RangeHash, _Unused,
_Traits>;
using __hashtable = _Hashtable<_Key, pair<const _Key, _Val>, _Alloc,
_Select1st, _Equal, _Hash, _RangeHash,
_Unused, _RehashPolicy, _Traits>;
using __hash_code = typename __hashtable_base::__hash_code;
public:
using key_type = typename __hashtable_base::key_type;
using mapped_type = _Val;
mapped_type&
operator[](const key_type& __k);
mapped_type&
operator[](key_type&& __k);
mapped_type&
at(const key_type& __k)
{
auto __ite = static_cast<__hashtable*>(this)->find(__k);
if (!__ite._M_cur)
__throw_out_of_range(__N("unordered_map::at"));
return __ite->second;
}
const mapped_type&
at(const key_type& __k) const
{
auto __ite = static_cast<const __hashtable*>(this)->find(__k);
if (!__ite._M_cur)
__throw_out_of_range(__N("unordered_map::at"));
return __ite->second;
}
};
template<typename _Key, typename _Val, typename _Alloc, typename _Equal,
typename _Hash, typename _RangeHash, typename _Unused,
typename _RehashPolicy, typename _Traits>
auto
_Map_base<_Key, pair<const _Key, _Val>, _Alloc, _Select1st, _Equal,
_Hash, _RangeHash, _Unused, _RehashPolicy, _Traits, true>::
operator[](const key_type& __k)
-> mapped_type&
{
__hashtable* __h = static_cast<__hashtable*>(this);
__hash_code __code = __h->_M_hash_code(__k);
std::size_t __bkt = __h->_M_bucket_index(__code);
if (auto __node = __h->_M_find_node(__bkt, __k, __code))
return __node->_M_v().second;
typename __hashtable::_Scoped_node __node {
__h,
std::piecewise_construct,
std::tuple<const key_type&>(__k),
std::tuple<>()
};
auto __pos
= __h->_M_insert_unique_node(__bkt, __code, __node._M_node);
__node._M_node = nullptr;
return __pos->second;
}
template<typename _Key, typename _Val, typename _Alloc, typename _Equal,
typename _Hash, typename _RangeHash, typename _Unused,
typename _RehashPolicy, typename _Traits>
auto
_Map_base<_Key, pair<const _Key, _Val>, _Alloc, _Select1st, _Equal,
_Hash, _RangeHash, _Unused, _RehashPolicy, _Traits, true>::
operator[](key_type&& __k)
-> mapped_type&
{
__hashtable* __h = static_cast<__hashtable*>(this);
__hash_code __code = __h->_M_hash_code(__k);
std::size_t __bkt = __h->_M_bucket_index(__code);
if (auto __node = __h->_M_find_node(__bkt, __k, __code))
return __node->_M_v().second;
typename __hashtable::_Scoped_node __node {
__h,
std::piecewise_construct,
std::forward_as_tuple(std::move(__k)),
std::tuple<>()
};
auto __pos
= __h->_M_insert_unique_node(__bkt, __code, __node._M_node);
__node._M_node = nullptr;
return __pos->second;
}
template<typename _Key, typename _Val, typename _Alloc, typename _Equal,
typename _Hash, typename _RangeHash, typename _Unused,
typename _RehashPolicy, typename _Traits, bool __uniq>
struct _Map_base<const _Key, pair<const _Key, _Val>,
_Alloc, _Select1st, _Equal, _Hash,
_RangeHash, _Unused, _RehashPolicy, _Traits, __uniq>
: _Map_base<_Key, pair<const _Key, _Val>, _Alloc, _Select1st, _Equal, _Hash,
_RangeHash, _Unused, _RehashPolicy, _Traits, __uniq>
{ };
template<typename _Key, typename _Value, typename _Alloc,
typename _ExtractKey, typename _Equal,
typename _Hash, typename _RangeHash, typename _Unused,
typename _RehashPolicy, typename _Traits>
struct _Insert_base
{
protected:
using __hashtable_base = _Hashtable_base<_Key, _Value, _ExtractKey,
_Equal, _Hash, _RangeHash,
_Unused, _Traits>;
using __hashtable = _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
_Hash, _RangeHash,
_Unused, _RehashPolicy, _Traits>;
using __hash_cached = typename _Traits::__hash_cached;
using __constant_iterators = typename _Traits::__constant_iterators;
using __hashtable_alloc = _Hashtable_alloc<
__alloc_rebind<_Alloc, _Hash_node<_Value,
__hash_cached::value>>>;
using value_type = typename __hashtable_base::value_type;
using size_type = typename __hashtable_base::size_type;
using __unique_keys = typename _Traits::__unique_keys;
using __node_alloc_type = typename __hashtable_alloc::__node_alloc_type;
using __node_gen_type = _AllocNode<__node_alloc_type>;
__hashtable&
_M_conjure_hashtable()
{ return *(static_cast<__hashtable*>(this)); }
template<typename _InputIterator, typename _NodeGetter>
void
_M_insert_range(_InputIterator __first, _InputIterator __last,
const _NodeGetter&, true_type __uks);
template<typename _InputIterator, typename _NodeGetter>
void
_M_insert_range(_InputIterator __first, _InputIterator __last,
const _NodeGetter&, false_type __uks);
public:
using iterator = _Node_iterator<_Value, __constant_iterators::value,
__hash_cached::value>;
using const_iterator = _Node_const_iterator<_Value,
__constant_iterators::value,
__hash_cached::value>;
using __ireturn_type = __conditional_t<__unique_keys::value,
std::pair<iterator, bool>,
iterator>;
__ireturn_type
insert(const value_type& __v)
{
__hashtable& __h = _M_conjure_hashtable();
__node_gen_type __node_gen(__h);
return __h._M_insert(__v, __node_gen, __unique_keys{});
}
iterator
insert(const_iterator __hint, const value_type& __v)
{
__hashtable& __h = _M_conjure_hashtable();
__node_gen_type __node_gen(__h);
return __h._M_insert(__hint, __v, __node_gen, __unique_keys{});
}
template<typename _KType, typename... _Args>
std::pair<iterator, bool>
try_emplace(const_iterator, _KType&& __k, _Args&&... __args)
{
__hashtable& __h = _M_conjure_hashtable();
auto __code = __h._M_hash_code(__k);
std::size_t __bkt = __h._M_bucket_index(__code);
if (auto __node = __h._M_find_node(__bkt, __k, __code))
return { iterator(__node), false };
typename __hashtable::_Scoped_node __node {
&__h,
std::piecewise_construct,
std::forward_as_tuple(std::forward<_KType>(__k)),
std::forward_as_tuple(std::forward<_Args>(__args)...)
};
auto __it
= __h._M_insert_unique_node(__bkt, __code, __node._M_node);
__node._M_node = nullptr;
return { __it, true };
}
void
insert(initializer_list<value_type> __l)
{ this->insert(__l.begin(), __l.end()); }
template<typename _InputIterator>
void
insert(_InputIterator __first, _InputIterator __last)
{
__hashtable& __h = _M_conjure_hashtable();
__node_gen_type __node_gen(__h);
return _M_insert_range(__first, __last, __node_gen, __unique_keys{});
}
};
template<typename _Key, typename _Value, typename _Alloc,
typename _ExtractKey, typename _Equal,
typename _Hash, typename _RangeHash, typename _Unused,
typename _RehashPolicy, typename _Traits>
template<typename _InputIterator, typename _NodeGetter>
void
_Insert_base<_Key, _Value, _Alloc, _ExtractKey, _Equal,
_Hash, _RangeHash, _Unused,
_RehashPolicy, _Traits>::
_M_insert_range(_InputIterator __first, _InputIterator __last,
const _NodeGetter& __node_gen, true_type __uks)
{
__hashtable& __h = _M_conjure_hashtable();
for (; __first != __last; ++__first)
__h._M_insert(*__first, __node_gen, __uks);
}
template<typename _Key, typename _Value, typename _Alloc,
typename _ExtractKey, typename _Equal,
typename _Hash, typename _RangeHash, typename _Unused,
typename _RehashPolicy, typename _Traits>
template<typename _InputIterator, typename _NodeGetter>
void
_Insert_base<_Key, _Value, _Alloc, _ExtractKey, _Equal,
_Hash, _RangeHash, _Unused,
_RehashPolicy, _Traits>::
_M_insert_range(_InputIterator __first, _InputIterator __last,
const _NodeGetter& __node_gen, false_type __uks)
{
using __rehash_type = typename __hashtable::__rehash_type;
using __rehash_state = typename __hashtable::__rehash_state;
using pair_type = std::pair<bool, std::size_t>;
size_type __n_elt = __detail::__distance_fw(__first, __last);
if (__n_elt == 0)
return;
__hashtable& __h = _M_conjure_hashtable();
__rehash_type& __rehash = __h._M_rehash_policy;
const __rehash_state& __saved_state = __rehash._M_state();
pair_type __do_rehash = __rehash._M_need_rehash(__h._M_bucket_count,
__h._M_element_count,
__n_elt);
if (__do_rehash.first)
__h._M_rehash(__do_rehash.second, __saved_state);
for (; __first != __last; ++__first)
__h._M_insert(*__first, __node_gen, __uks);
}
template<typename _Key, typename _Value, typename _Alloc,
typename _ExtractKey, typename _Equal,
typename _Hash, typename _RangeHash, typename _Unused,
typename _RehashPolicy, typename _Traits,
bool _Constant_iterators = _Traits::__constant_iterators::value>
struct _Insert;
template<typename _Key, typename _Value, typename _Alloc,
typename _ExtractKey, typename _Equal,
typename _Hash, typename _RangeHash, typename _Unused,
typename _RehashPolicy, typename _Traits>
struct _Insert<_Key, _Value, _Alloc, _ExtractKey, _Equal,
_Hash, _RangeHash, _Unused,
_RehashPolicy, _Traits, true>
: public _Insert_base<_Key, _Value, _Alloc, _ExtractKey, _Equal,
_Hash, _RangeHash, _Unused, _RehashPolicy, _Traits>
{
using __base_type = _Insert_base<_Key, _Value, _Alloc, _ExtractKey,
_Equal, _Hash, _RangeHash, _Unused,
_RehashPolicy, _Traits>;
using value_type = typename __base_type::value_type;
using iterator = typename __base_type::iterator;
using const_iterator =  typename __base_type::const_iterator;
using __ireturn_type = typename __base_type::__ireturn_type;
using __unique_keys = typename __base_type::__unique_keys;
using __hashtable = typename __base_type::__hashtable;
using __node_gen_type = typename __base_type::__node_gen_type;
using __base_type::insert;
__ireturn_type
insert(value_type&& __v)
{
__hashtable& __h = this->_M_conjure_hashtable();
__node_gen_type __node_gen(__h);
return __h._M_insert(std::move(__v), __node_gen, __unique_keys{});
}
iterator
insert(const_iterator __hint, value_type&& __v)
{
__hashtable& __h = this->_M_conjure_hashtable();
__node_gen_type __node_gen(__h);
return __h._M_insert(__hint, std::move(__v), __node_gen,
__unique_keys{});
}
};
template<typename _Key, typename _Value, typename _Alloc,
typename _ExtractKey, typename _Equal,
typename _Hash, typename _RangeHash, typename _Unused,
typename _RehashPolicy, typename _Traits>
struct _Insert<_Key, _Value, _Alloc, _ExtractKey, _Equal,
_Hash, _RangeHash, _Unused, _RehashPolicy, _Traits, false>
: public _Insert_base<_Key, _Value, _Alloc, _ExtractKey, _Equal,
_Hash, _RangeHash, _Unused, _RehashPolicy, _Traits>
{
using __base_type = _Insert_base<_Key, _Value, _Alloc, _ExtractKey,
_Equal, _Hash, _RangeHash, _Unused,
_RehashPolicy, _Traits>;
using value_type = typename __base_type::value_type;
using iterator = typename __base_type::iterator;
using const_iterator =  typename __base_type::const_iterator;
using __unique_keys = typename __base_type::__unique_keys;
using __hashtable = typename __base_type::__hashtable;
using __ireturn_type = typename __base_type::__ireturn_type;
using __base_type::insert;
template<typename _Pair>
using __is_cons = std::is_constructible<value_type, _Pair&&>;
template<typename _Pair>
using _IFcons = std::enable_if<__is_cons<_Pair>::value>;
template<typename _Pair>
using _IFconsp = typename _IFcons<_Pair>::type;
template<typename _Pair, typename = _IFconsp<_Pair>>
__ireturn_type
insert(_Pair&& __v)
{
__hashtable& __h = this->_M_conjure_hashtable();
return __h._M_emplace(__unique_keys{}, std::forward<_Pair>(__v));
}
template<typename _Pair, typename = _IFconsp<_Pair>>
iterator
insert(const_iterator __hint, _Pair&& __v)
{
__hashtable& __h = this->_M_conjure_hashtable();
return __h._M_emplace(__hint, __unique_keys{},
std::forward<_Pair>(__v));
}
};
template<typename _Policy>
using __has_load_factor = typename _Policy::__has_load_factor;
template<typename _Key, typename _Value, typename _Alloc,
typename _ExtractKey, typename _Equal,
typename _Hash, typename _RangeHash, typename _Unused,
typename _RehashPolicy, typename _Traits,
typename =
__detected_or_t<false_type, __has_load_factor, _RehashPolicy>>
struct _Rehash_base;
template<typename _Key, typename _Value, typename _Alloc,
typename _ExtractKey, typename _Equal,
typename _Hash, typename _RangeHash, typename _Unused,
typename _RehashPolicy, typename _Traits>
struct _Rehash_base<_Key, _Value, _Alloc, _ExtractKey, _Equal,
_Hash, _RangeHash, _Unused, _RehashPolicy, _Traits,
false_type >
{
};
template<typename _Key, typename _Value, typename _Alloc,
typename _ExtractKey, typename _Equal,
typename _Hash, typename _RangeHash, typename _Unused,
typename _RehashPolicy, typename _Traits>
struct _Rehash_base<_Key, _Value, _Alloc, _ExtractKey, _Equal,
_Hash, _RangeHash, _Unused, _RehashPolicy, _Traits,
true_type >
{
private:
using __hashtable = _Hashtable<_Key, _Value, _Alloc, _ExtractKey,
_Equal, _Hash, _RangeHash, _Unused,
_RehashPolicy, _Traits>;
public:
float
max_load_factor() const noexcept
{
const __hashtable* __this = static_cast<const __hashtable*>(this);
return __this->__rehash_policy().max_load_factor();
}
void
max_load_factor(float __z)
{
__hashtable* __this = static_cast<__hashtable*>(this);
__this->__rehash_policy(_RehashPolicy(__z));
}
void
reserve(std::size_t __n)
{
__hashtable* __this = static_cast<__hashtable*>(this);
__this->rehash(__this->__rehash_policy()._M_bkt_for_elements(__n));
}
};
template<int _Nm, typename _Tp,
bool __use_ebo = !__is_final(_Tp) && __is_empty(_Tp)>
struct _Hashtable_ebo_helper;
template<int _Nm, typename _Tp>
struct _Hashtable_ebo_helper<_Nm, _Tp, true>
: private _Tp
{
_Hashtable_ebo_helper() noexcept(noexcept(_Tp())) : _Tp() { }
template<typename _OtherTp>
_Hashtable_ebo_helper(_OtherTp&& __tp)
: _Tp(std::forward<_OtherTp>(__tp))
{ }
const _Tp& _M_cget() const { return static_cast<const _Tp&>(*this); }
_Tp& _M_get() { return static_cast<_Tp&>(*this); }
};
template<int _Nm, typename _Tp>
struct _Hashtable_ebo_helper<_Nm, _Tp, false>
{
_Hashtable_ebo_helper() = default;
template<typename _OtherTp>
_Hashtable_ebo_helper(_OtherTp&& __tp)
: _M_tp(std::forward<_OtherTp>(__tp))
{ }
const _Tp& _M_cget() const { return _M_tp; }
_Tp& _M_get() { return _M_tp; }
private:
_Tp _M_tp{};
};
template<typename _Key, typename _Value, typename _ExtractKey,
typename _Hash, typename _RangeHash, typename _Unused,
bool __cache_hash_code>
struct _Local_iterator_base;
template<typename _Key, typename _Value, typename _ExtractKey,
typename _Hash, typename _RangeHash, typename _Unused,
bool __cache_hash_code>
struct _Hash_code_base
: private _Hashtable_ebo_helper<1, _Hash>
{
private:
using __ebo_hash = _Hashtable_ebo_helper<1, _Hash>;
friend struct _Local_iterator_base<_Key, _Value, _ExtractKey,
_Hash, _RangeHash, _Unused, false>;
public:
typedef _Hash					hasher;
hasher
hash_function() const
{ return _M_hash(); }
protected:
typedef std::size_t 				__hash_code;
_Hash_code_base() = default;
_Hash_code_base(const _Hash& __hash) : __ebo_hash(__hash) { }
__hash_code
_M_hash_code(const _Key& __k) const
{
static_assert(__is_invocable<const _Hash&, const _Key&>{},
"hash function must be invocable with an argument of key type");
return _M_hash()(__k);
}
template<typename _Kt>
__hash_code
_M_hash_code_tr(const _Kt& __k) const
{
static_assert(__is_invocable<const _Hash&, const _Kt&>{},
"hash function must be invocable with an argument of key type");
return _M_hash()(__k);
}
__hash_code
_M_hash_code(const _Hash&,
const _Hash_node_value<_Value, true>& __n) const
{ return __n._M_hash_code; }
template<typename _H2>
__hash_code
_M_hash_code(const _H2&,
const _Hash_node_value<_Value, __cache_hash_code>& __n) const
{ return _M_hash_code(_ExtractKey{}(__n._M_v())); }
__hash_code
_M_hash_code(const _Hash_node_value<_Value, false>& __n) const
{ return _M_hash_code(_ExtractKey{}(__n._M_v())); }
__hash_code
_M_hash_code(const _Hash_node_value<_Value, true>& __n) const
{ return __n._M_hash_code; }
std::size_t
_M_bucket_index(__hash_code __c, std::size_t __bkt_count) const
{ return _RangeHash{}(__c, __bkt_count); }
std::size_t
_M_bucket_index(const _Hash_node_value<_Value, false>& __n,
std::size_t __bkt_count) const
noexcept( noexcept(declval<const _Hash&>()(declval<const _Key&>()))
&& noexcept(declval<const _RangeHash&>()((__hash_code)0,
(std::size_t)0)) )
{
return _RangeHash{}(_M_hash_code(_ExtractKey{}(__n._M_v())),
__bkt_count);
}
std::size_t
_M_bucket_index(const _Hash_node_value<_Value, true>& __n,
std::size_t __bkt_count) const
noexcept( noexcept(declval<const _RangeHash&>()((__hash_code)0,
(std::size_t)0)) )
{ return _RangeHash{}(__n._M_hash_code, __bkt_count); }
void
_M_store_code(_Hash_node_code_cache<false>&, __hash_code) const
{ }
void
_M_copy_code(_Hash_node_code_cache<false>&,
const _Hash_node_code_cache<false>&) const
{ }
void
_M_store_code(_Hash_node_code_cache<true>& __n, __hash_code __c) const
{ __n._M_hash_code = __c; }
void
_M_copy_code(_Hash_node_code_cache<true>& __to,
const _Hash_node_code_cache<true>& __from) const
{ __to._M_hash_code = __from._M_hash_code; }
void
_M_swap(_Hash_code_base& __x)
{ std::swap(__ebo_hash::_M_get(), __x.__ebo_hash::_M_get()); }
const _Hash&
_M_hash() const { return __ebo_hash::_M_cget(); }
};
template<typename _Key, typename _Value, typename _ExtractKey,
typename _Hash, typename _RangeHash, typename _Unused>
struct _Local_iterator_base<_Key, _Value, _ExtractKey,
_Hash, _RangeHash, _Unused, true>
: public _Node_iterator_base<_Value, true>
{
protected:
using __base_node_iter = _Node_iterator_base<_Value, true>;
using __hash_code_base = _Hash_code_base<_Key, _Value, _ExtractKey,
_Hash, _RangeHash, _Unused, true>;
_Local_iterator_base() = default;
_Local_iterator_base(const __hash_code_base&,
_Hash_node<_Value, true>* __p,
std::size_t __bkt, std::size_t __bkt_count)
: __base_node_iter(__p), _M_bucket(__bkt), _M_bucket_count(__bkt_count)
{ }
void
_M_incr()
{
__base_node_iter::_M_incr();
if (this->_M_cur)
{
std::size_t __bkt
= _RangeHash{}(this->_M_cur->_M_hash_code, _M_bucket_count);
if (__bkt != _M_bucket)
this->_M_cur = nullptr;
}
}
std::size_t _M_bucket;
std::size_t _M_bucket_count;
public:
std::size_t
_M_get_bucket() const { return _M_bucket; }
};
template<typename _Tp, bool _IsEmpty = std::is_empty<_Tp>::value>
struct _Hash_code_storage
{
__gnu_cxx::__aligned_buffer<_Tp> _M_storage;
_Tp*
_M_h() { return _M_storage._M_ptr(); }
const _Tp*
_M_h() const { return _M_storage._M_ptr(); }
};
template<typename _Tp>
struct _Hash_code_storage<_Tp, true>
{
static_assert( std::is_empty<_Tp>::value, "Type must be empty" );
_Tp*
_M_h() { return reinterpret_cast<_Tp*>(this); }
const _Tp*
_M_h() const { return reinterpret_cast<const _Tp*>(this); }
};
template<typename _Key, typename _Value, typename _ExtractKey,
typename _Hash, typename _RangeHash, typename _Unused>
using __hash_code_for_local_iter
= _Hash_code_storage<_Hash_code_base<_Key, _Value, _ExtractKey,
_Hash, _RangeHash, _Unused, false>>;
template<typename _Key, typename _Value, typename _ExtractKey,
typename _Hash, typename _RangeHash, typename _Unused>
struct _Local_iterator_base<_Key, _Value, _ExtractKey,
_Hash, _RangeHash, _Unused, false>
: __hash_code_for_local_iter<_Key, _Value, _ExtractKey, _Hash, _RangeHash,
_Unused>
, _Node_iterator_base<_Value, false>
{
protected:
using __hash_code_base = _Hash_code_base<_Key, _Value, _ExtractKey,
_Hash, _RangeHash, _Unused, false>;
using __node_iter_base = _Node_iterator_base<_Value, false>;
_Local_iterator_base() : _M_bucket_count(-1) { }
_Local_iterator_base(const __hash_code_base& __base,
_Hash_node<_Value, false>* __p,
std::size_t __bkt, std::size_t __bkt_count)
: __node_iter_base(__p), _M_bucket(__bkt), _M_bucket_count(__bkt_count)
{ _M_init(__base); }
~_Local_iterator_base()
{
if (_M_bucket_count != size_t(-1))
_M_destroy();
}
_Local_iterator_base(const _Local_iterator_base& __iter)
: __node_iter_base(__iter._M_cur), _M_bucket(__iter._M_bucket)
, _M_bucket_count(__iter._M_bucket_count)
{
if (_M_bucket_count != size_t(-1))
_M_init(*__iter._M_h());
}
_Local_iterator_base&
operator=(const _Local_iterator_base& __iter)
{
if (_M_bucket_count != -1)
_M_destroy();
this->_M_cur = __iter._M_cur;
_M_bucket = __iter._M_bucket;
_M_bucket_count = __iter._M_bucket_count;
if (_M_bucket_count != -1)
_M_init(*__iter._M_h());
return *this;
}
void
_M_incr()
{
__node_iter_base::_M_incr();
if (this->_M_cur)
{
std::size_t __bkt = this->_M_h()->_M_bucket_index(*this->_M_cur,
_M_bucket_count);
if (__bkt != _M_bucket)
this->_M_cur = nullptr;
}
}
std::size_t _M_bucket;
std::size_t _M_bucket_count;
void
_M_init(const __hash_code_base& __base)
{ ::new(this->_M_h()) __hash_code_base(__base); }
void
_M_destroy() { this->_M_h()->~__hash_code_base(); }
public:
std::size_t
_M_get_bucket() const { return _M_bucket; }
};
template<typename _Key, typename _Value, typename _ExtractKey,
typename _Hash, typename _RangeHash, typename _Unused,
bool __constant_iterators, bool __cache>
struct _Local_iterator
: public _Local_iterator_base<_Key, _Value, _ExtractKey,
_Hash, _RangeHash, _Unused, __cache>
{
private:
using __base_type = _Local_iterator_base<_Key, _Value, _ExtractKey,
_Hash, _RangeHash, _Unused, __cache>;
using __hash_code_base = typename __base_type::__hash_code_base;
public:
using value_type = _Value;
using pointer = __conditional_t<__constant_iterators,
const value_type*, value_type*>;
using reference = __conditional_t<__constant_iterators,
const value_type&, value_type&>;
using difference_type = ptrdiff_t;
using iterator_category = forward_iterator_tag;
_Local_iterator() = default;
_Local_iterator(const __hash_code_base& __base,
_Hash_node<_Value, __cache>* __n,
std::size_t __bkt, std::size_t __bkt_count)
: __base_type(__base, __n, __bkt, __bkt_count)
{ }
reference
operator*() const
{ return this->_M_cur->_M_v(); }
pointer
operator->() const
{ return this->_M_cur->_M_valptr(); }
_Local_iterator&
operator++()
{
this->_M_incr();
return *this;
}
_Local_iterator
operator++(int)
{
_Local_iterator __tmp(*this);
this->_M_incr();
return __tmp;
}
};
template<typename _Key, typename _Value, typename _ExtractKey,
typename _Hash, typename _RangeHash, typename _Unused,
bool __constant_iterators, bool __cache>
struct _Local_const_iterator
: public _Local_iterator_base<_Key, _Value, _ExtractKey,
_Hash, _RangeHash, _Unused, __cache>
{
private:
using __base_type = _Local_iterator_base<_Key, _Value, _ExtractKey,
_Hash, _RangeHash, _Unused, __cache>;
using __hash_code_base = typename __base_type::__hash_code_base;
public:
typedef _Value					value_type;
typedef const value_type*				pointer;
typedef const value_type&				reference;
typedef std::ptrdiff_t				difference_type;
typedef std::forward_iterator_tag			iterator_category;
_Local_const_iterator() = default;
_Local_const_iterator(const __hash_code_base& __base,
_Hash_node<_Value, __cache>* __n,
std::size_t __bkt, std::size_t __bkt_count)
: __base_type(__base, __n, __bkt, __bkt_count)
{ }
_Local_const_iterator(const _Local_iterator<_Key, _Value, _ExtractKey,
_Hash, _RangeHash, _Unused,
__constant_iterators,
__cache>& __x)
: __base_type(__x)
{ }
reference
operator*() const
{ return this->_M_cur->_M_v(); }
pointer
operator->() const
{ return this->_M_cur->_M_valptr(); }
_Local_const_iterator&
operator++()
{
this->_M_incr();
return *this;
}
_Local_const_iterator
operator++(int)
{
_Local_const_iterator __tmp(*this);
this->_M_incr();
return __tmp;
}
};
template<typename _Key, typename _Value, typename _ExtractKey,
typename _Equal, typename _Hash, typename _RangeHash,
typename _Unused, typename _Traits>
struct _Hashtable_base
: public _Hash_code_base<_Key, _Value, _ExtractKey, _Hash, _RangeHash,
_Unused, _Traits::__hash_cached::value>,
private _Hashtable_ebo_helper<0, _Equal>
{
public:
typedef _Key					key_type;
typedef _Value					value_type;
typedef _Equal					key_equal;
typedef std::size_t				size_type;
typedef std::ptrdiff_t				difference_type;
using __traits_type = _Traits;
using __hash_cached = typename __traits_type::__hash_cached;
using __hash_code_base = _Hash_code_base<_Key, _Value, _ExtractKey,
_Hash, _RangeHash, _Unused,
__hash_cached::value>;
using __hash_code = typename __hash_code_base::__hash_code;
private:
using _EqualEBO = _Hashtable_ebo_helper<0, _Equal>;
static bool
_S_equals(__hash_code, const _Hash_node_code_cache<false>&)
{ return true; }
static bool
_S_node_equals(const _Hash_node_code_cache<false>&,
const _Hash_node_code_cache<false>&)
{ return true; }
static bool
_S_equals(__hash_code __c, const _Hash_node_code_cache<true>& __n)
{ return __c == __n._M_hash_code; }
static bool
_S_node_equals(const _Hash_node_code_cache<true>& __lhn,
const _Hash_node_code_cache<true>& __rhn)
{ return __lhn._M_hash_code == __rhn._M_hash_code; }
protected:
_Hashtable_base() = default;
_Hashtable_base(const _Hash& __hash, const _Equal& __eq)
: __hash_code_base(__hash), _EqualEBO(__eq)
{ }
bool
_M_key_equals(const _Key& __k,
const _Hash_node_value<_Value,
__hash_cached::value>& __n) const
{
static_assert(__is_invocable<const _Equal&, const _Key&, const _Key&>{},
"key equality predicate must be invocable with two arguments of "
"key type");
return _M_eq()(__k, _ExtractKey{}(__n._M_v()));
}
template<typename _Kt>
bool
_M_key_equals_tr(const _Kt& __k,
const _Hash_node_value<_Value,
__hash_cached::value>& __n) const
{
static_assert(
__is_invocable<const _Equal&, const _Kt&, const _Key&>{},
"key equality predicate must be invocable with two arguments of "
"key type");
return _M_eq()(__k, _ExtractKey{}(__n._M_v()));
}
bool
_M_equals(const _Key& __k, __hash_code __c,
const _Hash_node_value<_Value, __hash_cached::value>& __n) const
{ return _S_equals(__c, __n) && _M_key_equals(__k, __n); }
template<typename _Kt>
bool
_M_equals_tr(const _Kt& __k, __hash_code __c,
const _Hash_node_value<_Value,
__hash_cached::value>& __n) const
{ return _S_equals(__c, __n) && _M_key_equals_tr(__k, __n); }
bool
_M_node_equals(
const _Hash_node_value<_Value, __hash_cached::value>& __lhn,
const _Hash_node_value<_Value, __hash_cached::value>& __rhn) const
{
return _S_node_equals(__lhn, __rhn)
&& _M_key_equals(_ExtractKey{}(__lhn._M_v()), __rhn);
}
void
_M_swap(_Hashtable_base& __x)
{
__hash_code_base::_M_swap(__x);
std::swap(_EqualEBO::_M_get(), __x._EqualEBO::_M_get());
}
const _Equal&
_M_eq() const { return _EqualEBO::_M_cget(); }
};
template<typename _Key, typename _Value, typename _Alloc,
typename _ExtractKey, typename _Equal,
typename _Hash, typename _RangeHash, typename _Unused,
typename _RehashPolicy, typename _Traits,
bool _Unique_keys = _Traits::__unique_keys::value>
struct _Equality;
template<typename _Key, typename _Value, typename _Alloc,
typename _ExtractKey, typename _Equal,
typename _Hash, typename _RangeHash, typename _Unused,
typename _RehashPolicy, typename _Traits>
struct _Equality<_Key, _Value, _Alloc, _ExtractKey, _Equal,
_Hash, _RangeHash, _Unused, _RehashPolicy, _Traits, true>
{
using __hashtable = _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
_Hash, _RangeHash, _Unused,
_RehashPolicy, _Traits>;
bool
_M_equal(const __hashtable&) const;
};
template<typename _Key, typename _Value, typename _Alloc,
typename _ExtractKey, typename _Equal,
typename _Hash, typename _RangeHash, typename _Unused,
typename _RehashPolicy, typename _Traits>
bool
_Equality<_Key, _Value, _Alloc, _ExtractKey, _Equal,
_Hash, _RangeHash, _Unused, _RehashPolicy, _Traits, true>::
_M_equal(const __hashtable& __other) const
{
using __node_type = typename __hashtable::__node_type;
const __hashtable* __this = static_cast<const __hashtable*>(this);
if (__this->size() != __other.size())
return false;
for (auto __itx = __this->begin(); __itx != __this->end(); ++__itx)
{
std::size_t __ybkt = __other._M_bucket_index(*__itx._M_cur);
auto __prev_n = __other._M_buckets[__ybkt];
if (!__prev_n)
return false;
for (__node_type* __n = static_cast<__node_type*>(__prev_n->_M_nxt);;
__n = __n->_M_next())
{
if (__n->_M_v() == *__itx)
break;
if (!__n->_M_nxt
|| __other._M_bucket_index(*__n->_M_next()) != __ybkt)
return false;
}
}
return true;
}
template<typename _Key, typename _Value, typename _Alloc,
typename _ExtractKey, typename _Equal,
typename _Hash, typename _RangeHash, typename _Unused,
typename _RehashPolicy, typename _Traits>
struct _Equality<_Key, _Value, _Alloc, _ExtractKey, _Equal,
_Hash, _RangeHash, _Unused, _RehashPolicy, _Traits, false>
{
using __hashtable = _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal,
_Hash, _RangeHash, _Unused,
_RehashPolicy, _Traits>;
bool
_M_equal(const __hashtable&) const;
};
template<typename _Key, typename _Value, typename _Alloc,
typename _ExtractKey, typename _Equal,
typename _Hash, typename _RangeHash, typename _Unused,
typename _RehashPolicy, typename _Traits>
bool
_Equality<_Key, _Value, _Alloc, _ExtractKey, _Equal,
_Hash, _RangeHash, _Unused, _RehashPolicy, _Traits, false>::
_M_equal(const __hashtable& __other) const
{
using __node_type = typename __hashtable::__node_type;
const __hashtable* __this = static_cast<const __hashtable*>(this);
if (__this->size() != __other.size())
return false;
for (auto __itx = __this->begin(); __itx != __this->end();)
{
std::size_t __x_count = 1;
auto __itx_end = __itx;
for (++__itx_end; __itx_end != __this->end()
&& __this->key_eq()(_ExtractKey{}(*__itx),
_ExtractKey{}(*__itx_end));
++__itx_end)
++__x_count;
std::size_t __ybkt = __other._M_bucket_index(*__itx._M_cur);
auto __y_prev_n = __other._M_buckets[__ybkt];
if (!__y_prev_n)
return false;
__node_type* __y_n = static_cast<__node_type*>(__y_prev_n->_M_nxt);
for (;;)
{
if (__this->key_eq()(_ExtractKey{}(__y_n->_M_v()),
_ExtractKey{}(*__itx)))
break;
auto __y_ref_n = __y_n;
for (__y_n = __y_n->_M_next(); __y_n; __y_n = __y_n->_M_next())
if (!__other._M_node_equals(*__y_ref_n, *__y_n))
break;
if (!__y_n || __other._M_bucket_index(*__y_n) != __ybkt)
return false;
}
typename __hashtable::const_iterator __ity(__y_n);
for (auto __ity_end = __ity; __ity_end != __other.end(); ++__ity_end)
if (--__x_count == 0)
break;
if (__x_count != 0)
return false;
if (!std::is_permutation(__itx, __itx_end, __ity))
return false;
__itx = __itx_end;
}
return true;
}
template<typename _NodeAlloc>
struct _Hashtable_alloc : private _Hashtable_ebo_helper<0, _NodeAlloc>
{
private:
using __ebo_node_alloc = _Hashtable_ebo_helper<0, _NodeAlloc>;
template<typename>
struct __get_value_type;
template<typename _Val, bool _Cache_hash_code>
struct __get_value_type<_Hash_node<_Val, _Cache_hash_code>>
{ using type = _Val; };
public:
using __node_type = typename _NodeAlloc::value_type;
using __node_alloc_type = _NodeAlloc;
using __node_alloc_traits = __gnu_cxx::__alloc_traits<__node_alloc_type>;
using __value_alloc_traits = typename __node_alloc_traits::template
rebind_traits<typename __get_value_type<__node_type>::type>;
using __node_ptr = __node_type*;
using __node_base = _Hash_node_base;
using __node_base_ptr = __node_base*;
using __buckets_alloc_type =
__alloc_rebind<__node_alloc_type, __node_base_ptr>;
using __buckets_alloc_traits = std::allocator_traits<__buckets_alloc_type>;
using __buckets_ptr = __node_base_ptr*;
_Hashtable_alloc() = default;
_Hashtable_alloc(const _Hashtable_alloc&) = default;
_Hashtable_alloc(_Hashtable_alloc&&) = default;
template<typename _Alloc>
_Hashtable_alloc(_Alloc&& __a)
: __ebo_node_alloc(std::forward<_Alloc>(__a))
{ }
__node_alloc_type&
_M_node_allocator()
{ return __ebo_node_alloc::_M_get(); }
const __node_alloc_type&
_M_node_allocator() const
{ return __ebo_node_alloc::_M_cget(); }
template<typename... _Args>
__node_ptr
_M_allocate_node(_Args&&... __args);
void
_M_deallocate_node(__node_ptr __n);
void
_M_deallocate_node_ptr(__node_ptr __n);
void
_M_deallocate_nodes(__node_ptr __n);
__buckets_ptr
_M_allocate_buckets(std::size_t __bkt_count);
void
_M_deallocate_buckets(__buckets_ptr, std::size_t __bkt_count);
};
template<typename _NodeAlloc>
template<typename... _Args>
auto
_Hashtable_alloc<_NodeAlloc>::_M_allocate_node(_Args&&... __args)
-> __node_ptr
{
auto __nptr = __node_alloc_traits::allocate(_M_node_allocator(), 1);
__node_ptr __n = std::__to_address(__nptr);
__try
{
::new ((void*)__n) __node_type;
__node_alloc_traits::construct(_M_node_allocator(),
__n->_M_valptr(),
std::forward<_Args>(__args)...);
return __n;
}
__catch(...)
{
__node_alloc_traits::deallocate(_M_node_allocator(), __nptr, 1);
__throw_exception_again;
}
}
template<typename _NodeAlloc>
void
_Hashtable_alloc<_NodeAlloc>::_M_deallocate_node(__node_ptr __n)
{
__node_alloc_traits::destroy(_M_node_allocator(), __n->_M_valptr());
_M_deallocate_node_ptr(__n);
}
template<typename _NodeAlloc>
void
_Hashtable_alloc<_NodeAlloc>::_M_deallocate_node_ptr(__node_ptr __n)
{
typedef typename __node_alloc_traits::pointer _Ptr;
auto __ptr = std::pointer_traits<_Ptr>::pointer_to(*__n);
__n->~__node_type();
__node_alloc_traits::deallocate(_M_node_allocator(), __ptr, 1);
}
template<typename _NodeAlloc>
void
_Hashtable_alloc<_NodeAlloc>::_M_deallocate_nodes(__node_ptr __n)
{
while (__n)
{
__node_ptr __tmp = __n;
__n = __n->_M_next();
_M_deallocate_node(__tmp);
}
}
template<typename _NodeAlloc>
auto
_Hashtable_alloc<_NodeAlloc>::_M_allocate_buckets(std::size_t __bkt_count)
-> __buckets_ptr
{
__buckets_alloc_type __alloc(_M_node_allocator());
auto __ptr = __buckets_alloc_traits::allocate(__alloc, __bkt_count);
__buckets_ptr __p = std::__to_address(__ptr);
__builtin_memset(__p, 0, __bkt_count * sizeof(__node_base_ptr));
return __p;
}
template<typename _NodeAlloc>
void
_Hashtable_alloc<_NodeAlloc>::
_M_deallocate_buckets(__buckets_ptr __bkts,
std::size_t __bkt_count)
{
typedef typename __buckets_alloc_traits::pointer _Ptr;
auto __ptr = std::pointer_traits<_Ptr>::pointer_to(*__bkts);
__buckets_alloc_type __alloc(_M_node_allocator());
__buckets_alloc_traits::deallocate(__alloc, __ptr, __bkt_count);
}
}
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif