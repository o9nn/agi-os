#ifndef _OPENCOG_LRU_CACHE_H
#define _OPENCOG_LRU_CACHE_H
#include <atomic>
#include <limits>
#include <list>
#include <shared_mutex>
#include <opencog/util/exceptions.h>
#include <opencog/asmoses/utils/hashing.h>
#include <opencog/util/Logger.h>
#include <opencog/util/oc_assert.h>
#include <opencog/util/platform.h>
namespace opencog {
struct inf_cache_base
{
typedef size_t size_type;
inf_cache_base(const std::string& name) :
_misses(0), _hits(0), _cache_name(name)
{
logger().info("Cache %s", _cache_name.c_str());
}
~inf_cache_base()
{
logger().info("Cache %s hits=%u misses=%u",
_cache_name.c_str(), get_hits(), get_misses());
}
size_type get_misses() const { return _misses.load(); }
size_type get_hits() const { return _hits.load(); }
protected:
mutable std::atomic<size_type> _misses;
mutable std::atomic<size_type> _hits;
std::string _cache_name;
};
struct cache_base : public inf_cache_base
{
cache_base(size_type n, const std::string& name)
: inf_cache_base(name), _n(n) {}
~cache_base() {}
size_type max_size() const { return _n; }
protected:
size_type _n;
};
template<typename F,
typename Hash=std::hash<typename F::argument_type>,
typename Equals=std::equal_to<typename F::argument_type> >
struct lru_cache : public F, public cache_base
{
typedef typename F::argument_type argument_type;
typedef typename F::result_type result_type;
typedef typename std::list<argument_type> list;
typedef typename list::iterator list_iter;
typedef std::unordered_map<list_iter,result_type,
deref_hash<list_iter,Hash>,
deref_equals<list_iter,Equals> > map;
typedef typename map::iterator map_iter;
lru_cache(size_type n, const F& f=F(), const std::string name = "lru_cache")
: F(f), cache_base(n, name), _fu(f), _map(n+1) {}
inline bool full() const { return _map.size()==_n; }
inline bool empty() const { return _map.empty(); }
void remove(const argument_type& x) {
_lru.push_front(x);
map_iter it=_map.find(_lru.begin());
if (it != _map.end()) {
_lru.erase(it->first);
_map.erase(it);
}
_lru.pop_front();
}
result_type operator()(const argument_type& x) const {
if (empty()) {
if (full())
return if_f(x);
_lru.push_front(x);
map_iter it = _map.insert(make_pair(_lru.begin(), ifx_f(x))).first;
return it->second;
}
_lru.push_front(x);
map_iter it=_map.find(_lru.begin());
if (it!=_map.end()) {
_lru.pop_front();
_lru.splice(_lru.begin(), _lru,it->first);
++_hits;
return it->second;
}
it = _map.insert(make_pair(_lru.begin(), ifx_f(x))).first;
if (_map.size() > _n) {
_map.erase(--_lru.end());
_lru.pop_back();
}
OC_ASSERT(_map.size() <= _n,
"lru_cache - _map size greater than _n (%d).", _n);
OC_ASSERT(_lru.size() == _map.size(),
"lru_cache - _lru size different from _map size.");
return it->second;
}
void clear() {
_map.clear();
_lru.clear();
}
void resize(unsigned n) {
_n = n;
while(_map.size() > _n) {
_lru.begin();
map_iter it = _map.find(_lru.begin());
OC_ASSERT(it != _map.end(),
"Element in _lru has no corresponding iterator in _map");
_lru.erase(it->first);
_map.erase(it);
}
OC_ASSERT(_lru.size() == _map.size(),
"lru_cache - _lru size different from _map size.");
}
protected:
const F& _fu;
mutable map _map;
mutable list _lru;
inline result_type _f(const argument_type& x) const {
return _fu(x);
}
inline result_type if_f(const argument_type& x) const {
++_misses;
return _f(x);
}
inline result_type ifx_f(const argument_type& x) const {
++_misses;
return x_f(x);
}
inline result_type x_f(const argument_type& x) const {
try {
return _f(x);
} catch(...) {
_lru.pop_front();
throw;
}
}
};
template<typename F,
typename Hash=std::hash<typename F::argument_type>,
typename Equals=std::equal_to<typename F::argument_type> >
struct lru_cache_threaded : public lru_cache<F, Hash, Equals>
{
private:
typedef lru_cache<F, Hash, Equals> super;
typedef std::shared_mutex cache_mutex;
typedef std::shared_lock<cache_mutex> shared_lock;
typedef std::unique_lock<cache_mutex> unique_lock;
public:
typedef typename F::argument_type argument_type;
typedef typename F::result_type result_type;
typedef typename super::list list;
typedef typename list::iterator list_iter;
typedef typename super::map map;
typedef typename map::iterator map_iter;
typedef typename map::size_type size_type;
lru_cache_threaded(size_type n, const F& f=F(),
const std::string name = "lru_cache_threaded")
: super(n, f, name) {}
inline bool full() const {
shared_lock lock(mutex);
return super::full();
}
inline bool empty() const {
shared_lock lock(mutex);
return super::empty();
}
inline unsigned max_size() const {
shared_lock lock(mutex);
return super::max_size();
}
void remove(const argument_type& x) {
unique_lock lock(mutex);
super::remove(x);
}
result_type operator()(const argument_type& x) const {
unique_lock lock(mutex);
return super::operator()(x);
}
void clear() {
unique_lock lock(mutex);
super::clear();
}
protected:
mutable cache_mutex mutex;
inline void lru_push_front(const argument_type& x) const {
unique_lock lock(mutex);
super::_lru.push_front(x);
}
inline result_type if_f(const argument_type& x) const {
++super::misses;
return super::_f(x);
}
inline result_type ifx_f(const argument_type& x) const {
++super::misses;
return xs_f(x);
}
inline result_type x_f(const argument_type& x) const {
try {
return _f(x);
} catch(...) {
unique_lock lock(mutex);
super::_lru.pop_front();
throw;
}
}
};
template<typename F,
typename Hash=std::hash<typename F::argument_type>,
typename Equals=std::equal_to<typename F::argument_type> >
struct prr_cache : public F, public cache_base
{
typedef typename F::argument_type argument_type;
typedef typename F::result_type result_type;
typedef std::unordered_map<argument_type, result_type, Hash, Equals> map;
typedef typename map::iterator map_iter;
prr_cache(size_type n, const F& f=F(), const std::string name = "prr_cache")
: F(f), cache_base(n, name), _fu(f), _map(n+1) {}
bool full() const { return _map.size() == _n; }
bool empty() const { return _map.empty(); }
result_type operator()(const argument_type& x) const
{
map_iter it = _map.find(x);
if (it != _map.end()) {
++_hits;
return it->second;
}
else {
result_type res = if_f(x);
if (full()) {
_map.erase(_map.begin());
}
_map[x] = res;
return res;
}
}
void resize(unsigned n)
{
_n = n;
while (_map.size() > _n)
_map.erase(_map.begin());
}
void clear()
{
_map.clear();
}
protected:
const F& _fu;
mutable map _map;
inline result_type _f(const argument_type& x) const
{
return _fu(x);
}
inline result_type if_f(const argument_type& x) const
{
++_misses;
return _fu(x);
}
};
template<typename F,
typename Hash=std::hash<typename F::argument_type>,
typename Equals=std::equal_to<typename F::argument_type> >
struct prr_cache_threaded : public prr_cache<F, Hash, Equals>
{
private:
typedef prr_cache<F, Hash, Equals> super;
typedef std::shared_mutex cache_mutex;
typedef std::shared_lock<cache_mutex> shared_lock;
typedef std::unique_lock<cache_mutex> unique_lock;
public:
typedef typename F::argument_type argument_type;
typedef typename F::result_type result_type;
typedef typename super::map map;
typedef typename map::iterator map_iter;
typedef typename map::size_type size_type;
prr_cache_threaded(size_type n, const F& f=F(),
const std::string name = "prr_cache_threaded")
: super(n, f, name) {}
bool full() const
{
shared_lock lock(mutex);
return super::full();
}
bool empty() const
{
shared_lock lock(mutex);
return super::empty();
}
unsigned max_size() const
{
shared_lock lock(mutex);
return super::max_size();
}
result_type operator()(const argument_type& x) const
{
{
shared_lock lock(mutex);
map_iter it = super::_map.find(x);
if (it != super::_map.end()) {
++super::_hits;
return it->second;
}
}
result_type res = incmis_f(x);
if (full()) {
unique_lock lock(mutex);
OC_ASSERT (0 < super::_n, "zero-sized cache is unusable!");
super::_map.erase(super::_map.begin());
super::_map[x] = res;
return res;
}
unique_lock lock(mutex);
super::_map[x] = res;
return res;
}
void resize(unsigned n)
{
unique_lock lock(mutex);
super::resize(n);
}
void clear()
{
unique_lock lock(mutex);
super::clear();
}
protected:
mutable cache_mutex mutex;
inline result_type incmis_f(const argument_type& x) const
{
++super::_misses;
return super::_f(x);
}
};
template<typename F,
typename Hash=std::hash<typename F::argument_type>,
typename Equals=std::equal_to<typename F::argument_type> >
struct inf_cache : public F, public inf_cache_base {
typedef typename F::argument_type argument_type;
typedef typename F::result_type result_type;
typedef std::unordered_map<argument_type, result_type, Hash, Equals> map;
typedef typename map::iterator map_iter;
typedef std::shared_mutex cache_mutex;
typedef std::shared_lock<cache_mutex> shared_lock;
typedef std::unique_lock<cache_mutex> unique_lock;
inf_cache(const F& f=F(), const std::string name = "inf_cache")
: F(f), inf_cache_base(name) {}
result_type operator()(const argument_type& x) const {
{
shared_lock lock(_mutex);
auto it = _map.find(x);
if (it != _map.end()) {
++_hits;
return it->second;
}
}
++_misses;
result_type y = F::operator()(x);
{
unique_lock lock(_mutex);
return _map[x] = y;
}
}
protected:
mutable cache_mutex _mutex;
mutable map _map;
};
template<typename Cache>
struct adaptive_cache {
typedef typename Cache::result_type result_type;
typedef typename Cache::argument_type argument_type;
adaptive_cache(Cache& cache,
unsigned ncycles = 1000,
float llimit = 0.75, float lfact = 2,
float ulimit = 0.90, float ufrac = 2)
: _cache(cache), _counter(0), _ncycles(ncycles),
_llimit(llimit), _lfact(lfact),
_ulimit(ulimit), _ufrac(ufrac) {}
result_type operator()(const argument_type& x) const {
if(_counter++ % _ncycles == 0) {
float tram = getTotalRAM();
float fram = getFreeRAM();
float free_mem_ratio = 1 - fram/tram;
if(free_mem_ratio < _llimit && _cache.full()) {
try {
_cache.resize(static_cast<unsigned>(_cache.max_size()*_lfact));
} catch(const std::exception&) {
_cache.resize(std::numeric_limits<unsigned>::max());
}
}
else if (free_mem_ratio > _ulimit) {
_cache.resize(std::max(1U, (unsigned)(_cache.max_size()/_ufrac)));
}
}
return _cache(x);
}
unsigned get_misses() const { return _cache.get_misses(); }
unsigned get_hits() const { return _cache.get_hits(); }
private:
Cache& _cache;
mutable unsigned _counter;
unsigned _ncycles;
float _llimit;
float _lfact;
float _ulimit;
float _ufrac;
};
template<typename ARG, typename RESULT>
struct lru_cache_arg_result {
typedef ARG argument_type;
typedef RESULT result_type;
typedef typename std::hash<argument_type> Hash;
typedef typename std::equal_to<argument_type> Equals;
typedef typename std::list<argument_type> list;
typedef typename list::iterator list_iter;
typedef std::unordered_map<list_iter,result_type,
deref_hash<list_iter,Hash>,
deref_equals<list_iter,Equals> > map;
typedef typename map::iterator map_iter;
typedef typename map::size_type size_type;
lru_cache_arg_result(size_type n) : _n(n), _map(n+1) { }
inline bool full() const { return _map.size()==_n; }
inline bool empty() const { return _map.empty(); }
map_iter find(const argument_type& x) {
_lru.push_front(x);
map_iter it=_map.find(_lru.begin());
_lru.pop_front();
if (it!=_map.end())
_lru.splice(_lru.begin(),_lru,it->first);
return it;
}
inline bool is_cache_failure(map_iter mi) {
return mi == _map.end();
}
void insert_new(const argument_type& x, const result_type& y) {
_lru.push_front(x);
_map.insert(make_pair(_lru.begin(),y)).first;
if(full()) {
_map.erase(--_lru.end());
_lru.pop_back();
}
}
void clear() {
_map.clear();
_lru.clear();
}
protected:
size_type _n;
mutable map _map;
mutable list _lru;
};
}
#endif