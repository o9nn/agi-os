#ifndef _OPENCOG_ATOM_H
#define _OPENCOG_ATOM_H
#include <atomic>
#include <functional>
#include <memory>
#include <shared_mutex>
#include <string>
#include <unordered_set>
#if HAVE_SPARSEHASH
#include <sparsehash/sparse_hash_set>
#include <sparsehash/sparse_hash_map>
#define USE_HASHABLE_WEAK_PTR 1
#endif
#if HAVE_FOLLY
#include <folly/container/F14Set.h>
#define USE_HASHABLE_WEAK_PTR 1
#endif
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
namespace opencog
{
#if USE_HASHABLE_WEAK_PTR
template<class T>
struct hashable_weak_ptr : public std::weak_ptr<T>
{
hashable_weak_ptr(std::shared_ptr<T>const& sp) :
std::weak_ptr<T>(sp)
{
if (!sp) return;
_hash = std::hash<T*>{}(sp.get());
}
#if HAVE_SPARSEHASH
static std::weak_ptr<T> _dummy;
hashable_weak_ptr(void) :
std::weak_ptr<T>(_dummy)
{
_hash = 0;
}
#endif
std::size_t get_hash() const noexcept { return _hash; }
friend bool operator<(hashable_weak_ptr const& lhs, hashable_weak_ptr const& rhs)
{
return lhs.owner_before(rhs);
}
friend bool operator!=(hashable_weak_ptr const& lhs, hashable_weak_ptr const& rhs)
{
return lhs<rhs or rhs<lhs;
}
friend bool operator==(hashable_weak_ptr const& lhs, hashable_weak_ptr const& rhs)
{
return not (lhs != rhs);
}
private:
std::size_t _hash = 0;
};
typedef hashable_weak_ptr<Atom> WinkPtr;
#else
#if USE_BARE_BACKPOINTER
typedef const Atom* WinkPtr;
#else
typedef std::weak_ptr<Atom> WinkPtr;
#endif
#endif
}
#if USE_BARE_BACKPOINTER
#define WEAKLY_DO(HA,WP,STMT) { Handle HA(WP->get_handle()); STMT; }
#else
#define WEAKLY_DO(HA,WP,STMT) { Handle HA(WP.lock()); if (HA) { STMT; }}
#endif
namespace std
{
#if USE_HASHABLE_WEAK_PTR
template<class T> struct owner_less<opencog::hashable_weak_ptr<T>>
{
bool operator()(const opencog::hashable_weak_ptr<T>& lhs,
const opencog::hashable_weak_ptr<T>& rhs) const noexcept
{
return lhs.owner_before(rhs);
}
};
template<class T> struct hash<opencog::hashable_weak_ptr<T>>
{
std::size_t operator()(const opencog::hashable_weak_ptr<T>& w) const noexcept
{
return w.get_hash();
}
};
#else
#if USE_BARE_BACKPOINTER
template <> struct owner_less<const opencog::Atom*>
{
bool operator()(const opencog::Atom* const& lhs,
const opencog::Atom* const& rhs) const noexcept
{
return lhs < rhs;
}
};
#endif
#endif
}
namespace opencog
{
class AtomSpace;
typedef std::size_t Arity;
typedef HandleSeq IncomingSet;
#if HAVE_SPARSEHASH
#define USE_SPARSE_INCOMING 1
#endif
#if USE_SPARSE_INCOMING
typedef google::sparse_hash_set<WinkPtr> WincomingSet;
#endif
#if USE_FOLLY
typedef folly::F14ValueSet<WinkPtr> WincomingSet;
#endif
#if not (USE_SPARSE_INCOMING || USE_FOLLY)
typedef std::set<WinkPtr, std::owner_less<WinkPtr> > WincomingSet;
#endif
typedef std::map<Type, WincomingSet> InSetMap;
#if USE_SPARSE_KVP
typedef google::sparse_hash_map<Handle, ValuePtr> KVPMap;
#error "USE_SPARSE_KVP is enabled! It works, but you probably did this by accident. If you meant to do this, edit the header file and try again."
#else
typedef std::map<const Handle, ValuePtr> KVPMap;
#endif
class Atom
: public Value
{
friend class AtomSpace;
friend class TypeIndex;
friend class Link;
friend class Frame;
friend class StateLink;
friend class StorageNode;
protected:
#define USE_MUTEX_POOL 1
#if USE_MUTEX_POOL
struct MutexPool
{
static constexpr size_t POOL_SIZE = 64;
mutable std::shared_mutex mutexes[POOL_SIZE];
inline std::shared_mutex& get_mutex(ContentHash hsh) {
return mutexes[hsh % POOL_SIZE];
}
};
static MutexPool _mutex_pool;
#define _MTX (_mutex_pool.get_mutex(_content_hash))
#define INCOMING_SHARED_LOCK std::shared_lock<std::shared_mutex> lck(_MTX);
#define INCOMING_UNIQUE_LOCK std::unique_lock<std::shared_mutex> lck(_MTX);
#define KVP_UNIQUE_LOCK std::unique_lock<std::shared_mutex> lck(_MTX);
#define KVP_SHARED_LOCK std::shared_lock<std::shared_mutex> lck(_MTX);
#else
#define INCOMING_SHARED_LOCK std::shared_lock<std::shared_mutex> lck(_mtx);
#define INCOMING_UNIQUE_LOCK std::unique_lock<std::shared_mutex> lck(_mtx);
#define KVP_UNIQUE_LOCK std::unique_lock<std::shared_mutex> lck(_mtx);
#define KVP_SHARED_LOCK std::shared_lock<std::shared_mutex> lck(_mtx);
#endif
enum AtomFlags : uint8_t {
ABSENT_FLAG     = 0x01,
MARKED_FLAG     = 0x02,
CHECKED_FLAG    = 0x04,
USE_ISET_FLAG   = 0x08
};
mutable std::atomic<uint8_t> _flags;
mutable ContentHash _content_hash;
virtual void setAtomSpace(AtomSpace *);
AtomSpace *_atom_space;
mutable KVPMap _values;
#if not defined(USE_MUTEX_POOL) or (0 == USE_MUTEX_POOL)
mutable std::shared_mutex _mtx;
#endif
Atom(Type t)
: Value(t),
_flags(0),
_content_hash(Handle::INVALID_HASH),
_atom_space(nullptr)
{
#if USE_SPARSE_KVP
_values.set_deleted_key(Handle());
#endif
}
Atom& operator=(const Atom& other)
{ return *this; }
Atom& operator=(Atom&& other)
{ return *this; }
#ifndef USE_INCOME_INDEX
private:
struct InSet
{
InSetMap _iset;
};
InSet _local_incoming_set;
protected:
inline bool have_inset_map(void) const { return true; }
inline InSetMap& get_inset_map(void) { return _local_incoming_set._iset; }
inline const InSetMap& get_inset_map_const(void) const { return _local_incoming_set._iset; }
inline void drop_inset_map(void) {}
#else
bool have_inset_map(void) const;
InSetMap& get_inset_map(void);
const InSetMap& get_inset_map_const(void) const;
void drop_inset_map(void);
#endif
void keep_incoming_set();
void drop_incoming_set();
void insert_atom(const Handle&);
void remove_atom(const Handle&);
void swap_atom(const Handle&, const Handle&);
virtual void install();
virtual void remove();
virtual ContentHash compute_hash() const = 0;
private:
bool isMarkedForRemoval() const;
bool markForRemoval();
bool unsetRemovalFlag();
bool isChecked() const;
bool setChecked();
bool setUnchecked();
bool isAbsent() const;
bool setAbsent();
bool setPresent();
void getLocalInc(const AtomSpace*, HandleSet&, Type) const;
void getCoveredInc(const AtomSpace*, HandleSet&, Type) const;
public:
virtual ~Atom();
virtual bool is_atom() const { return true; }
AtomSpace* getAtomSpace() const { return _atom_space; }
inline ContentHash get_hash() const {
if (Handle::INVALID_HASH != _content_hash)
return _content_hash;
_content_hash = compute_hash();
return _content_hash;
}
virtual const std::string& get_name() const {
throw RuntimeException(TRACE_INFO, "Not a node!");
}
virtual Arity get_arity() const { return size(); }
virtual const HandleSeq& getOutgoingSet() const {
throw RuntimeException(TRACE_INFO, "Not a link!");
}
virtual Handle getOutgoingAtom(Arity) const {
throw RuntimeException(TRACE_INFO, "Not a link!");
}
virtual bool bevaluate(AtomSpace*, bool silent=false) {
throw RuntimeException(TRACE_INFO, "Not evaluatable!");
}
virtual TruthValuePtr evaluate(AtomSpace* as, bool silent=false) {
if (bevaluate(as, silent))
return TruthValue::TRUE_TV();
return TruthValue::FALSE_TV();
}
virtual bool is_evaluatable() const { return false; }
virtual ValuePtr execute(AtomSpace*, bool silent=false) {
throw RuntimeException(TRACE_INFO,
"Not executable! %s", to_string().c_str());
}
virtual ValuePtr execute(void) { return execute(_atom_space, false); }
virtual bool is_executable() const { return false; }
inline Handle get_handle() const {
return Handle(std::dynamic_pointer_cast<Atom>(
const_cast<Atom*>(this)->shared_from_this()));
}
virtual void setValue(const Handle& key, const ValuePtr& value);
virtual ValuePtr getValue(const Handle& key) const;
ValuePtr incrementCount(const Handle& key, const std::vector<double>&);
ValuePtr incrementCount(const Handle& key, size_t idx, double);
HandleSet getKeys() const;
void copyValues(const Handle&);
bool haveValues() const {
return not _values.empty();
}
std::string valuesToString() const;
void clearValues();
TruthValuePtr getTruthValue() const;
void setTruthValue(const TruthValuePtr&);
TruthValuePtr incrementCountTV(double);
bool isIncomingSetEmpty(const AtomSpace* = nullptr) const;
size_t getIncomingSetSize(const AtomSpace* = nullptr) const;
IncomingSet getIncomingSet(const AtomSpace* = nullptr) const;
IncomingSet getIncomingSetByType(Type, const AtomSpace* = nullptr) const;
size_t getIncomingSetSizeByType(Type, const AtomSpace* = nullptr) const;
virtual std::string to_string(const std::string& indent) const = 0;
virtual std::string to_short_string(const std::string& indent) const = 0;
virtual std::string id_to_string() const;
std::string to_string() const { return to_string(""); }
std::string to_short_string() const { return to_short_string(""); }
virtual bool operator==(const Atom&) const = 0;
bool operator!=(const Atom& other) const
{ return not operator==(other); }
virtual bool operator==(const Value& other) const
{
if (_type != other.get_type()) return false;
return operator==(dynamic_cast<const Atom&>(other));
}
virtual bool operator<(const Atom&) const = 0;
};
#define ATOM_PTR_DECL(CNAME)                                \
typedef std::shared_ptr<CNAME> CNAME##Ptr;              \
static inline CNAME##Ptr CNAME##Cast(const Handle& h)   \
{ return std::dynamic_pointer_cast<CNAME>(h); }     \
static inline CNAME##Ptr CNAME##Cast(const ValuePtr& v) \
{ return std::dynamic_pointer_cast<CNAME>(v); }
#define CREATE_DECL(CNAME)  std::make_shared<CNAME>
static inline AtomPtr AtomCast(const ValuePtr& pa)
{ return std::dynamic_pointer_cast<Atom>(pa); }
static inline AtomPtr AtomCast(const Handle& h)
{ return AtomPtr(h); }
static inline Handle HandleCast(const ValuePtr& pa)
{ return Handle(AtomCast(pa)); }
static inline ValuePtr ValueCast(const Handle& h)
{ return std::dynamic_pointer_cast<Value>(h); }
const Handle& truth_key(void);
std::string oc_to_string(const Atom& atom,
const std::string& indent=empty_string);
}
namespace std {
template<>
struct less<opencog::AtomPtr>
{
bool operator()(const opencog::AtomPtr& ata, const opencog::AtomPtr& atb) const
{
return ata->operator<(*atb);
}
};
ostream& operator<<(ostream&, const opencog::IncomingSet&);
}
#endif