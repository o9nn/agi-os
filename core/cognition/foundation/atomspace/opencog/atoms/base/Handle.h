#ifndef _OPENCOG_HANDLE_H
#define _OPENCOG_HANDLE_H
#include <iostream>
#include <climits>
#include <cstdint>
#include <functional>
#include <limits>
#include <map>
#include <memory>
#include <string>
#include <sstream>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <opencog/util/empty_string.h>
#include <opencog/atoms/atom_types/types.h>
namespace opencog
{
typedef size_t UUID;
typedef uint64_t ContentHash;
class Atom;
typedef std::shared_ptr<Atom> AtomPtr;
class Handle : public AtomPtr
{
friend class Atom;
friend class content_based_atom_ptr_less;
friend class content_based_handle_less;
private:
static bool atoms_less(const Atom*, const Atom*);
static bool content_based_atoms_less(const Atom*, const Atom*);
static const AtomPtr NULL_POINTER;
public:
static const ContentHash INVALID_HASH = std::numeric_limits<size_t>::max();
static const Handle UNDEFINED;
explicit Handle(const AtomPtr& atom) : AtomPtr(atom) {}
explicit Handle(AtomPtr&& atom) : AtomPtr(atom) {}
explicit Handle() {}
~Handle() {}
ContentHash value(void) const;
inline Handle& operator=(const AtomPtr& a) {
this->AtomPtr::operator=(a);
return *this;
}
inline Handle& operator=(AtomPtr&& a) {
this->AtomPtr::operator=(a);
return *this;
}
inline Atom* atom_ptr() {
return get();
}
inline const Atom* const_atom_ptr() const {
return get();
}
explicit inline operator bool() const noexcept {
if (get()) return true;
return false;
}
inline bool operator==(std::nullptr_t) const noexcept {
return get() == nullptr;
}
inline bool operator!=(std::nullptr_t) const noexcept {
return get() != nullptr;
}
inline bool operator==(const Atom* ap) const noexcept {
return get() == ap;
}
inline bool operator!=(const Atom* ap) const noexcept {
return get() != ap;
}
inline bool operator==(const Handle& h) const noexcept {
return get() == h.get();
}
inline bool operator!=(const Handle& h) const noexcept {
return get() != h.get();
}
bool operator< (const Handle& h) const noexcept;
static int compare(const Handle&, const Handle&);
};
static inline bool operator== (std::nullptr_t, const Handle& rhs) noexcept
{ return rhs == (Atom*) nullptr; }
static inline bool operator!= (std::nullptr_t, const Handle& rhs) noexcept
{ return rhs != (Atom*) nullptr; }
bool content_eq(const opencog::Handle& lh,
const opencog::Handle& rh) noexcept;
std::size_t hash_value(Handle const&);
typedef std::pair<Handle, Handle> HandlePair;
typedef std::vector<Handle> HandleSeq;
typedef std::set<HandleSeq> HandleSeqSet;
typedef std::vector<HandleSeq> HandleSeqSeq;
typedef std::set<Handle> HandleSet;
typedef std::set<HandleSet> HandleSetSet;
typedef std::vector<HandleSet> HandleSetSeq;
typedef std::unordered_set<Handle> UnorderedHandleSet;
typedef std::map<Handle, Handle> HandleMap;
typedef std::unordered_map<Handle, Handle> UnorderedHandleMap;
typedef std::map<Handle, HandleSet> HandleMultimap;
typedef std::map<Handle, HandleSeq> HandleSeqMap;
typedef std::vector<HandleMap> HandleMapSeq;
typedef std::vector<HandleMapSeq> HandleMapSeqSeq;
typedef std::set<HandleMap> HandleMapSet;
typedef std::vector<HandlePair> HandlePairSeq;
typedef HandleMap GroundingMap;
typedef std::vector<GroundingMap> GroundingMapSeq;
typedef std::vector<GroundingMapSeq> GroundingMapSeqSeq;
bool content_eq(const opencog::HandleSeq& lhs,
const opencog::HandleSeq& rhs);
bool content_eq(const opencog::HandleSet& lhs,
const opencog::HandleSet& rhs);
bool content_eq(const opencog::HandleSetSeq& lhs,
const opencog::HandleSetSeq& rhs);
bool content_contains(const opencog::HandleSeq& hs, const opencog::Handle& h);
struct content_based_atom_ptr_less
{
bool operator()(const Atom* al, const Atom* ar) const
{
return Handle::content_based_atoms_less(al, ar);
}
};
struct content_based_handle_less
{
bool operator()(const Handle& hl, const Handle& hr) const
{
return Handle::content_based_atoms_less(hl.const_atom_ptr(),
hr.const_atom_ptr());
}
};
struct handle_seq_less
{
bool operator()(const HandleSeq& hsl, const HandleSeq& hsr) const
{
size_t sl = hsl.size();
size_t sr = hsr.size();
if (sl != sr) return sl < sr;
for (size_t i=0; i<sl; i++)
{
if (hsl[i] != hsl[i]) return hsl[i].operator<(hsr[i]);
}
return false;
}
};
struct handle_seq_ptr_less
{
bool operator()(const HandleSeq* hsl, const HandleSeq* hsr) const
{
return handle_seq_less().operator()(*hsl, *hsr);
}
};
static inline std::string operator+ (const char *lhs, Handle h)
{
std::string rhs = lhs;
char buff[25];
snprintf(buff, 24, "%llu)", (unsigned long long) h.value());
return rhs + buff;
}
static inline std::string operator+ (const std::string &lhs, Handle h)
{
char buff[25];
snprintf(buff, 24, "%llu)", (unsigned long long) h.value());
return lhs + buff;
}
#define OC_TO_STRING_INDENT "  "
std::string oc_to_string(const Handle& h,
const std::string& indent=empty_string);
std::string oc_to_string(const HandlePair& hp,
const std::string& indent=empty_string);
std::string oc_to_string(const HandleSeq& hs,
const std::string& indent=empty_string);
std::string oc_to_string(const HandleSeqSeq& hss,
const std::string& indent=empty_string);
std::string oc_to_string(const HandleSet& ohs,
const std::string& indent=empty_string);
std::string oc_to_string(const HandleSetSet& ohss,
const std::string& indent=empty_string);
std::string oc_to_string(const HandleSetSeq& ohss,
const std::string& indent=empty_string);
std::string oc_to_string(const UnorderedHandleSet& uhs,
const std::string& indent=empty_string);
std::string oc_to_string(const HandleMap& hm,
const std::string& indent=empty_string);
std::string oc_to_string(const HandleMap::value_type& hmv,
const std::string& indent=empty_string);
std::string oc_to_string(const UnorderedHandleMap& hm,
const std::string& indent=empty_string);
std::string oc_to_string(const HandleMultimap& hmm,
const std::string& indent=empty_string);
std::string oc_to_string(const HandleSeqMap& hsm,
const std::string& indent=empty_string);
std::string oc_to_string(const HandleMapSeq& hms,
const std::string& indent=empty_string);
std::string oc_to_string(const HandleMapSeqSeq& hmss,
const std::string& indent=empty_string);
std::string oc_to_string(const HandleMapSet& hms,
const std::string& indent=empty_string);
std::string oc_to_string(const HandlePairSeq& hps,
const std::string& indent=empty_string);
std::string oc_to_string(Type type,
const std::string& indent=empty_string);
std::string oc_to_string(const TypeSet& types,
const std::string& indent=empty_string);
std::string oc_to_string(const AtomPtr& aptr,
const std::string& indent=empty_string);
template<typename T>
static inline
typename std::enable_if< std::is_base_of<Atom, T>::value, std::shared_ptr<T> >::type
CastFromHandle(const Handle& handle)
{
return std::dynamic_pointer_cast<T>(handle);
}
template<typename T>
static inline
typename std::enable_if< std::is_base_of<Atom, T>::value, std::shared_ptr<T> >::type
CastFromAtomPtr(const AtomPtr& atom)
{
return std::dynamic_pointer_cast<T>(atom);
}
}
namespace std {
ostream& operator<<(ostream&, const opencog::HandleMap&);
ostream& operator<<(ostream&, const opencog::HandleSeq&);
ostream& operator<<(ostream&, const opencog::HandleSet&);
ostream& operator<<(ostream&, const opencog::UnorderedHandleSet&);
ostream& operator<<(ostream&, const opencog::UnorderedHandleMap&);
template<>
struct hash<opencog::Handle>
{
typedef std::size_t result_type;
typedef opencog::Handle argument_type;
std::size_t operator()(const opencog::Handle& h) const noexcept
{ return hash_value(h); }
#if HAVE_SPARSEHASH
std::size_t operator()(void) const noexcept
{ return 0; }
#endif
};
template<>
struct equal_to<opencog::Handle>
{
typedef bool result_type;
typedef opencog::Handle first_argument;
typedef opencog::Handle second_argument;
bool
operator()(const opencog::Handle& lh,
const opencog::Handle& rh) const noexcept
{
if (lh == rh) return true;
if (nullptr == lh or nullptr == rh) return false;
return opencog::content_eq(lh, rh);
}
};
template<>
struct hash<opencog::HandlePair>
{
typedef std::size_t result_type;
typedef opencog::HandlePair argument_type;
std::size_t
operator()(const opencog::HandlePair& hp) const noexcept
{ return hash_value(hp.first) + hash_value(hp.second); }
};
template<>
struct equal_to<opencog::HandlePair>
{
typedef bool result_type;
typedef opencog::HandlePair first_argument;
typedef opencog::HandlePair second_argument;
bool
operator()(const opencog::HandlePair& lhp,
const opencog::HandlePair& rhp) const noexcept
{
if (lhp == rhp) return true;
std::equal_to<opencog::Handle> eq;
return eq.operator()(lhp.first, rhp.first) and
eq.operator()(lhp.second, rhp.second);
}
};
template<>
struct hash<opencog::HandleSeq>
{
typedef std::size_t result_type;
typedef opencog::HandleSeq argument_type;
std::size_t
operator()(const opencog::HandleSeq& hseq) const noexcept
{
std::size_t hsh = 0;
for (const opencog::Handle& h : hseq) hsh += hash_value(h);
return hsh;
}
};
template<>
struct equal_to<opencog::HandleSeq>
{
typedef bool result_type;
typedef opencog::HandleSeq first_argument;
typedef opencog::HandleSeq second_argument;
bool
operator()(const opencog::HandleSeq& lhs,
const opencog::HandleSeq& rhs) const noexcept
{
if (lhs == rhs) return true;
size_t len = lhs.size();
if (rhs.size() != len) return false;
std::equal_to<opencog::Handle> eq;
for (size_t i=0; i<len; i++)
{
if (not eq.operator()(lhs[i], rhs[i])) return false;
}
return true;
}
};
}
#endif