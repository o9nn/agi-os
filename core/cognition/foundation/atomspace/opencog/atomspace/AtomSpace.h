#ifndef _OPENCOG_ATOMSPACE_H
#define _OPENCOG_ATOMSPACE_H
#include <opencog/util/async_method_caller.h>
#include <opencog/util/exceptions.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
#include <opencog/atomspace/Frame.h>
#include <opencog/atomspace/TypeIndex.h>
class AtomTableUTest;
namespace opencog
{
class AtomSpace;
typedef std::shared_ptr<AtomSpace> AtomSpacePtr;
class AtomSpace : public Frame
{
friend class StorageNode;
static const bool EMIT_DIAGNOSTICS = true;
static const bool DONT_EMIT_DIAGNOSTICS = false;
static const bool CHECK_VALUES = true;
static const bool DONT_CHECK_VALUES = false;
AtomSpace& operator=(const AtomSpace&) = delete;
AtomSpace(const AtomSpace&) = delete;
TypeIndex typeIndex;
#if USE_INCOME_INDEX
IncomeIndex incomeIndex;
public:
bool have_inset_map(const Handle& h) const {
return incomeIndex.haveInset(h); }
InSetMap& get_inset_map(const Handle& h) {
return incomeIndex.getInset(h); }
void drop_inset_map(const Handle& h) {
return incomeIndex.removeInset(h); }
private:
#endif
UUID _uuid;
bool _read_only;
bool _copy_on_write;
bool _transient;
std::vector<AtomSpacePtr> _environ;
NameServer& _nameserver;
int addedTypeConnection;
void typeAdded(Type);
void init();
void clear_all_atoms();
Handle add(const Handle&, bool force=false,
bool recurse=false, bool absent = false);
Handle check(const Handle&, bool force=false);
Handle lookupHide(const Handle&, bool hide=false) const;
virtual ContentHash compute_hash() const;
void shadow_by_type(UnorderedHandleSet&,
Type type,
bool subclass,
bool parent,
const AtomSpace*) const;
void get_absent_atoms(HandleSeq&) const;
void get_atoms_in_frame(HandleSeq&) const;
public:
AtomSpace(AtomSpace* base=nullptr, bool transient=false);
AtomSpace(AtomSpacePtr&);
AtomSpace(const HandleSeq&);
~AtomSpace();
bool is_node(void) const { return true; }
bool is_link(void) const { return true; }
UUID get_uuid(void) const { return _uuid; }
void ready_transient(AtomSpace* parent);
void clear_transient();
void set_read_only(void);
void set_read_write(void);
bool get_read_only(void) { return _read_only; }
void set_copy_on_write(void) { _copy_on_write = true; }
void clear_copy_on_write(void) { _copy_on_write = false; }
bool get_copy_on_write(void) const { return _copy_on_write; }
int depth(const Handle& atom) const;
int depth(const AtomSpace*) const;
bool in_environ(const Handle&) const;
bool in_environ(const AtomSpace*) const;
virtual const std::string& get_name() const;
virtual Arity get_arity() const { return _environ.size(); }
virtual size_t size() const { return get_arity(); }
virtual const HandleSeq& getOutgoingSet() const { return _outgoing; }
virtual Handle getOutgoingAtom(Arity) const;
virtual void setAtomSpace(AtomSpace *);
const std::vector<AtomSpacePtr>& getEnviron() const { return _environ; }
void set_name(const std::string&);
virtual bool operator==(const Atom&) const;
virtual bool operator<(const Atom&) const;
static bool content_compare(const AtomSpace& first,
const AtomSpace& second,
bool check_values=CHECK_VALUES,
bool emit_diagnostics=DONT_EMIT_DIAGNOSTICS);
size_t get_size() const;
size_t get_num_atoms_of_type(Type type, bool subclass=false) const;
void clear();
void barrier(void);
Handle add_atom(const Handle&);
Handle add_atom(const AtomPtr& a)
{ return add_atom(a->get_handle()); }
Handle add_node(Type, std::string&&);
Handle xadd_node(Type t, std::string str) {
return add_node(t, std::move(str));
}
Handle add_link(Type, HandleSeq&&);
Handle xadd_link(Type t, HandleSeq seq) {
return add_link(t, std::move(seq));
}
inline Handle add_link(Type t)
{
return add_link(t, HandleSeq{});
}
inline Handle add_link(Type t, Handle h)
{
return add_link(t, HandleSeq({h}));
}
inline Handle add_link(Type t, Handle ha, Handle hb)
{
return add_link(t, {ha, hb});
}
inline Handle add_link(Type t, Handle ha, Handle hb, Handle hc)
{
return add_link(t, {ha, hb, hc});
}
inline Handle add_link(Type t, Handle ha, Handle hb, Handle hc, Handle hd)
{
return add_link(t, {ha, hb, hc, hd});
}
inline Handle add_link(Type t, Handle ha, Handle hb, Handle hc,
Handle hd, Handle he)
{
return add_link(t, {ha, hb, hc, hd, he});
}
inline Handle add_link(Type t, Handle ha, Handle hb, Handle hc,
Handle hd, Handle he, Handle hf)
{
return add_link(t, {ha, hb, hc, hd, he, hf});
}
inline Handle add_link(Type t, Handle ha, Handle hb, Handle hc,
Handle hd, Handle he, Handle hf, Handle hg)
{
return add_link(t, {ha, hb, hc, hd, he, hf, hg});
}
inline Handle add_link(Type t, Handle ha, Handle hb, Handle hc,
Handle hd, Handle he, Handle hf, Handle hg,
Handle hh)
{
return add_link(t, {ha, hb, hc, hd, he, hf, hg, hh});
}
inline Handle add_link(Type t, Handle ha, Handle hb, Handle hc,
Handle hd, Handle he, Handle hf, Handle hg,
Handle hh, Handle hi)
{
return add_link(t, {ha, hb, hc, hd, he, hf, hg, hh, hi});
}
ValuePtr add_atoms(const ValuePtr&);
Handle get_atom(const Handle&) const;
bool extract_atom(const Handle&, bool recursive=false);
bool remove_atom(const Handle& h, bool recursive=false) {
return extract_atom(h, recursive);
}
Handle set_value(const Handle&, const Handle& key, const ValuePtr& value);
Handle set_truthvalue(const Handle&, const TruthValuePtr&);
Handle increment_count(const Handle&, const Handle&, const std::vector<double>&);
Handle increment_count(const Handle&, const Handle&, size_t, double);
Handle increment_countTV(const Handle&, double = 1.0);
Handle lookupHandle(const Handle& h) const
{ return lookupHide(h, true); }
Handle get_node(Type, std::string&&) const;
inline Handle xget_handle(Type t, std::string str) const {
return get_node(t, std::move(str));
}
inline Handle get_handle(Type t, std::string str) const {
return get_node(t, std::move(str));
}
Handle get_link(Type, HandleSeq&&) const;
inline Handle xget_handle(Type t, HandleSeq outgoing) const {
return get_link(t, std::move(outgoing));
}
inline Handle get_handle(Type t, HandleSeq outgoing) const {
return get_link(t, std::move(outgoing));
}
inline Handle get_link(Type t, const Handle& ha, const Handle& hb) const {
return get_link(t, {ha, hb});
}
bool is_valid_handle(const Handle& h) const {
return (nullptr != h) and (h->getAtomSpace() != nullptr);
}
void
get_handles_by_type(HandleSeq&,
Type type,
bool subclass=false,
bool parent=true,
const AtomSpace* = nullptr) const;
void
get_handles_by_type(UnorderedHandleSet&,
Type type,
bool subclass=false,
bool parent=true,
const AtomSpace* = nullptr) const;
void
get_root_set_by_type(HandleSeq&,
Type type,
bool subclass=false,
bool parent=true,
const AtomSpace* = nullptr) const;
virtual std::string to_string(void) const;
virtual std::string to_string(const std::string& indent) const;
virtual std::string to_short_string(const std::string& indent) const;
Handle storage_add_nocheck(const Handle& h) { return add(h); }
};
static inline AtomSpacePtr AtomSpaceCast(const ValuePtr& a)
{ return std::dynamic_pointer_cast<AtomSpace>(a); }
static inline AtomSpacePtr AtomSpaceCast(AtomSpace* as)
{ return AtomSpaceCast(as->shared_from_this()); }
static inline Handle HandleCast(AtomSpace* as)
{ return HandleCast(as->shared_from_this()); }
template< class... Args >
AtomSpacePtr createAtomSpace( Args&&... args )
{
AtomSpacePtr asp(std::make_shared<AtomSpace>(std::forward<Args>(args) ...));
asp->setAtomSpace(asp.get());
asp->install();
asp->setAtomSpace(nullptr);
return asp;
}
}
namespace std {
ostream& operator<<(ostream&, const opencog::AtomSpace&);
}
#endif