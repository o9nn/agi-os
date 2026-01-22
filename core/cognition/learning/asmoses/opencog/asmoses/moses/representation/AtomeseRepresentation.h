#ifndef _ATOMESE_REPRESENTATION_H
#define _ATOMESE_REPRESENTATION_H
#include <opencog/util/random.h>
#include <opencog/util/empty_string.h>
#include <opencog/asmoses/reduct/reduct/reduct.h>
#include <boost/operators.hpp>
#include "field_set.h"
#include "representation_parameters.h"
namespace opencog
{
namespace moses
{
struct AtomeseRepresentation : public boost::noncopyable, boost::equality_comparable<AtomeseRepresentation>
{
AtomeseRepresentation(const Handle& exemplar,
const Handle& tt,
AtomSpacePtr as,
const HandleSet &ignore_ops,
const representation_parameters& rp);
protected:
Handle _exemplar;
field_set _fields;
AtomSpacePtr _as;
const Handle _DSN =
createNode(DEFINED_SCHEMA_NODE, randstr(std::string("REP") + "-"));
Handle _rep;
HandleSeq _variables;
representation_parameters _rep_params;
Handle get_candidate(const Handle &h);
typedef std::map<Handle, int> lookup;
lookup disc_lookup;
lookup contin_lookup;
public:
Handle get_candidate(const instance inst, bool reduce);
void set_rep(Handle);
void set_fields(field_set);
const field_set& fields() const {
return _fields;
}
void set_variables(HandleSeq);
void clean_atomese_prog(Handle&, bool reduce, bool knob_building=false);
bool operator==(const AtomeseRepresentation& other) const;
const Handle rep() const {
return _rep;
}
typedef std::multimap<field_set::disc_spec, Handle> disc_map;
typedef std::multimap<field_set::contin_spec, Handle> contin_map;
disc_map disc;
contin_map contin;
std::ostream& ostream_rep(std::ostream& out) const;
std::ostream& ostream_exemplar(std::ostream& out) const;
std::ostream& ostream_handle(const Handle&, std::ostream &out) const;
std::ostream& ostream_link(const Handle &h, std::ostream &out) const;
};
}
std::string oc_to_string(const moses::AtomeseRepresentation& rep,
const std::string& indent=empty_string);
}
#endif