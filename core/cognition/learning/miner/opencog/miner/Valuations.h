#ifndef OPENCOG_VALUATIONS_H_
#define OPENCOG_VALUATIONS_H_
#include <opencog/util/Counter.h>
#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/core/Variables.h>
namespace opencog
{
typedef Counter<Handle, double> HandleCounter;
typedef Counter<Handle, unsigned> HandleUCounter;
class ValuationsBase
{
public:
ValuationsBase(const Variables& variables);
ValuationsBase();
bool no_focus() const;
const Handle& focus_variable() const;
unsigned focus_index() const;
HandleSeq remaining_variables() const;
void inc_focus_variable() const;
void dec_focus_variable() const;
Handle variable(unsigned i) const;
unsigned index(const Handle& var) const;
unsigned size() const;
bool empty() const;
Variables variables;
protected:
mutable unsigned _var_idx;
};
class SCValuations : public ValuationsBase
{
public:
SCValuations(const Variables& variables, const Handle& satset=Handle::UNDEFINED);
HandleUCounter values(const Handle& var) const;
HandleUCounter values(unsigned var_idx) const;
const Handle& focus_value(const HandleSeq& values) const;
bool operator<(const SCValuations& other) const;
unsigned size() const;
bool empty() const;
std::string to_string(const std::string& indent=empty_string) const;
HandleSeqSeq valuations;
};
typedef std::set<SCValuations> SCValuationsSet;
class Valuations : public ValuationsBase
{
public:
Valuations(const Handle& pattern, const HandleSeq& db);
Valuations(const Variables& variables, const SCValuationsSet& scvs);
Valuations(const Variables& variables);
const SCValuations& get_scvaluations(const Handle& var) const;
const SCValuations& get_scvaluations(unsigned var_idx) const;
const SCValuations& focus_scvaluations() const;
void inc_focus_variable() const;
void dec_focus_variable() const;
HandleUCounter values(const Handle& var) const;
HandleUCounter values(unsigned var_idx) const;
unsigned size() const;
bool empty() const;
std::string to_string(const std::string& indent=empty_string) const;
SCValuationsSet scvs;
private:
void setup_size();
unsigned _size;
};
typedef std::map<Handle, Valuations> HandleValuationsMap;
std::string oc_to_string(const SCValuations& scvaluations,
const std::string& indent=empty_string);
std::string oc_to_string(const SCValuationsSet& scvs,
const std::string& indent=empty_string);
std::string oc_to_string(const Valuations& valuations,
const std::string& indent=empty_string);
std::string oc_to_string(const HandleValuationsMap& h2vals,
const std::string& indent=empty_string);
}
#endif