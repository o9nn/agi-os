#ifndef _OPENCOG_PATTERN_LINK_H
#define _OPENCOG_PATTERN_LINK_H
#include <unordered_map>
#include <opencog/atoms/core/Quotation.h>
#include <opencog/atoms/rule/RuleLink.h>
#include <opencog/atoms/pattern/Pattern.h>
namespace opencog
{
class PatternLink;
LINK_PTR_DECL(PatternLink)
class PatternLink : public RuleLink
{
protected:
Pattern _pat;
PatternTermSeq _fixed;
size_t _num_virts;
HandleSeq _virtual;
size_t _num_comps;
HandleSeqSeq _components;
HandleSetSeq _component_vars;
HandleSeq _component_patterns;
PatternTermPtr make_term_tree(const Handle&);
void make_term_tree_recursive(const PatternTermPtr&,
PatternTermPtr&);
void pin_term(const PatternTermPtr&);
void pin_term_recursive(const PatternTermPtr&,
const PatternTermPtr&);
void record_mandatory(const PatternTermPtr&);
bool record_literal(const PatternTermPtr&, bool reverse=false);
void unbundle_clauses(const Handle& body);
bool unbundle_clauses_rec(const PatternTermPtr&,
const TypeSet&,
bool reverse=false);
void locate_defines(const PatternTermSeq& clauses);
void validate_variables(HandleSet& vars,
const HandleSeq& clauses);
bool is_virtual(const Handle&);
void locate_cacheable(const PatternTermSeq& clauses);
bool need_dummies(const PatternTermPtr&);
bool add_unaries(const PatternTermPtr&);
void add_dummies(const PatternTermPtr&);
void make_connectivity_map(void);
void make_map_recursive(const Handle&, const PatternTermPtr&);
void check_connectivity(const HandleSeqSeq&);
void check_satisfiability(const HandleSet&,
const HandleSetSeq&);
void get_clause_variables(const PatternTermPtr&);
void clauses_get_variables(const PatternTermSeq&);
void init(void);
void init_bottom(void);
void common_init(void);
void disjointed_init(void);
void setup_components(void);
virtual void setAtomSpace(AtomSpace *);
protected:
static void prt(const Handle& h)
{
printf("%s\n", h->to_short_string().c_str());
}
public:
PatternLink(const HandleSeq&&, Type=PATTERN_LINK);
PatternLink(const Handle& body);
PatternLink(const Handle& varcdecls, const Handle& body);
PatternLink(const Variables&, const Handle&);
PatternLink(const PatternLink&) = delete;
PatternLink& operator=(const PatternLink&) = delete;
PatternLink(const HandleSet& vars,
const Variables& varspec,
const HandleSeq& component,
const PatternTermSeq& absents);
PatternLink(const HandleSet&,
const HandleSeq&);
PatternLinkPtr jit_analyze(void);
const Variables& get_variables(void) const { return _variables; }
const Pattern& get_pattern(void) const { return _pat; }
const HandleSeqSeq& get_components(void) const { return _components; }
const HandleSeq& get_component_patterns(void) const
{ return _component_patterns; }
const HandleSeq& get_virtual(void) const { return _virtual; }
void debug_log(std::string) const;
static Handle factory(const Handle&);
std::string to_long_string(const std::string& indent) const;
};
#define createPatternLink CREATE_DECL(PatternLink)
std::string oc_to_string(const PatternLink& pl,
const std::string& indent=empty_string);
}
#endif