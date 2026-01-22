#ifndef _OPENCOG_INSTANTIATOR_H
#define _OPENCOG_INSTANTIATOR_H
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/core/Context.h>
namespace opencog {
class Instantiator
{
private:
AtomSpace *_as;
struct Instate
{
Instate(const GroundingMap& varmap) :
_varmap(varmap),
_context(false),
_consume_quotations(true),
_needless_quotation(true),
_halt(false)
{}
const GroundingMap& _varmap;
Context _context;
bool _consume_quotations;
bool _needless_quotation;
bool _inside_evaluation;
bool _halt;
bool _silent;
};
Handle walk_tree(const Handle& tree,
Instate&) const;
bool walk_sequence(HandleSeq&, const HandleSeq&,
Instate&) const;
Handle reduce_exout(const Handle& exout,
Instate&) const;
static bool not_self_match(Type t);
public:
Instantiator(AtomSpace* as);
Instantiator(const AtomSpacePtr&);
ValuePtr instantiate(const Handle& expr,
const GroundingMap& vars,
bool silent=false) const;
ValuePtr execute(const Handle& expr, bool silent=false);
};
}
#endif