#ifndef _OPENCOG_UNIFIER_LINK_H
#define _OPENCOG_UNIFIER_LINK_H
#include <opencog/atoms/base/Link.h>
#include <opencog/unify/types/atom_types.h>
namespace opencog
{
class Unify;
class UnifierLink : public Link
{
private:
void init(void);
protected:
Unify* _unifier;
bool _is_dynamic;
void make_uni(const HandleSeq&);
HandleSeq rewrite(AtomSpace*, bool);
public:
UnifierLink(const HandleSeq&&, Type = UNIFIER_LINK);
UnifierLink(const UnifierLink&) = delete;
UnifierLink& operator=(const UnifierLink&) = delete;
virtual ~UnifierLink();
virtual bool is_executable(void) const { return true; }
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(UnifierLink)
#define createUnifierLink CREATE_DECL(UnifierLink)
}
extern "C" {
void opencog_unify_atoms_init(void);
};
#endif