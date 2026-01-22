#ifndef _OPENCOG_RANDOM_CALLBACK_H
#define _OPENCOG_RANDOM_CALLBACK_H
#include <opencog/generate/CollectStyle.h>
#include <opencog/generate/Dictionary.h>
#include <opencog/generate/GenerateCallback.h>
#include <opencog/generate/LinkStyle.h>
#include <opencog/generate/RandomParameters.h>
namespace opencog
{
class RandomCallback :
public GenerateCallback,
private LinkStyle,
private CollectStyle
{
private:
Dictionary _dict;
RandomParameters* _parms;
Handle _weight_key;
size_t _steps_taken;
HandleSeqSeq _root_sections;
std::vector<std::discrete_distribution<size_t>> _root_dist;
Handle select_from_lexis(const OdoFrame&,
const Handle&, size_t,
const Handle&);
std::map<Handle, std::discrete_distribution<size_t>> _distmap;
Handle select_from_open(const OdoFrame&,
const Handle&, size_t,
const Handle&);
struct OpenSelections
{
HandleSeqMap _opensect;
std::map<Handle, std::discrete_distribution<size_t>> _opendi;
};
OpenSelections _opensel;
std::stack<OpenSelections> _opensel_stack;
public:
RandomCallback(AtomSpace*, const Dictionary&, RandomParameters&);
virtual ~RandomCallback();
virtual void clear(AtomSpace*);
void set_weight_key(const Handle& pred) { _weight_key = pred; }
virtual void root_set(const HandleSet&);
virtual HandleSet next_root(void);
virtual HandleSeq joints(const Handle& con) {
return _dict.joints(con);
}
virtual Handle select(const OdoFrame&,
const Handle&, size_t,
const Handle&);
virtual Handle make_link(const Handle&, const Handle&,
const Handle&, const Handle&);
virtual size_t num_links(const Handle&, const Handle&,
const Handle&);
virtual void push_frame(const OdoFrame&);
virtual void pop_frame(const OdoFrame&);
virtual bool step(const OdoFrame&);
virtual void solution(const OdoFrame&);
virtual Handle get_solutions(void);
};
}
#endif