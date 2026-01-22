#ifndef _OPENCOG_SIMPLE_CALLBACK_H
#define _OPENCOG_SIMPLE_CALLBACK_H
#include <opencog/util/Counter.h>
#include <opencog/generate/CollectStyle.h>
#include <opencog/generate/Dictionary.h>
#include <opencog/generate/GenerateCallback.h>
#include <opencog/generate/LinkStyle.h>
namespace opencog
{
typedef Counter<Handle, unsigned> HandleUCounter;
class SimpleCallback :
public GenerateCallback,
private LinkStyle,
private CollectStyle
{
private:
Dictionary _dict;
size_t _steps_taken;
HandleSeqSeq _root_sections;
std::vector<HandleSeq::iterator> _root_iters;
Handle select_from_lexis(const OdoFrame&,
const Handle&, size_t,
const Handle&);
HandleUCounter _lexlit;
std::stack<HandleUCounter> _lexlit_stack;
Handle select_from_open(const OdoFrame&,
const Handle&, size_t,
const Handle&);
Handle check_self(const HandleSeq&, const Handle&,
const Handle&, size_t);
struct OpenSelections
{
HandleSeqMap _opensect;
HandleUCounter _openit;
};
OpenSelections _opensel;
std::stack<OpenSelections> _opensel_stack;
public:
SimpleCallback(AtomSpace*, const Dictionary&);
virtual ~SimpleCallback();
virtual void clear(AtomSpace*);
virtual bool step(const OdoFrame&);
virtual HandleSeq joints(const Handle& con) {
return _dict.joints(con);
}
virtual void root_set(const HandleSet&);
virtual HandleSet next_root(void);
virtual Handle select(const OdoFrame&,
const Handle&, size_t,
const Handle&);
virtual Handle make_link(const Handle&, const Handle&,
const Handle&, const Handle&);
virtual size_t num_links(const Handle&, const Handle&,
const Handle&);
virtual void push_frame(const OdoFrame&);
virtual void pop_frame(const OdoFrame&);
virtual void push_odometer(const Odometer&);
virtual void pop_odometer(const Odometer&);
virtual void solution(const OdoFrame&);
virtual Handle get_solutions(void);
};
}
#endif