#ifndef _OPENCOG_AGGREGATE_H
#define _OPENCOG_AGGREGATE_H
#include <set>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/generate/Odometer.h>
#include <opencog/generate/GenerateCallback.h>
namespace opencog
{
class Aggregate
{
private:
AtomSpace* _as;
AtomSpacePtr _scratch;
GenerateCallback* _cb;
OdoFrame _frame;
Odometer _odo;
std::stack<OdoFrame> _frame_stack;
std::stack<HandleSeq> _odo_sections;
void push_frame();
void pop_frame();
std::stack<Odometer> _odo_stack;
void push_odo();
void pop_odo();
void clear(void);
bool init_odometer(void);
bool step_odometer(void);
bool do_step(void);
void recurse(void);
HandlePair connect_section(const Handle&, size_t,
const Handle&, const Handle&);
Handle make_link(const Handle&, size_t, const Handle&);
public:
Aggregate(AtomSpace*);
~Aggregate();
void aggregate(const HandleSet&, GenerateCallback&);
};
}
#endif