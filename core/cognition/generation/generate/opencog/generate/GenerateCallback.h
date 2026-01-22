#ifndef _OPENCOG_GENERATE_CALLBACK_H
#define _OPENCOG_GENERATE_CALLBACK_H
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/generate/Odometer.h>
namespace opencog
{
class GenerateCallback
{
protected:
AtomSpace* _as;
public:
GenerateCallback(AtomSpace* as) : _as(as) {}
virtual ~GenerateCallback() {}
virtual void clear(AtomSpace*) = 0;
virtual void root_set(const HandleSet& points) = 0;
virtual HandleSet next_root(void) = 0;
virtual HandleSeq joints(const Handle&) = 0;
virtual Handle select(const OdoFrame&,
const Handle& fm_sect, size_t offset,
const Handle& to_con) = 0;
virtual Handle make_link(const Handle& fm_con, const Handle& to_con,
const Handle& fm_pnt, const Handle& to_pnt) = 0;
virtual size_t num_links(const Handle& fm_sect, const Handle& to_sect,
const Handle& link_type) = 0;
virtual void push_frame(const OdoFrame&) {}
virtual void pop_frame(const OdoFrame&) {}
virtual void push_odometer(const Odometer&) {}
virtual void pop_odometer(const Odometer&) {}
virtual bool step(const OdoFrame&) { return true; }
virtual void solution(const OdoFrame&) = 0;
virtual Handle get_solutions(void) = 0;
size_t max_solutions = -1;
bool allow_self_connections = false;
size_t pair_any_links = 1;
size_t pair_typed_links = 1;
size_t max_network_size = -1;
size_t max_depth = -1;
size_t max_steps = 25101;
Handle point_set = Handle::UNDEFINED;
};
}
#endif