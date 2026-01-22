#ifndef _OPENCOG_COLLECT_STYLE_H
#define _OPENCOG_COLLECT_STYLE_H
#include <opencog/generate/Odometer.h>
namespace opencog
{
class CollectStyle
{
protected:
std::set<HandleSet> _solutions;
public:
CollectStyle(void);
~CollectStyle();
void clear(void) { _solutions.clear(); }
void record_solution(const OdoFrame&);
size_t num_solutions(void) { return _solutions.size(); }
std::set<HandleSet> get_solution_set(void) { return _solutions; }
Handle get_solutions(void);
};
}
#endif