#include <opencog/atomspace/AtomSpace.h>
extern "C"
{
using namespace opencog;
Handle* PatternMatcher_BindLink(AtomSpace* this_ptr, Handle* handle);
int PatternMatcher_SatisfactionLink(AtomSpace* this_ptr
, Handle* handle
, char** tv_type
, double* parameters);
}