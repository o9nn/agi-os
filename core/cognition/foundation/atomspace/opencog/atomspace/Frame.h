#ifndef _OPENCOG_FRAME_H
#define _OPENCOG_FRAME_H
#include <opencog/atoms/base/Atom.h>
namespace opencog
{
class Frame : public Atom
{
private:
void init();
protected:
HandleSeq _outgoing;
std::string _name;
void scrub_incoming_set();
virtual void remove();
public:
virtual void install();
public:
Frame(Type t)
: Atom(t)
{
init();
}
Frame(Type t, const HandleSeq oset)
: Atom(t), _outgoing(std::move(oset))
{
init();
}
virtual ~Frame();
bool is_atom(void) const { return true; }
};
typedef std::shared_ptr<Frame> FramePtr;
static inline FramePtr FrameCast(const ValuePtr& a)
{ return std::dynamic_pointer_cast<Frame>(a); }
}
#endif