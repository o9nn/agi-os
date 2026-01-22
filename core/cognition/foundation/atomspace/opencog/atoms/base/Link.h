#ifndef _OPENCOG_LINK_H
#define _OPENCOG_LINK_H
#include <functional>
#include <string>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/ClassServer.h>
namespace opencog
{
class Link : public Atom
{
private:
void init();
protected:
HandleSeq _outgoing;
virtual void install();
virtual void remove();
virtual ContentHash compute_hash() const;
public:
Link(HandleSeq oset, Type t=LINK)
: Atom(t), _outgoing(std::move(oset))
{
init();
}
Link(Type t)
: Atom(t)
{
init();
}
Link(Type t, const Handle& h)
: Atom(t), _outgoing({h})
{
init();
}
Link(Type t, const Handle& ha, const Handle &hb)
: Atom(t), _outgoing({ha, hb})
{
init();
}
Link(Type t, const Handle& ha, const Handle &hb, const Handle &hc)
: Atom(t), _outgoing({ha, hb, hc})
{
init();
}
Link(Type t, const Handle& ha, const Handle &hb,
const Handle &hc, const Handle &hd)
: Atom(t), _outgoing({ha, hb, hc, hd})
{
init();
}
Link(const Link&) = delete;
Link& operator=(const Link&) = delete;
~Link();
virtual bool is_node() const { return false; }
virtual bool is_link() const { return true; }
virtual Arity get_arity() const {
return _outgoing.size();
}
virtual size_t size() const {
return _outgoing.size();
}
virtual const HandleSeq& getOutgoingSet() const
{
return _outgoing;
}
virtual Handle getOutgoingAtom(Arity pos) const
{
return _outgoing.at(pos);
}
template<class T>
inline bool foreach_outgoing(bool (T::*cb)(const Handle&), T *data)
{
for (const Handle& out_h : _outgoing) {
if ((data->*cb)(out_h)) return true;
}
return false;
}
std::string to_string(const std::string& indent) const;
std::string to_short_string(const std::string& indent) const;
using Atom::to_string;
using Atom::to_short_string;
virtual bool operator==(const Atom&) const;
virtual bool operator<(const Atom&) const;
};
#define LINK_PTR_DECL(CNAME)  ATOM_PTR_DECL(CNAME)
LINK_PTR_DECL(Link);
template< class... Args >
Handle createLink( Args&&... args )
{
Handle tmp(std::make_shared<Link>(std::forward<Args>(args) ...));
return classserver().factory(tmp);
}
}
namespace std {
template<>
struct less<opencog::LinkPtr>
{
bool operator()(const opencog::LinkPtr& la, const opencog::LinkPtr& lb) const
{
return la->operator<(*lb);
}
};
}
#endif