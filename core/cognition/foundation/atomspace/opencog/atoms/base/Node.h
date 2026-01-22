#ifndef _OPENCOG_NODE_H
#define _OPENCOG_NODE_H
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/ClassServer.h>
namespace opencog
{
class Node : public Atom
{
protected:
std::string _name;
void init();
virtual ContentHash compute_hash() const;
public:
Node(Type t, const std::string s)
: Atom(t), _name(std::move(s))
{
init();
}
Node(const Node&) = delete;
Node& operator=(const Node&) = delete;
virtual bool is_node() const { return true; }
virtual bool is_link() const { return false; }
virtual const std::string& get_name() const { return _name; }
virtual size_t size() const { return 1; }
std::string to_string(const std::string& indent) const;
std::string to_short_string(const std::string& indent) const;
std::string to_string_esc(void) const;
using Atom::to_string;
using Atom::to_short_string;
virtual bool operator==(const Atom&) const;
virtual bool operator<(const Atom&) const;
};
#define NODE_PTR_DECL(CNAME) ATOM_PTR_DECL(CNAME)
NODE_PTR_DECL(Node)
template< class... Args >
Handle createNode( Args&&... args )
{
Handle tmp(std::make_shared<Node>(std::forward<Args>(args) ...));
return classserver().factory(tmp);
}
}
#endif