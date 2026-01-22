#ifndef _OPENCOG_OCTOMAP_NODE_H
#define _OPENCOG_OCTOMAP_NODE_H
#include <memory>
#include <opencog/atoms/base/Node.h>
#include "TimeOctomap.h"
namespace opencog
{
using TimeOctomapPtr = std::shared_ptr<TimeOctomap<Handle>>;
class OctoMapNode : public Node
{
private:
TimeOctomapPtr octomapPtr;
protected:
public:
OctoMapNode(Type t, const std::string&);
OctoMapNode(const std::string&, TimeOctomapPtr);
virtual ~OctoMapNode();
inline TimeOctomapPtr get_map(void)
{
return octomapPtr;
}
static Handle factory(const Handle&);
};
typedef std::shared_ptr<OctoMapNode> OctoMapNodePtr;
static inline OctoMapNodePtr OctoMapNodeCast(const Handle& h)
{
return std::dynamic_pointer_cast<OctoMapNode>(h);
}
static inline OctoMapNodePtr OctoMapNodeCast(AtomPtr a)
{
return std::dynamic_pointer_cast<OctoMapNode>(a);
}
#define createOctoMapNode std::make_shared<OctoMapNode>
}
#endif