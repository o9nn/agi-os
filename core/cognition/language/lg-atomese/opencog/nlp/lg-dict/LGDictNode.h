#ifndef _OPENCOG_LG_DICT_NODE_H
#define _OPENCOG_LG_DICT_NODE_H
#include <string>
#include <link-grammar/dict-api.h>
#include <opencog/atoms/base/Node.h>
namespace opencog
{
class LgDictNode : public Node
{
protected:
Dictionary _dict;
public:
LgDictNode(const std::string&&);
LgDictNode(const LgDictNode&) = delete;
LgDictNode& operator=(const LgDictNode&) = delete;
virtual ~LgDictNode();
virtual void setAtomSpace(AtomSpace*);
Dictionary get_dictionary(void);
static Handle factory(const Handle&);
};
typedef std::shared_ptr<LgDictNode> LgDictNodePtr;
static inline LgDictNodePtr LgDictNodeCast(const Handle& h)
{ return std::dynamic_pointer_cast<LgDictNode>(h); }
static inline LgDictNodePtr LgDictNodeCast(AtomPtr a)
{ return std::dynamic_pointer_cast<LgDictNode>(a); }
#define createLgDictNode std::make_shared<LgDictNode>
}
#endif