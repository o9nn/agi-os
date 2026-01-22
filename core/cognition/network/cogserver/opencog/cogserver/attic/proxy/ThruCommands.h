#ifndef _OPENCOG_THRU_COMMANDS_H
#define _OPENCOG_THRU_COMMANDS_H
#include <vector>
#include <opencog/persist/api/StorageNode.h>
#include <opencog/persist/sexpr/Commands.h>
namespace opencog {
class ThruCommands : public UnwrappedCommands
{
protected:
AtomSpacePtr _as;
Handle _truth_key;
std::vector<StorageNodePtr> _targets;
Commands _decoder;
public:
ThruCommands();
virtual ~ThruCommands();
void init(const AtomSpacePtr&);
};
}
#endif