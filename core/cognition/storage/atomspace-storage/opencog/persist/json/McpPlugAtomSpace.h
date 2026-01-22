#ifndef _OPENCOG_MCP_PLUG_ATOMSPACE_H
#define _OPENCOG_MCP_PLUG_ATOMSPACE_H
#include <opencog/atomspace/AtomSpace.h>
#include "McpPlugin.h"
namespace opencog {
class McpPlugAtomSpace : public McpPlugin
{
private:
AtomSpace* _as;
public:
McpPlugAtomSpace(AtomSpace* as) : _as(as) {}
virtual ~McpPlugAtomSpace() = default;
virtual std::string get_tool_descriptions() const;
virtual std::string invoke_tool(const std::string& tool_name,
const std::string& arguments) const;
};
}
#endif