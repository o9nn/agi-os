#ifndef _OPENCOG_MCP_PLUGIN_H
#define _OPENCOG_MCP_PLUGIN_H
#include <string>
#include <vector>
namespace opencog {
class McpPlugin
{
public:
virtual ~McpPlugin() = default;
virtual std::string get_tool_descriptions() const = 0;
virtual std::string invoke_tool(const std::string& tool_name,
const std::string& arguments) const = 0;
};
}
#endif