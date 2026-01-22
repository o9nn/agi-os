#ifndef _OPENCOG_MCP_PLUG_ECHO_H
#define _OPENCOG_MCP_PLUG_ECHO_H
#include <opencog/persist/json/McpPlugin.h>
namespace opencog {
class McpPlugEcho : public McpPlugin
{
public:
McpPlugEcho() = default;
virtual ~McpPlugEcho() = default;
virtual std::string get_tool_descriptions() const override;
virtual std::string invoke_tool(const std::string& tool_name,
const std::string& arguments) const override;
};
}
#if HAVE_MCP
#include <json/json.h>
#include <sstream>
#include <memory>
static inline std::string json_to_string(const Json::Value& value) {
static Json::StreamWriterBuilder builder = []() {
Json::StreamWriterBuilder b;
b["indentation"] = "";
return b;
}();
std::unique_ptr<Json::StreamWriter> writer(builder.newStreamWriter());
std::ostringstream oss;
writer->write(value, &oss);
return oss.str();
}
#endif
#endif