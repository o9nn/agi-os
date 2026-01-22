#ifndef _OPENCOG_MCP_EVAL_H
#define _OPENCOG_MCP_EVAL_H
#include <string>
#include <memory>
#include <unordered_map>
#include <vector>
#include <opencog/eval/GenericEval.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/persist/json/McpPlugin.h>
namespace opencog {
class McpEval : public GenericEval
{
private:
McpEval(const AtomSpacePtr&);
bool _started;
bool _done;
std::string _result;
AtomSpacePtr _atomspace;
std::vector<std::shared_ptr<McpPlugin>> _plugins;
std::unordered_map<std::string, std::shared_ptr<McpPlugin>> _tool_to_plugin;
public:
virtual ~McpEval();
virtual std::string get_name(void) const { return "McpEval"; }
virtual void begin_eval(void);
virtual void eval_expr(const std::string&);
virtual std::string poll_result(void);
virtual void interrupt(void);
void register_plugin(std::shared_ptr<McpPlugin> plugin);
void unregister_plugin(std::shared_ptr<McpPlugin> plugin);
static McpEval* get_evaluator(const AtomSpacePtr&);
};
}
#endif