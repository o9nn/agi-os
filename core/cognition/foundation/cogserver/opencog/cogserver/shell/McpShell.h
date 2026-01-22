#ifndef _OPENCOG_MCP_SHELL_H
#define _OPENCOG_MCP_SHELL_H
#include <opencog/network/GenericShell.h>
#include <opencog/atomspace/AtomSpace.h>
namespace opencog {
class McpShell : public GenericShell
{
protected:
AtomSpacePtr _shellspace;
public:
McpShell(const AtomSpacePtr&);
virtual ~McpShell();
virtual GenericEval* get_evaluator(void);
};
}
#endif