#ifndef _OPENCOG_MCP_SERVER_H
#define _OPENCOG_MCP_SERVER_H
#include <string>
#include <opencog/network/ConsoleSocket.h>
#include <opencog/cogserver/server/CogServer.h>
#include <opencog/cogserver/shell/McpEval.h>
namespace opencog
{
class MCPServer : public ConsoleSocket
{
private:
CogServer& _cserver;
McpEval* _eval;
protected:
virtual void OnConnection(void);
virtual void OnLine (const std::string&);
public:
MCPServer(CogServer&);
~MCPServer();
};
}
#endif