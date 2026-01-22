#ifndef _OPENCOG_SERVER_CONSOLE_H
#define _OPENCOG_SERVER_CONSOLE_H
#include <condition_variable>
#include <mutex>
#include <string>
#include <opencog/network/ConsoleSocket.h>
#include <opencog/cogserver/server/CogServer.h>
namespace opencog
{
class ServerConsole : public ConsoleSocket
{
private:
static std::string _prompt;
protected:
CogServer& _cserver;
bool handle_telnet_iac(const std::string&);
void OnConnection(void);
void OnLine(const std::string&);
public:
ServerConsole(CogServer&);
~ServerConsole();
void sendPrompt();
};
}
#endif