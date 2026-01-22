#ifndef _OPENCOG_WEB_SERVER_H
#define _OPENCOG_WEB_SERVER_H
#include <string>
#include <opencog/network/ConsoleSocket.h>
#include <opencog/cogserver/server/CogServer.h>
#include <opencog/cogserver/server/Request.h>
namespace opencog
{
class WebServer : public ConsoleSocket
{
private:
CogServer& _cserver;
Request* _request;
protected:
virtual void OnConnection(void);
virtual void OnLine (const std::string&);
std::string html_stats(void);
std::string favicon(void);
public:
WebServer(CogServer&);
~WebServer();
};
}
#endif