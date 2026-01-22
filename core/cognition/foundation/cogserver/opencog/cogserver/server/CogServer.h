#ifndef _OPENCOG_COGSERVER_H
#define _OPENCOG_COGSERVER_H
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/cogserver/server/Module.h>
#include <opencog/cogserver/server/ModuleManager.h>
#include <opencog/network/NetworkServer.h>
#include <opencog/cogserver/server/RequestManager.h>
namespace opencog
{
class CogServer :
public RequestManager,
public ModuleManager
{
protected:
AtomSpacePtr _atomSpace;
NetworkServer* _consoleServer;
NetworkServer* _webServer;
NetworkServer* _mcpServer;
bool _running;
CogServer(void);
CogServer(AtomSpacePtr);
friend CogServer& cogserver(void);
friend CogServer& cogserver(AtomSpacePtr);
public:
virtual ~CogServer(void);
AtomSpacePtr getAtomSpace() { return _atomSpace; }
void setAtomSpace(const AtomSpacePtr& as) { _atomSpace = as; }
virtual void serverLoop(void);
virtual void runLoopStep(void);
virtual void stop(void);
void set_max_open_sockets(int);
virtual void enableNetworkServer(int port=17001);
virtual void enableWebServer(int port=18080);
virtual void enableMCPServer(int port=18888);
virtual void disableNetworkServer(void);
virtual void disableWebServer(void);
virtual void disableMCPServer(void);
bool running(void) { return _running; }
Request* createRequest(const std::string& id) {
return RequestManager::createRequest(id, *this);
}
bool loadModule(const std::string& filename) {
return ModuleManager::loadModule(filename, *this);
}
void loadModules(void) { ModuleManager::loadModules(*this); }
Logger &logger(void) { return opencog::logger(); }
std::string display_stats(int nlines = -1);
std::string display_web_stats(void);
static std::string stats_legend(void);
};
CogServer& cogserver(void);
CogServer& cogserver(AtomSpacePtr);
inline AtomSpacePtr cython_server_atomspace(void)
{
return cogserver().getAtomSpace();
}
}
#endif