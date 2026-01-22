#ifndef _OPENCOG_REQUEST_MANAGER_H
#define _OPENCOG_REQUEST_MANAGER_H
#include <mutex>
#include <vector>
#include <opencog/util/concurrent_queue.h>
#include <opencog/cogserver/server/Factory.h>
#include <opencog/cogserver/server/Request.h>
#include <opencog/cogserver/server/RequestClassInfo.h>
namespace opencog
{
class RequestManager
{
protected:
std::map<const std::string, AbstractFactory<Request> const*>
_factories;
std::map<const std::string, Request*> requests;
std::mutex processRequestsMutex;
concurrent_queue<Request*> requestQueue;
public:
RequestManager(void);
~RequestManager(void);
bool registerRequest(const std::string& id,
AbstractFactory<Request> const* factory);
bool unregisterRequest(const std::string& id);
std::list<const char*> requestIds(void) const;
Request* createRequest(const std::string& id, CogServer&);
const RequestClassInfo& requestInfo(const std::string& id) const;
void pushRequest(Request* request) { requestQueue.push(request); }
Request* popRequest(void) { return requestQueue.value_pop(); }
int getRequestQueueSize(void) { return requestQueue.size(); }
void processRequests(void);
};
}
#endif