#ifndef _OPENCOG_ATOMSPACE_PUBLISHER_MODULE_H
#define _OPENCOG_ATOMSPACE_PUBLISHER_MODULE_H
#include <string>
#include <lib/zmq/zhelpers.hpp>
#include <json/json.h>
#include <tbb/task.h>
#include <tbb/concurrent_queue.h>
#include <opencog/util/sigslot.h>
#include <opencog/cogserver/server/Module.h>
#include <opencog/cogserver/server/CogServer.h>
#include <opencog/attentionbank/bank/AttentionBank.h>
#ifndef TBB_H
#define TBB_H
template<typename F>
class lambda_task : public tbb::task {
F my_func;
tbb::task* execute() {
my_func();
return NULL;
}
public:
lambda_task(const F& f) : my_func(f) {}
};
template<typename F>
void tbb_enqueue_lambda(const F& f) {
tbb::task::enqueue(*new(tbb::task::allocate_root()) lambda_task<F>(f));
}
#endif
namespace opencog
{
class CogServer;
class AtomSpacePublisherModule;
typedef std::shared_ptr<AtomSpacePublisherModule> AtomSpacePublisherModulePtr;
struct message_t {
std::string type;
std::string payload;
};
const int HWM = 10000000;
class AtomSpacePublisherModule : public Module
{
private:
AtomSpace* as;
AtomSignal* _remove_atom_signal;
int _remove_atom_connection;
AtomSignal* _add_atom_signal;
int _add_atom_connection;
TVCHSigl* _tvchange_signal;
int _tvchange_connection;
AVCHSigl* _avchange_signal;
int _avchange_connection;
AVCHSigl* _add_af_signal;
int _add_af_connection;
AVCHSigl* _remove_af_signal;
int _remove_af_connection;
void enableSignals();
void disableSignals();
tbb::concurrent_bounded_queue<message_t> queue;
zmq::context_t * context;
void InitZeroMQ();
void proxy();
void sendMessage(std::string messageType, std::string payload);
std::string atomMessage(Json::Value jsonAtom);
std::string avMessage(Json::Value jsonAtom,
Json::Value jsonAVOld,
Json::Value jsonAVNew);
std::string tvMessage(Json::Value jsonAtom,
Json::Value jsonTVOld,
Json::Value jsonTVNew);
Json::Value atomToJSON(Handle h);
Json::Value tvToJSON(TruthValuePtr tv);
Json::Value avToJSON(AttentionValuePtr av);
DECLARE_CMD_REQUEST(AtomSpacePublisherModule, "publisher-enable-signals",
do_publisherEnableSignals,
"Enable AtomSpace event publishing",
"Usage: publisher-enable-signals",
false, false)
DECLARE_CMD_REQUEST(AtomSpacePublisherModule, "publisher-disable-signals",
do_publisherDisableSignals,
"Disable AtomSpace event publishing",
"Usage: publisher-disable-signals",
false, false)
public:
AtomSpacePublisherModule(CogServer&);
virtual ~AtomSpacePublisherModule();
virtual void run();
static const char *id(void);
virtual void init(void);
void atomAddSignal(Handle h);
void atomRemoveSignal(AtomPtr atom);
void AVChangedSignal(const Handle& h,
const AttentionValuePtr& av_old,
const AttentionValuePtr& av_new);
void TVChangedSignal(const Handle& h,
const TruthValuePtr& tv_old,
const TruthValuePtr& tv_new);
void addAFSignal(const Handle& h,
const AttentionValuePtr& av_old,
const AttentionValuePtr& av_new);
void removeAFSignal(const Handle& h,
const AttentionValuePtr& av_old,
const AttentionValuePtr& av_new);
};
}
#endif