#pragma once
#include <map>
#include <memory>
#include <thread>
#include "BusCommandProcessor.h"
#include "ContextBrokerProxy.h"
#include "StoppableThread.h"
using namespace std;
using namespace service_bus;
namespace context_broker {
class ContextBrokerProcessor : public BusCommandProcessor {
public:
ContextBrokerProcessor();
~ContextBrokerProcessor();
virtual shared_ptr<BusCommandProxy> factory_empty_proxy();
virtual void run_command(shared_ptr<BusCommandProxy> proxy);
void create_context(shared_ptr<StoppableThread> monitor, shared_ptr<ContextBrokerProxy> proxy);
void write_cache(shared_ptr<ContextBrokerProxy> proxy);
bool read_cache(shared_ptr<ContextBrokerProxy> proxy);
void set_attention_broker_parameters(double rent_rate,
double spreading_rate_lowerbound,
double spreading_rate_upperbound);
void update_attention_broker(shared_ptr<ContextBrokerProxy> proxy);
private:
void thread_process_one_query(shared_ptr<StoppableThread> monitor,
shared_ptr<ContextBrokerProxy> proxy);
map<string, shared_ptr<StoppableThread>> query_threads;
mutex query_threads_mutex;
shared_ptr<ContextBrokerProxy> proxy;
};
}