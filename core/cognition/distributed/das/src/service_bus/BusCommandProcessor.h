#pragma once
#include <set>
#include <string>
#include <vector>
#include "BusCommandProxy.h"
using namespace std;
namespace service_bus {
class BusCommandProcessor {
friend class ServiceBus;
public:
BusCommandProcessor(const set<string>& commands);
virtual ~BusCommandProcessor() {}
virtual shared_ptr<BusCommandProxy> factory_empty_proxy() = 0;
virtual void run_command(shared_ptr<BusCommandProxy> proxy) = 0;
private:
bool check_command(const string& command);
set<string> commands;
};
}