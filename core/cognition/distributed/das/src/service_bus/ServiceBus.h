#pragma once
#include <mutex>
#include <set>
#include <string>
#include "BusCommandProcessor.h"
#include "BusCommandProxy.h"
#include "BusNode.h"
#include "PortPool.h"
#include "Utils.h"
using namespace std;
using namespace commons;
using namespace distributed_algorithm_node;
namespace service_bus {
class ServiceBus {
private:
class Node : public BusNode {
public:
Node();
Node(const string& id,
shared_ptr<BusNode::Bus> bus,
const set<string>& node_commands,
const string& known_peer);
shared_ptr<Message> message_factory(string& command, vector<string>& args);
shared_ptr<BusCommandProcessor> processor;
private:
shared_ptr<BusNode::Bus> bus;
};
class BusCommandMessage : public Message {
public:
BusCommandMessage(const string& command, const vector<string>& args);
void act(shared_ptr<MessageFactory> node);
private:
string command;
vector<string> args;
};
static set<string> SERVICE_LIST;
shared_ptr<ServiceBus::Node> bus_node;
shared_ptr<BusNode::Bus> bus;
mutex api_mutex;
unsigned int next_request_serial;
public:
static string PATTERN_MATCHING_QUERY;
static string QUERY_EVOLUTION;
static string LINK_CREATION;
static string INFERENCE;
static string CONTEXT;
static string ATOMDB;
virtual void register_processor(shared_ptr<BusCommandProcessor> processor);
virtual void issue_bus_command(shared_ptr<BusCommandProxy> bus_command);
static void initialize_statics(const set<string>& commands = {},
unsigned int port_lower = 64000,
unsigned int port_upper = 64999);
ServiceBus(const string& host_id, const string& known_peer = "");
~ServiceBus();
};
}