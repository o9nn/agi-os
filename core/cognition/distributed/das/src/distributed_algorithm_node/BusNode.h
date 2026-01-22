#pragma once
#include <string>
#include "DistributedAlgorithmNode.h"
using namespace std;
namespace distributed_algorithm_node {
class BusNode : public DistributedAlgorithmNode {
public:
class Bus {
public:
Bus();
Bus(const Bus& other);
void add(const string& command);
void set_ownership(const string& command, const string& node_id);
const string& get_ownership(const string& command);
bool contains(const string& command);
string to_string();
bool operator==(const Bus& other);
Bus& operator=(const Bus& other);
Bus& operator+(const string& command);
private:
map<string, string> command_owner;
};
static string SET_COMMAND_OWNERSHIP;
BusNode(const string& node_id,
const Bus& bus,
const set<string>& node_commands,
const string& known_peer = "",
MessageBrokerType messaging_backend = MessageBrokerType::GRPC);
virtual void node_joined_network(const string& node_id);
string cast_leadership_vote();
virtual shared_ptr<Message> message_factory(string& command, vector<string>& args);
void set_ownership(const string& command, const string& bus_node_id);
const string& get_ownership(const string& command);
void take_ownership(const set<string>& commands);
void send_bus_command(const string& command, const vector<string>& args);
virtual string to_string();
protected:
bool is_master;
string trusted_known_peer_id;
Bus bus;
private:
set<string> my_commands;
void join_bus();
void broadcast_my_commands(const string& target_id = "");
};
class SetCommandOwnership : public Message {
public:
SetCommandOwnership(const string& node_id, const vector<string>& command_list);
void act(shared_ptr<MessageFactory> node);
private:
string node_id;
vector<string> command_list;
};
}