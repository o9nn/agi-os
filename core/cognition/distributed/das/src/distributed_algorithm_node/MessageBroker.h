#ifndef _DISTRIBUTED_ALGORITHM_NODE_MESSAGEBROKER_H
#define _DISTRIBUTED_ALGORITHM_NODE_MESSAGEBROKER_H
#include <mutex>
#include <string>
#include <unordered_set>
#include <vector>
#include "Message.h"
#include "SharedQueue.h"
#include "Stoppable.h"
#include "StoppableThread.h"
#include "distributed_algorithm_node.grpc.pb.h"
using namespace std;
using namespace commons;
namespace distributed_algorithm_node {
enum class MessageBrokerType { RAM, GRPC };
class DistributedAlgorithmNode;
class MessageBroker : public Stoppable {
public:
static shared_ptr<MessageBroker> factory(MessageBrokerType instance_type,
shared_ptr<MessageFactory> host_node,
const string& node_id);
MessageBroker(shared_ptr<MessageFactory> host_node, const string& node_id);
virtual ~MessageBroker();
virtual void add_peer(const string& peer_id);
bool is_peer(const string& peer_id);
virtual void stop();
bool stopped();
virtual void join_network() = 0;
virtual void broadcast(const string& command, const vector<string>& args) = 0;
virtual void send(const string& command, const vector<string>& args, const string& recipient) = 0;
shared_ptr<MessageFactory> host_node;
unordered_set<string> peers;
mutex peers_mutex;
string node_id;
bool stop_flag;
mutex stop_flag_mutex;
bool joined_network;
};
class SynchronousSharedRAM : public MessageBroker {
public:
SynchronousSharedRAM(shared_ptr<MessageFactory> host_node, const string& node_id);
~SynchronousSharedRAM();
virtual void join_network();
virtual void broadcast(const string& command, const vector<string>& args);
virtual void send(const string& command, const vector<string>& args, const string& recipient);
void stop();
private:
static unsigned int MESSAGE_THREAD_COUNT;
static unordered_map<string, SharedQueue*> NODE_QUEUE;
static mutex NODE_QUEUE_MUTEX;
vector<shared_ptr<StoppableThread>> inbox_threads;
SharedQueue incoming_messages;
void inbox_thread_method(shared_ptr<StoppableThread> monitor);
};
class SynchronousGRPC : public MessageBroker, public dasproto::DistributedAlgorithmNode::Service {
public:
SynchronousGRPC(shared_ptr<MessageFactory> host_node, const string& node_id);
~SynchronousGRPC();
virtual void add_peer(const string& peer_id);
virtual void join_network();
virtual void broadcast(const string& command, const vector<string>& args);
virtual void send(const string& command, const vector<string>& args, const string& recipient);
void stop();
grpc::Status ping(grpc::ServerContext* grpc_context,
const dasproto::Empty* request,
dasproto::Ack* reply) override;
grpc::Status execute_message(grpc::ServerContext* grpc_context,
const dasproto::MessageData* request,
dasproto::Empty* reply) override;
private:
static unsigned int MESSAGE_THREAD_COUNT;
static mutex GRPC_BUILDER_MUTEX;
unique_ptr<grpc::Server> grpc_server;
shared_ptr<StoppableThread> grpc_thread;
vector<shared_ptr<StoppableThread>> inbox_threads;
SharedQueue incoming_messages;
SharedQueue outgoing_messages;
bool grpc_server_started_flag;
mutex grpc_server_started_flag_mutex;
bool inbox_setup_finished_flag;
mutex inbox_setup_finished_flag_mutex;
void set_grpc_server_started();
bool grpc_server_started();
void set_inbox_setup_finished();
bool inbox_setup_finished();
void grpc_thread_teardown(shared_ptr<StoppableThread> monitor);
void grpc_thread_method(shared_ptr<StoppableThread> monitor);
void inbox_thread_method(shared_ptr<StoppableThread> monitor);
};
class CommandLinePackage {
public:
CommandLinePackage(const string& command, const vector<string>& args);
~CommandLinePackage();
string command;
vector<string> args;
bool is_broadcast;
unordered_set<string> visited;
};
}
#endif