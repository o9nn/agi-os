#ifndef _DISTRIBUTED_ALGORITHM_NODE_DISTRIBUTEDALGORITHMNODE_H
#define _DISTRIBUTED_ALGORITHM_NODE_DISTRIBUTEDALGORITHMNODE_H
#include <string>
#include <vector>
#include "LeadershipBroker.h"
#include "Message.h"
#include "MessageBroker.h"
using namespace std;
namespace distributed_algorithm_node {
class DistributedAlgorithmNode : public MessageFactory {
public:
virtual ~DistributedAlgorithmNode();
DistributedAlgorithmNode(const string& node_id,
LeadershipBrokerType leadership_algorithm,
MessageBrokerType messaging_backend);
void join_network();
bool is_leader();
string leader_id();
bool has_leader();
void add_peer(const string& peer_id);
string node_id();
void broadcast(const string& command, const vector<string>& args);
void send(const string& command, const vector<string>& args, const string& recipient);
virtual shared_ptr<Message> message_factory(string& command, vector<string>& args);
virtual void graceful_shutdown();
virtual void node_joined_network(const string& node_id) = 0;
virtual string cast_leadership_vote() = 0;
virtual string to_string();
private:
string my_node_id;
shared_ptr<LeadershipBroker> leadership_broker;
shared_ptr<MessageBroker> message_broker;
struct {
string NODE_JOINED_NETWORK = "node_joined_network";
} known_commands;
};
}
#endif