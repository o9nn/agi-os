#ifndef _DISTRIBUTED_ALGORITHM_NODE_STARNODE_H
#define _DISTRIBUTED_ALGORITHM_NODE_STARNODE_H
#include <string>
#include "DistributedAlgorithmNode.h"
using namespace std;
namespace distributed_algorithm_node {
class StarNode : public DistributedAlgorithmNode {
public:
StarNode(const string& node_id, MessageBrokerType messaging_backend = MessageBrokerType::GRPC);
StarNode(const string& node_id,
const string& server_id,
MessageBrokerType messaging_backend = MessageBrokerType::GRPC);
virtual ~StarNode();
virtual void node_joined_network(const string& node_id);
string cast_leadership_vote();
protected:
bool is_server;
string server_id;
};
}
#endif