#ifndef _DISTRIBUTED_ALGORITHM_NODE_MESSAGE_H
#define _DISTRIBUTED_ALGORITHM_NODE_MESSAGE_H
#include <memory>
#include <string>
#include <vector>
using namespace std;
namespace distributed_algorithm_node {
class DistributedAlgorithmNode;
class Message;
class MessageFactory {
public:
virtual shared_ptr<Message> message_factory(string& command, vector<string>& args) = 0;
};
class Message {
public:
virtual void act(shared_ptr<MessageFactory> node) = 0;
Message();
~Message();
private:
};
class DoNothing : public Message {
public:
DoNothing() {}
~DoNothing() {}
void act(shared_ptr<MessageFactory> node) {}
};
class NodeJoinedNetwork : public Message {
private:
string joining_node;
public:
NodeJoinedNetwork(string& node_id);
~NodeJoinedNetwork();
void act(shared_ptr<MessageFactory> node);
};
}
#endif