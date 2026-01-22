#ifndef _DISTRIBUTED_ALGORITHM_NODE_LEADERSHIPBROKER_H
#define _DISTRIBUTED_ALGORITHM_NODE_LEADERSHIPBROKER_H
#include "MessageBroker.h"
using namespace std;
namespace distributed_algorithm_node {
enum class LeadershipBrokerType { SINGLE_MASTER_SERVER, TRUSTED_BUS_PEER };
class LeadershipBroker {
public:
static shared_ptr<LeadershipBroker> factory(LeadershipBrokerType instance_type);
LeadershipBroker();
virtual ~LeadershipBroker();
void set_message_broker(shared_ptr<MessageBroker> message_broker);
string leader_id();
void set_leader_id(const string& leader_id);
bool has_leader();
virtual void start_leader_election(const string& my_vote) = 0;
private:
shared_ptr<MessageBroker> message_broker;
string network_leader_id;
};
class SingleMasterServer : public LeadershipBroker {
public:
SingleMasterServer();
~SingleMasterServer();
void start_leader_election(const string& my_vote);
};
class TrustedBusPeer : public LeadershipBroker {
public:
TrustedBusPeer();
~TrustedBusPeer();
void start_leader_election(const string& my_vote);
};
}
#endif