#pragma once
#define DEBUG
#include <string>
#include <unordered_map>
#include "HebbianNetwork.h"
#include "HebbianNetworkUpdater.h"
#include "SharedQueue.h"
#include "StimulusSpreader.h"
#include "WorkerThreads.h"
#include "attention_broker.grpc.pb.h"
using dasproto::AttentionBroker;
using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;
using grpc::Status;
namespace attention_broker {
class AttentionBrokerServer final : public AttentionBroker::Service {
public:
AttentionBrokerServer();
~AttentionBrokerServer();
static const unsigned int WORKER_THREADS_COUNT = 10;
string global_context;
static double RENT_RATE;
static double SPREADING_RATE_LOWERBOUND;
static double SPREADING_RATE_UPPERBOUND;
Status ping(ServerContext* grpc_context,
const dasproto::Empty* request,
dasproto::Ack* reply) override;
Status stimulate(ServerContext* grpc_context,
const dasproto::HandleCount* request,
dasproto::Ack* reply) override;
Status correlate(ServerContext* grpc_context,
const dasproto::HandleList* request,
dasproto::Ack* reply) override;
Status asymmetric_correlate(ServerContext* grpc_context,
const dasproto::HandleList* request,
dasproto::Ack* reply) override;
Status get_importance(ServerContext* grpc_context,
const dasproto::HandleList* request,
dasproto::ImportanceList* reply) override;
Status set_determiners(ServerContext* grpc_context,
const dasproto::HandleListList* request,
dasproto::Ack* reply) override;
Status set_parameters(ServerContext* grpc_context,
const dasproto::Parameters* request,
dasproto::Ack* reply) override;
Status save_context(ServerContext* grpc_context,
const dasproto::ContextPersistence* request,
dasproto::Ack* reply) override;
Status drop_and_load_context(ServerContext* grpc_context,
const dasproto::ContextPersistence* request,
dasproto::Ack* reply) override;
void graceful_shutdown();
private:
bool rpc_api_enabled = true;
SharedQueue* stimulus_requests;
SharedQueue* correlation_requests;
WorkerThreads* worker_threads;
unordered_map<string, HebbianNetwork*> hebbian_network;
HebbianNetworkUpdater* updater;
StimulusSpreader* stimulus_spreader;
HebbianNetwork* select_hebbian_network(const string& context);
};
}