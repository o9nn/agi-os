#pragma once
#include "HebbianNetwork.h"
#include "attention_broker.grpc.pb.h"
using namespace std;
namespace attention_broker {
enum class HebbianNetworkUpdaterType {
EXACT_COUNT
};
class HebbianNetworkUpdater {
public:
static HebbianNetworkUpdater* factory(HebbianNetworkUpdaterType instance_type);
virtual ~HebbianNetworkUpdater();
virtual void correlation(const dasproto::HandleList* request) = 0;
virtual void asymmetric_correlation(const dasproto::HandleList* request) = 0;
void determiners(const dasproto::HandleList& sub_request, HebbianNetwork* network);
protected:
HebbianNetworkUpdater();
private:
};
class ExactCountHebbianUpdater : public HebbianNetworkUpdater {
public:
ExactCountHebbianUpdater();
~ExactCountHebbianUpdater();
void correlation(const dasproto::HandleList* request);
void asymmetric_correlation(
const dasproto::HandleList* request);
};
}