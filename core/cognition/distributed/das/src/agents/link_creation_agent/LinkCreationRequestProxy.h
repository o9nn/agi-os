#pragma once
#include "BaseProxy.h"
#include "Message.h"
using namespace std;
using namespace agents;
using namespace distributed_algorithm_node;
namespace link_creation_agent {
class LinkCreationRequestProxy : public BaseProxy {
public:
static string MAX_ANSWERS;
static string REPEAT_COUNT;
static string CONTEXT;
static string ATTENTION_UPDATE_FLAG;
static string POSITIVE_IMPORTANCE_FLAG;
static string QUERY_INTERVAL;
static string QUERY_TIMEOUT;
static string USE_METTA_AS_QUERY_TOKENS;
LinkCreationRequestProxy();
LinkCreationRequestProxy(const vector<string>& tokens);
virtual ~LinkCreationRequestProxy();
virtual void pack_command_line_args();
void set_default_parameters();
void set_parameter(const string& key, const PropertyValue& value);
private:
mutex api_mutex;
};
}