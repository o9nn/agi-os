#pragma once
#include "BaseQueryProxy.h"
using namespace std;
using namespace agents;
namespace inference_agent {
class InferenceProxy : public BaseQueryProxy {
public:
static string INFERENCE_REQUEST_TIMEOUT;
static string REPEAT_COUNT;
InferenceProxy();
InferenceProxy(const vector<string>& tokens);
virtual ~InferenceProxy();
void pack_command_line_args();
void set_default_parameters();
void set_parameter(const string& key, const PropertyValue& value);
private:
mutex api_mutex;
};
}