#pragma once
#include <mutex>
#include "BaseQueryProxy.h"
#include "Message.h"
#include "QueryAnswer.h"
#include "SharedQueue.h"
using namespace std;
using namespace service_bus;
using namespace distributed_algorithm_node;
namespace query_engine {
class PatternMatchingQueryProxy : public BaseQueryProxy {
public:
static string COUNT;
static string POSITIVE_IMPORTANCE_FLAG;
static string UNIQUE_VALUE_FLAG;
static string COUNT_FLAG;
PatternMatchingQueryProxy();
PatternMatchingQueryProxy(const vector<string>& tokens, const string& context);
virtual ~PatternMatchingQueryProxy();
virtual shared_ptr<QueryAnswer> pop();
virtual void pack_command_line_args();
bool from_remote_peer(const string& command, const vector<string>& args) override;
void count_answer(const vector<string>& args);
private:
void init();
void set_default_parameters();
mutex api_mutex;
};
}