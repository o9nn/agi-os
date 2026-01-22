#pragma once
#include <memory>
#include <string>
#include "QueryNode.h"
#include "Utils.h"
#define DEBUG
using namespace std;
using namespace query_node;
using namespace commons;
namespace query_element {
class QueryElement {
public:
string id;
string subsequent_id;
unsigned int arity;
QueryElement();
virtual ~QueryElement();
virtual void setup_buffers() = 0;
virtual void graceful_shutdown() = 0;
bool is_terminal;
bool is_operator;
virtual string to_string();
protected:
bool is_flow_finished();
void set_flow_finished();
private:
bool flow_finished;
mutex flow_finished_mutex;
};
}