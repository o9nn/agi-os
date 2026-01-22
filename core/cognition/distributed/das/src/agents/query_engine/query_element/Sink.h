#pragma once
#include "QueryElement.h"
using namespace std;
namespace query_element {
class Sink : public QueryElement {
public:
Sink(shared_ptr<QueryElement> precedent, const string& id, bool setup_buffers_flag = true);
virtual ~Sink();
virtual void graceful_shutdown();
virtual void setup_buffers();
bool finished();
shared_ptr<QueryNode> input_buffer;
protected:
shared_ptr<QueryElement> precedent;
unsigned int query_answer_count;
};
}