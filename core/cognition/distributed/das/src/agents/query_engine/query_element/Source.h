#pragma once
#include "QueryAnswer.h"
#include "QueryElement.h"
using namespace std;
namespace query_element {
class Source : public QueryElement {
public:
Source();
virtual ~Source();
virtual void graceful_shutdown();
virtual void setup_buffers();
protected:
shared_ptr<QueryNode> output_buffer;
shared_ptr<QueryElement> subsequent;
};
}