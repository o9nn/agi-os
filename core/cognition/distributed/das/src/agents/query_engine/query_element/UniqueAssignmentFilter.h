#pragma once
#include "Operator.h"
using namespace std;
namespace query_element {
class UniqueAssignmentFilter : public Operator<1> {
public:
UniqueAssignmentFilter(const shared_ptr<QueryElement>& input);
virtual ~UniqueAssignmentFilter();
virtual void setup_buffers();
virtual void graceful_shutdown();
private:
thread* operator_thread;
void initialize(const shared_ptr<QueryElement>& input);
void thread_filter();
};
}