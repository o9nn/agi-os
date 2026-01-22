#pragma once
#include "HebbianNetwork.h"
#include "SharedQueue.h"
namespace attention_broker {
using namespace std;
using namespace commons;
enum class SelectorType { EVEN_THREAD_COUNT };
enum class RequestType { STIMULUS, CORRELATION };
class RequestSelector {
public:
virtual ~RequestSelector();
static RequestSelector* factory(SelectorType instance_type,
unsigned int thread_id,
SharedQueue* stimulus,
SharedQueue* correlation);
virtual pair<RequestType, void*> next() = 0;
protected:
RequestSelector(unsigned int thread_id,
SharedQueue* stimulus,
SharedQueue* correlation);
unsigned int thread_id;
SharedQueue* stimulus;
SharedQueue* correlation;
};
class EvenThreadCount : public RequestSelector {
public:
~EvenThreadCount();
EvenThreadCount(unsigned int thread_id,
SharedQueue* stimulus,
SharedQueue* correlation);
pair<RequestType, void*> next();
private:
bool even_thread_id;
};
}