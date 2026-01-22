#pragma once
#include "QueryAnswer.h"
#include "Sink.h"
using namespace std;
using namespace query_engine;
namespace query_element {
class Iterator : public Sink {
public:
Iterator(shared_ptr<QueryElement> precedent);
~Iterator();
bool finished();
QueryAnswer* pop();
};
}