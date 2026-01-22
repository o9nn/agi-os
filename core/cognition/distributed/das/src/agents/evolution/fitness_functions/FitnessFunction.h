#pragma once
#include <memory>
#include "QueryAnswer.h"
using namespace std;
using namespace query_engine;
namespace fitness_functions {
class FitnessFunction {
public:
FitnessFunction(){};
virtual ~FitnessFunction(){};
virtual float eval(shared_ptr<QueryAnswer> query_answer) = 0;
};
}