#pragma once
#include <map>
#include <memory>
#include <mutex>
#include "FitnessFunction.h"
using namespace std;
namespace fitness_functions {
class FitnessFunctionRegistry {
public:
static string REMOTE_FUNCTION;
~FitnessFunctionRegistry() {}
static shared_ptr<FitnessFunction> function(const string& tag);
static void initialize_statics();
private:
FitnessFunctionRegistry() {}
static bool INITIALIZED;
static map<string, shared_ptr<FitnessFunction>> FUNCTION;
};
}