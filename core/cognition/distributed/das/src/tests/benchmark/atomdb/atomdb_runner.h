#pragma once
#include <algorithm>
#include <chrono>
#include <map>
#include <memory>
#include <mutex>
#include <numeric>
#include <random>
#include <string>
#include <vector>
#include "AtomDB.h"
#include "AtomDBAPITypes.h"
#include "MorkDB.h"
#include "benchmark_runner.h"
#include "benchmark_utils.h"
using namespace std;
using namespace atomdb;
class AtomDBRunner : public Runner {
public:
AtomDBRunner(int tid, shared_ptr<AtomDB> db, int iterations);
vector<string> get_random_link_handle(shared_ptr<atomdb_api_types::HandleSet> handle_set,
size_t max_count = Runner::MAX_COUNT,
size_t n = 1);
protected:
shared_ptr<AtomDB> db_;
};