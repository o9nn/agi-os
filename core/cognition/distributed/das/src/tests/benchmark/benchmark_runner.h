#pragma once
#include <algorithm>
#include <chrono>
#include <iostream>
#include <map>
#include <memory>
#include <mutex>
#include <numeric>
#include <random>
#include <string>
#include <vector>
#include "benchmark_utils.h"
using namespace std;
extern mutex global_mutex;
extern map<string, Metrics> global_metrics;
class Runner {
public:
Runner(int tid, int iterations);
virtual ~Runner() {}
protected:
int tid_;
int iterations_;
static const size_t MAX_COUNT;
static const vector<string> contains_links_query;
static const vector<string> sentence_links_query;
template <typename Fsetup, typename Fbench>
void run_benchmark(const string& key, Fsetup&& setup, Fbench&& bench, int divisor = 1) {
vector<double> operation_time;
for (int i = 0; i < iterations_; ++i) {
auto ret = setup(i);
double duration = measure_execution_time([&] { bench(ret); });
operation_time.push_back(duration);
}
double total_time = accumulate(operation_time.begin(), operation_time.end(), 0.0);
int total_ops = iterations_ * divisor;
double avg_time = total_time / total_ops;
double ops_per_sec = total_ops / (total_time / 1000.0);
global_mutex.lock();
global_metrics[key] = Metrics{operation_time, total_time, avg_time, ops_per_sec};
global_mutex.unlock();
}
};