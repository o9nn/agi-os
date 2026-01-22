#pragma once
#include <chrono>
#include <iostream>
#include <mutex>
#include <thread>
#include <vector>
#include "SharedQueue.h"
using namespace std;
using namespace commons;
namespace attention_broker {
class WorkerThreads {
public:
WorkerThreads(SharedQueue* stimulus, SharedQueue* correlation);
~WorkerThreads();
void graceful_stop();
private:
unsigned int threads_count;
vector<thread*> threads;
bool stop_flag = false;
SharedQueue* stimulus_requests;
SharedQueue* correlation_requests;
mutex stop_flag_mutex;
void worker_thread(unsigned int thread_id,
SharedQueue* stimulus_requests,
SharedQueue* correlation_requests);
};
}