#ifndef _COMMONS_SHAREDQUEUE_H
#define _COMMONS_SHAREDQUEUE_H
#include <mutex>
namespace commons {
class SharedQueue {
public:
SharedQueue(unsigned int initial_size = 1000);
~SharedQueue();
void enqueue(void* request);
void* dequeue();
bool empty();
unsigned int size();
protected:
unsigned int current_size();
unsigned int current_start();
unsigned int current_end();
unsigned int current_count();
private:
std::mutex shared_queue_mutex;
void** requests;
unsigned int allocated_size;
unsigned int count;
unsigned int start;
unsigned int end;
void enlarge_shared_queue();
};
}
#endif