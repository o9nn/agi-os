#ifndef _OC_ASYNC_WRITER_H
#define _OC_ASYNC_WRITER_H
#include <atomic>
#include <chrono>
#include <mutex>
#include <thread>
#include <vector>
#include <opencog/util/concurrent_queue.h>
#include <opencog/util/concurrent_stack.h>
#include <opencog/util/exceptions.h>
#include <opencog/util/Logger.h>
#include <opencog/util/macros.h>
namespace opencog
{
template<typename Writer, typename Element>
class async_caller
{
private:
concurrent_queue<Element> _store_queue;
std::vector<std::thread> _write_threads;
std::mutex _write_mutex;
std::mutex _enqueue_mutex;
std::atomic<unsigned long> _busy_writers;
std::atomic<unsigned long> _pending;
size_t _high_watermark;
size_t _low_watermark;
Writer* _writer;
void (Writer::*_do_write)(const Element&);
unsigned int _thread_count;
bool _stopping_writers;
void start_writer_thread();
void stop_writer_threads();
void write_loop();
void drain();
public:
async_caller(Writer*, void (Writer::*)(const Element&), int nthreads=4);
~async_caller();
void enqueue(const Element&);
void enqueue(Element&&);
void flush_queue();
void barrier();
void set_watermarks(size_t, size_t);
bool _in_drain;
std::atomic<unsigned long> _item_count;
std::atomic<unsigned long> _flush_count;
std::atomic<unsigned long> _drain_count;
std::atomic<unsigned long> _drain_msec;
std::atomic<unsigned long> _drain_slowest_msec;
std::atomic<unsigned long> _drain_concurrent;
unsigned long get_busy_writers() const { return _busy_writers; }
unsigned long get_queue_size() const { return _pending; }
unsigned long get_high_watermark() const { return _high_watermark; }
unsigned long get_low_watermark() const { return _low_watermark; }
void clear_stats();
};
#define DEFAULT_HIGH_WATER_MARK 100
#define DEFAULT_LOW_WATER_MARK 10
template<typename Writer, typename Element>
async_caller<Writer, Element>::async_caller(Writer* wr,
void (Writer::*cb)(const Element&),
int nthreads)
{
_writer = wr;
_do_write = cb;
_stopping_writers = false;
_thread_count = 0;
_busy_writers = 0;
_pending = 0;
_in_drain = false;
_high_watermark = DEFAULT_HIGH_WATER_MARK;
_low_watermark = DEFAULT_LOW_WATER_MARK;
clear_stats();
for (int i=0; i<nthreads; i++)
start_writer_thread();
}
template<typename Writer, typename Element>
async_caller<Writer, Element>::~async_caller()
{
stop_writer_threads();
}
template<typename Writer, typename Element>
void async_caller<Writer, Element>::set_watermarks(size_t hi, size_t lo)
{
_high_watermark = hi;
_low_watermark = lo;
}
template<typename Writer, typename Element>
void async_caller<Writer, Element>::clear_stats()
{
_item_count = 0;
_flush_count = 0;
_drain_count = 0;
_drain_msec = 0;
_drain_slowest_msec = 0;
_drain_concurrent = 0;
}
template<typename Writer, typename Element>
void async_caller<Writer, Element>::start_writer_thread()
{
std::unique_lock<std::mutex> lock(_write_mutex);
if (_stopping_writers)
throw RuntimeException(TRACE_INFO,
"Cannot start; async_caller writer threads are being stopped!");
_write_threads.push_back(std::thread(&async_caller::write_loop, this));
_thread_count ++;
}
template<typename Writer, typename Element>
void async_caller<Writer, Element>::stop_writer_threads()
{
std::unique_lock<std::mutex> lock(_write_mutex);
if (0 == _thread_count) return;
_stopping_writers = true;
while (0 < _pending)
{
std::this_thread::sleep_for(std::chrono::milliseconds(1));
}
_store_queue.cancel();
while (0 < _write_threads.size())
{
_write_threads.back().join();
_write_threads.pop_back();
_thread_count --;
}
_store_queue.cancel_reset();
while (not _store_queue.is_empty())
{
Element elt = _store_queue.value_pop();
(_writer->*_do_write)(elt);
}
_stopping_writers = false;
}
template<typename Writer, typename Element>
void async_caller<Writer, Element>::drain()
{
_flush_count++;
while (0 < _pending)
{
std::this_thread::sleep_for(std::chrono::milliseconds(1));
}
}
template<typename Writer, typename Element>
void async_caller<Writer, Element>::flush_queue()
{
_flush_count++;
while (0 < _store_queue.size())
{
std::this_thread::sleep_for(std::chrono::milliseconds(1));
}
}
template<typename Writer, typename Element>
void async_caller<Writer, Element>::barrier()
{
std::unique_lock<std::mutex> lock(_enqueue_mutex);
std::thread::id tid = std::this_thread::get_id();
for (const auto& th : _write_threads)
{
if (th.get_id() == tid)
{
flush_queue();
return;
}
}
drain();
}
template<typename Writer, typename Element>
void async_caller<Writer, Element>::write_loop()
{
try
{
while (true)
{
Element elt = _store_queue.value_pop();
_busy_writers ++;
(_writer->*_do_write)(elt);
_busy_writers --;
_pending --;
}
}
catch (typename concurrent_queue<Element>::Canceled& e)
{
return;
}
}
template<typename Writer, typename Element>
void async_caller<Writer, Element>::enqueue(Element&& elt)
{
if (_stopping_writers)
throw RuntimeException(TRACE_INFO,
"Cannot store; async_caller writer threads are being stopped!");
if (0 == _thread_count)
{
_item_count++;
(_writer->*_do_write)(std::move(elt));
return;
}
std::thread::id tid = std::this_thread::get_id();
for (const auto& th : _write_threads)
{
if (th.get_id() == tid)
{
_pending ++;
_store_queue.push(std::move(elt));
_item_count++;
return;
}
}
{
std::unique_lock<std::mutex> lock(_enqueue_mutex);
_pending ++;
_store_queue.push(std::move(elt));
_item_count++;
}
if (_high_watermark < _store_queue.size())
{
if (_in_drain) _drain_concurrent ++;
else _drain_count++;
_in_drain = true;
auto start = std::chrono::steady_clock::now();
do
{
std::this_thread::sleep_for(std::chrono::milliseconds(1));
}
while (_low_watermark < _store_queue.size());
_in_drain = false;
auto end = std::chrono::steady_clock::now();
auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
unsigned long msec = duration.count();
logger().debug("async_caller overfull queue; had to sleep %d millisecs to drain!", msec);
_drain_msec += msec;
if (_drain_slowest_msec < msec) _drain_slowest_msec = msec;
}
}
template<typename Writer, typename Element>
void async_caller<Writer, Element>::enqueue(const Element& elt)
{
enqueue(std::move(Element(elt)));
}
}
#endif