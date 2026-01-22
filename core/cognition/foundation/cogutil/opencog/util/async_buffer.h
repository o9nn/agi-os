#ifndef _OC_ASYNC_BUFFER_H
#define _OC_ASYNC_BUFFER_H
#include <atomic>
#include <chrono>
#include <mutex>
#include <thread>
#include <vector>
#include <unistd.h>
#include <opencog/util/concurrent_set.h>
#include <opencog/util/exceptions.h>
#include <opencog/util/Logger.h>
#include <opencog/util/macros.h>
namespace opencog
{
template<typename Writer, typename Element>
class async_buffer
{
private:
concurrent_set<Element> _store_set;
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
bool _stall_writers;
void start_writer_thread();
void stop_writer_threads();
void write_loop();
void do_insert(const Element&);
void drain();
public:
async_buffer(Writer*, void (Writer::*)(const Element&), int nthreads=4);
~async_buffer();
void insert(const Element&);
void flush();
void barrier();
void set_watermarks(size_t, size_t);
void stall(bool);
void open(int nthreads=4);
void close();
bool _in_drain;
std::atomic<unsigned long> _item_count;
std::atomic<unsigned long> _duplicate_count;
std::atomic<unsigned long> _flush_count;
std::atomic<unsigned long> _drain_count;
std::atomic<unsigned long> _drain_msec;
std::atomic<unsigned long> _drain_slowest_msec;
std::atomic<unsigned long> _drain_concurrent;
unsigned long get_busy_writers() const { return _busy_writers; }
unsigned long get_size() const { return _store_set.size(); }
unsigned long get_pending() const { return _pending; }
unsigned long get_high_watermark() const { return _high_watermark; }
unsigned long get_low_watermark() const { return _low_watermark; }
bool stalling() const { return _stall_writers; }
void clear_stats();
};
#define DEFAULT_HIGH_WATER_MARK 100
#define DEFAULT_LOW_WATER_MARK 10
template<typename Writer, typename Element>
async_buffer<Writer, Element>::async_buffer(Writer* wr,
void (Writer::*cb)(const Element&),
int nthreads)
{
_writer = wr;
_do_write = cb;
_stopping_writers = false;
_thread_count = 0;
_busy_writers = 0;
_pending = 0;
_stall_writers = false;
_in_drain = false;
_high_watermark = DEFAULT_HIGH_WATER_MARK;
_low_watermark = DEFAULT_LOW_WATER_MARK;
clear_stats();
for (int i=0; i<nthreads; i++)
start_writer_thread();
}
template<typename Writer, typename Element>
void async_buffer<Writer, Element>::open(int nthreads)
{
if (0 < _thread_count) return;
for (int i=0; i<nthreads; i++)
start_writer_thread();
}
template<typename Writer, typename Element>
async_buffer<Writer, Element>::~async_buffer()
{
stop_writer_threads();
}
template<typename Writer, typename Element>
void async_buffer<Writer, Element>::close()
{
stop_writer_threads();
}
template<typename Writer, typename Element>
void async_buffer<Writer, Element>::set_watermarks(size_t hi, size_t lo)
{
_high_watermark = hi;
_low_watermark = lo;
}
template<typename Writer, typename Element>
void async_buffer<Writer, Element>::stall(bool st)
{
_stall_writers = st;
}
template<typename Writer, typename Element>
void async_buffer<Writer, Element>::clear_stats()
{
_item_count = 0;
_duplicate_count = 0;
_flush_count = 0;
_drain_count = 0;
_drain_msec = 0;
_drain_slowest_msec = 0;
_drain_concurrent = 0;
}
template<typename Writer, typename Element>
void async_buffer<Writer, Element>::start_writer_thread()
{
std::unique_lock<std::mutex> lock(_write_mutex);
if (_stopping_writers)
throw RuntimeException(TRACE_INFO,
"Cannot start; async_buffer writer threads are being stopped!");
_write_threads.push_back(std::thread(&async_buffer::write_loop, this));
_thread_count ++;
}
template<typename Writer, typename Element>
void async_buffer<Writer, Element>::stop_writer_threads()
{
_stall_writers = false;
std::unique_lock<std::mutex> lock(_write_mutex);
if (0 == _thread_count) return;
_stopping_writers = true;
while (0 < _pending)
{
usleep(1000);
}
_store_set.cancel();
while (0 < _write_threads.size())
{
_write_threads.back().join();
_write_threads.pop_back();
_thread_count --;
}
_store_set.cancel_reset();
while (not _store_set.is_empty())
{
Element elt = _store_set.value_get();
(_writer->*_do_write)(elt);
}
_stopping_writers = false;
}
template<typename Writer, typename Element>
void async_buffer<Writer, Element>::drain()
{
bool save_stall = _stall_writers;
_stall_writers = false;
_flush_count++;
while (0 < _pending)
{
std::this_thread::sleep_for(std::chrono::milliseconds(1));
}
_stall_writers = save_stall;
}
template<typename Writer, typename Element>
void async_buffer<Writer, Element>::flush()
{
bool save_stall = _stall_writers;
_stall_writers = false;
_flush_count++;
while (0 < _store_set.size())
{
std::this_thread::sleep_for(std::chrono::milliseconds(1));
}
_stall_writers = save_stall;
}
template<typename Writer, typename Element>
void async_buffer<Writer, Element>::barrier()
{
std::unique_lock<std::mutex> lock(_enqueue_mutex);
std::thread::id tid = std::this_thread::get_id();
for (const auto& th : _write_threads)
{
if (th.get_id() == tid)
{
flush();
return;
}
}
drain();
}
template<typename Writer, typename Element>
void async_buffer<Writer, Element>::write_loop()
{
try
{
while (true)
{
while (_stall_writers and _store_set.size() < _low_watermark)
{
std::this_thread::sleep_for(std::chrono::milliseconds(3));
}
Element elt = _store_set.value_get();
_busy_writers ++;
(_writer->*_do_write)(elt);
_busy_writers --;
_pending --;
}
}
catch (typename concurrent_set<Element>::Canceled& e)
{
return;
}
}
template<typename Writer, typename Element>
void async_buffer<Writer, Element>::do_insert(const Element& elt)
{
_pending ++;
bool inserted = _store_set.insert(elt);
_item_count++;
if (not inserted)
{
_duplicate_count++;
_pending --;
}
}
template<typename Writer, typename Element>
void async_buffer<Writer, Element>::insert(const Element& elt)
{
if (_stopping_writers)
throw RuntimeException(TRACE_INFO,
"Cannot store; async_buffer writer threads are being stopped!");
if (0 == _thread_count)
{
_item_count++;
(_writer->*_do_write)(elt);
return;
}
std::thread::id tid = std::this_thread::get_id();
for (const auto& th : _write_threads)
{
if (th.get_id() == tid)
{
do_insert(elt);
return;
}
}
{
std::unique_lock<std::mutex> lock(_enqueue_mutex);
do_insert(elt);
}
if (_high_watermark < _store_set.size())
{
if (_in_drain) _drain_concurrent ++;
else _drain_count++;
_in_drain = true;
auto start = std::chrono::steady_clock::now();
do
{
std::this_thread::sleep_for(std::chrono::milliseconds(1));
}
while (_low_watermark < _store_set.size());
_in_drain = false;
auto end = std::chrono::steady_clock::now();
auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
unsigned long msec = duration.count();
logger().debug("async_buffer overfull set; had to sleep %d millisecs to drain!", msec);
_drain_msec += msec;
if (_drain_slowest_msec < msec) _drain_slowest_msec = msec;
}
}
}
#endif