#ifndef _OPENCOG_CONSOLE_SOCKET_H
#define _OPENCOG_CONSOLE_SOCKET_H
#include <condition_variable>
#include <mutex>
#include <string>
#include <opencog/network/ServerSocket.h>
#include <opencog/network/GenericShell.h>
namespace opencog
{
class ConsoleSocket : public ServerSocket
{
private:
volatile unsigned int _use_count;
std::mutex _in_use_mtx;
std::condition_variable _in_use_cv;
protected:
GenericShell* _shell;
virtual void OnConnection(void) = 0;
virtual void OnLine(const std::string&) = 0;
virtual std::string connection_header(void);
virtual std::string connection_stats(void);
public:
ConsoleSocket(void);
virtual ~ConsoleSocket();
void get() { std::unique_lock<std::mutex> lck(_in_use_mtx); _use_count++; }
void put() { std::unique_lock<std::mutex> lck(_in_use_mtx); _use_count--; _in_use_cv.notify_all(); }
void SetShell(GenericShell *);
GenericShell * getShell(void) { return _shell; }
unsigned int get_use_count() const { return _use_count; }
};
}
#endif