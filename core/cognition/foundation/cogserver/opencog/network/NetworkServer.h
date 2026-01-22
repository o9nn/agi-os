#ifndef _OPENCOG_SIMPLE_NETWORK_SERVER_H
#define _OPENCOG_SIMPLE_NETWORK_SERVER_H
#include <atomic>
#include <queue>
#include <string>
#include <thread>
#include <asio.hpp>
#include <opencog/network/ServerSocket.h>
namespace opencog
{
class NetworkServer
{
protected:
std::string _name;
short _port;
std::atomic_bool _running;
asio::io_service _io_service;
asio::ip::tcp::acceptor _acceptor;
std::thread* _listener_thread;
void listen();
ServerSocket* (*_getServer)(void);
time_t _start_time;
time_t _last_connect;
size_t _nconnections;
public:
NetworkServer(unsigned short port, const char* name);
~NetworkServer();
void run(ServerSocket* (*)(void));
void stop();
std::string display_stats(int nlines = -1);
};
}
#endif