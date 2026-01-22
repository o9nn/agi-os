#ifndef _OPENCOG_COG_CHANNEL_H
#define _OPENCOG_COG_CHANNEL_H
#include <atomic>
#include <string>
#include <unistd.h>
#include <opencog/util/async_buffer.h>
namespace opencog
{
template<typename Client, typename Data>
class CogChannel
{
private:
std::string _uri;
std::string _host;
std::string _port;
void* _servinfo;
static std::atomic_int _nsocks;
static thread_local struct tlso {
int _sockfd;
tlso() { _sockfd = 0; }
~tlso() { if (_sockfd) { close(_sockfd); _nsocks--; }}
} s;
int open_sock();
void do_send(const std::string&);
std::string do_recv(bool=false);
struct Msg
{
std::string str_to_send;
Data data;
Client* client;
void (Client::*callback)(const std::string&, const Data&);
int operator<(const Msg& other) const
{ return this < &other; }
};
async_buffer<CogChannel, Msg> _msg_buffer;
void reply_handler(const Msg&);
public:
CogChannel(void);
CogChannel(const CogChannel&) = delete;
CogChannel& operator=(const CogChannel&) = delete;
~CogChannel();
void open_connection(const std::string& uri);
void close_connection(void);
bool connected(void);
void enqueue(Client*, const std::string&, Data&,
void (Client::*)(const std::string&, const Data&));
void synchro(Client*, const std::string&, Data&,
void (Client::*)(const std::string&, Data&));
void barrier();
void flush();
void clear_stats();
std::string print_stats();
};
}
#endif