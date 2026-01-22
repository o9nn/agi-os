#ifndef _OPENCOG_SERVER_SOCKET_H
#define _OPENCOG_SERVER_SOCKET_H
#include <atomic>
#include <pthread.h>
#include <asio.hpp>
namespace opencog
{
class ServerSocket
{
private:
asio::ip::tcp::socket* _socket;
static bool _network_gone;
static unsigned int _max_open_sockets;
static volatile unsigned int _num_open_sockets;
static std::mutex _max_mtx;
static std::condition_variable _max_cv;
static size_t _num_open_stalls;
std::string get_telnet_line(asio::streambuf&);
std::string get_http_body(asio::streambuf&);
void Send(const asio::const_buffer&);
bool _got_first_line;
bool _got_http_header;
bool _do_frame_io;
std::string _webkey;
void HandshakeLine(const std::string&);
std::string get_websocket_data(void);
std::string get_websocket_line(void);
void send_websocket_pong(void);
void send_websocket(const std::string&);
protected:
bool _is_http_socket;
bool _got_websock_header;
std::string _url;
bool _keep_alive;
size_t _content_length;
bool _is_mcp_socket;
virtual void OnConnection(void) = 0;
virtual void OnLine (const std::string&) = 0;
time_t _start_time;
pid_t _tid;
pthread_t _pth;
const char* _status;
time_t _last_activity;
size_t _line_count;
virtual std::string connection_header(void);
virtual std::string connection_stats(void);
public:
ServerSocket(void);
virtual ~ServerSocket();
void act_as_http_socket(void) { _is_http_socket = true; }
void act_as_mcp(void) { _is_mcp_socket = true; }
void set_connection(asio::ip::tcp::socket*);
void handle_connection(void);
void Send(const std::string&);
void Exit(void);
static void network_gone(void) { _network_gone = true; }
static std::string display_stats(int);
static void half_ping(void);
static bool kill(pid_t);
static std::atomic_size_t total_line_count;
static void set_max_open_sockets(unsigned int m) { _max_open_sockets = m; }
static unsigned int get_max_open_sockets() { return _max_open_sockets; }
static unsigned int get_num_open_sockets() { return _num_open_sockets; }
static size_t get_num_open_stalls() { return _num_open_stalls; }
};
}
#endif