#pragma once
#include <mutex>
#include "BusCommandProxy.h"
#include "Message.h"
#include "Properties.h"
using namespace std;
using namespace service_bus;
namespace agents {
class BaseProxy : public BusCommandProxy {
public:
static string ABORT;
static string FINISHED;
BaseProxy();
virtual ~BaseProxy();
virtual bool finished();
void abort();
virtual void tokenize(vector<string>& output);
virtual void untokenize(vector<string>& tokens);
bool is_aborting();
virtual string to_string();
virtual void raise_error(const string& error_message, unsigned int error_code = 0);
virtual bool from_remote_peer(const string& command, const vector<string>& args) override;
void abort(const vector<string>& args);
void command_finished(const vector<string>& args);
virtual void pack_command_line_args() = 0;
Properties parameters;
bool error_flag;
unsigned int error_code;
string error_message;
private:
mutex api_mutex;
bool abort_flag;
bool command_finished_flag;
};
}