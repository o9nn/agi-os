#ifndef _OPENCOG_GENERIC_SHELL_H
#define _OPENCOG_GENERIC_SHELL_H
#include <condition_variable>
#include <mutex>
#include <string>
#include <thread>
#include <opencog/util/concurrent_queue.h>
namespace opencog {
class ConsoleSocket;
class GenericEval;
class GenericShell
{
private:
std::mutex _pending_mtx;
std::string _pending_output;
ConsoleSocket* socket;
std::thread* evalthr;
std::thread* pollthr;
concurrent_queue<std::string> evalque;
volatile bool _init_done;
protected:
std::string abort_prompt;
std::string normal_prompt;
std::string pending_prompt;
bool show_output;
bool show_prompt;
volatile bool self_destruct;
bool apply_discipline;
virtual GenericEval* get_evaluator(void) = 0;
virtual void thread_init(void);
virtual void line_discipline(const std::string &expr);
std::condition_variable _poll_cv;
std::mutex _poll_mtx;
void wake_poll();
void eval_loop();
void poll_loop();
void poll_and_send();
std::condition_variable _eval_cv;
std::mutex _eval_mtx;
bool _eval_done;
GenericEval* _evaluator;
void start_eval();
void finish_eval();
void while_not_done();
virtual void user_interrupt();
virtual void put_output(const std::string&);
virtual std::string get_output();
virtual std::string poll_output();
public:
GenericShell(void);
virtual ~GenericShell();
virtual void set_socket(ConsoleSocket *);
virtual void eval(const std::string &);
virtual const std::string& get_prompt(void);
virtual void hush_output(bool);
virtual void hush_prompt(bool);
virtual void discipline(bool);
const char* _name;
bool eval_done() const { return _eval_done; }
size_t pending() const { return _pending_output.size(); }
size_t queued() const { return evalque.size(); }
};
}
#endif